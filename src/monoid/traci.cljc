(ns monoid.traci
  (:require [monoid.traci.api :as api]
            [monoid.traci.in-memory :as in-memory]
            [clojure.pprint :as pprint]
            )
  (:import  [java.time Instant]))

(def ^{:dynamic true} *trace* {:depth 0})

(defn traceable? [x]
  (or (fn? x)
      (instance? clojure.lang.MultiFn x)))

(defn fns
  "List all fns and MultiFn in a namespace"
  [namespace]
  (->> (the-ns namespace) ns-interns vals
       (filter (comp traceable? var-get))))

(defn arglist-regular-fn [arglist args]
  (zipmap arglist args))

(defn arglist-for-varargs [arglist args]
  (let [remove-&    (- (count arglist ) 2)
        ;; remove & from arglist
        vararg-list (into (subvec arglist 0 remove-&) (subvec arglist (inc remove-&)))
        ;; collect varargs into own vector
        args-vector (vec args)
        varargs     (into  (subvec args-vector 0 remove-&) (vector (subvec args-vector remove-&)))
        ]
    (zipmap vararg-list varargs)))

(defn arglist-for-multi [args]
  {'_ args}
  )

(defn arglist-for-args
  "Create a map of argument names with their actual value. Coalesce varargs.
  i.e. [x y] (1 2) -> {x 1 y 2} and
  [x & xs] (1 2 3) -> {x 1 xs [2 3]}
  "
  [meta-of-fn args]
  (let [no-of-args (count args)
        arglist (first (filter (fn [arglist]
                                 (or (= no-of-args (count arglist)) (.contains arglist '&)))
                               (:arglists meta-of-fn)))
        ]
    (cond
      (nil? arglist)         (arglist-for-multi args)
      (.contains arglist '&) (arglist-for-varargs arglist args)
      true                   (arglist-regular-fn arglist args))))

(defn default-trace-producer [trace-id name metadata parent-id parent-name f args result exception start-time-ms duration-ns opts]
  (cond-> #:traci{:id          trace-id
                  :name        name
                  :args        (arglist-for-args metadata args)
                  :result      result                       ;; TODO - shorten result
                  :start       (java.time.Instant/ofEpochMilli start-time-ms)
                  :duration-ns duration-ns
                  :version     (:version opts :current)
                  :caller      parent-name
                  :caller-id   parent-id}
          exception (assoc :traci/exception exception)
          ))

(defn- tracing-fn [name metadata f args trace-producer trace-store opts]
  (let [{:keys [parent-id depth parent-name]} *trace*
        trace-id  (str (if parent-id (str parent-id ".") "") (gensym (:name metadata)) "." depth)
        start-time-ms (System/currentTimeMillis)
        start-time-ns (System/nanoTime)
        [result exception] (binding [*trace* (-> *trace*
                                                 (update :depth inc)
                                                 (assoc :parent-id trace-id)
                                                 (assoc :parent-name name))]
                             (try [(apply f args) nil]
                                  (catch Exception e
                                    [nil e])))
        duration-ns (- (System/nanoTime) start-time-ns)]
    (api/store! trace-store
                (trace-producer trace-id name metadata parent-id parent-name
                                f args result exception start-time-ms duration-ns opts) opts)
    ;; re-throw exception
    (if exception
      (throw exception)
      result)))

(defn trace-var
  "Trace a var, which must deref to something implementing fn"
  [^clojure.lang.Var var opts]
  (let [ns       (.ns var)
        f        @var
        metadata (meta var)]
       (if (and (ifn? f) (not (:macro metadata)) (not (::tracied metadata)))
         (let [fqname (symbol (str ns) (.. var sym toString))
               trace-producer (:trace-producer opts default-trace-producer)
               trace-store    (:trace-store opts in-memory/in-memory-store)
               ] ;; we want a symbol with ns set
           (alter-meta! var assoc ::tracied f)              ;; stuff away original fn
           (alter-var-root var
                           #(fn [& args]
                              (tracing-fn fqname metadata % args trace-producer trace-store opts)))
           var
           ))))

(defn untrace-var
  [^clojure.lang.Var var]
  (let [metadata (meta var)
        f        (::tracied metadata)]
    (when f
      (alter-meta! var dissoc ::tracied)
      (alter-var-root var (constantly f)))))

(defn query-store
  "Query the store configured in `opts`. Query language is store-implementation dependent.
  If no :trace-store is set, use monoid.traci.in-memory store"
  ([q]
   (query-store q {}))
  ([q opts]
   (let [trace-store (:trace-store opts in-memory/in-memory-store)]
     (api/query trace-store q opts))))

(defn trace-ns
  "Add tracing to all fns of a namespace.
  'opts' - options to use for this tracing session.
  Untrace and trace-ns again if you need to change options.
  "
  ([namespace]
   (trace-ns namespace {}))
  ([namespace opts]
   (let [ns (the-ns namespace)]
     (when (not (= ns (the-ns 'monoid.traci)))
       (doall
         (map #(trace-var % opts) (fns ns)))))))

(defn untrace-ns [namespace]
  (let [ns (the-ns namespace)]
    (when (not (= ns (the-ns 'monoid.traci)))
      (doseq [f (fns ns)]
        (untrace-var f)))))

(defn prettify-trace [{:traci/keys [name args result exception]}]
  (str "(" name " " (with-out-str (pprint/pprint args)) ") \n=>" (with-out-str (pprint/pprint result))
       (when exception (with-out-str (pprint/pprint exception)))))