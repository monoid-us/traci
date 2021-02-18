(ns monoid.traci.in-memory
  "In-memory trace capture. Uses an agent to store all traces, keyed by ns/fn"
  (:require [monoid.traci.api :as api]
            ))

(def store (agent {}))

(defn- datafy [trace]
  "Make caller and caller-id navigable."
  (with-meta trace {'clojure.core.protocols/nav
                    (fn [trace k v]
                      (case k
                        (:traci/caller :traci/caller-id)
                        (get-in @store [(:traci/caller trace) (:traci/version trace) (:traci/caller-id trace)])
                        v
                        )
                      )}))

(defn store-max
  "Store a list of IDs alongside trace and version in insertion order"
  [store trace max-count]
  (let [path [(:traci/name trace) (:traci/version trace)]
        entries (get-in store (conj path :entries) '())]
    (if (>= (count entries) max-count)
      (update-in store path (fn [traces]
                              (-> traces
                                  (assoc :entries (conj (butlast entries) (:traci/id trace)))
                                  (assoc (:traci/id trace) (datafy trace))
                                  (dissoc (last entries))   ;; remove old trace
                                  )))
      (update-in store path (fn [traces]
                              (-> traces
                                  (update :entries conj (:traci/id trace))
                                  (assoc (:traci/id trace) (datafy trace)))
                              )))))

(defn store-normal [store-state trace opts]
  (update-in
    store-state
    [(:traci/name trace) (:traci/version trace)]
    assoc
    (:traci/id trace)
    (datafy trace))
  )

(defn save [store-state trace opts]
  (if-let [max-count (::max-traces-per-version opts)]
    (store-max store-state trace max-count)
    (store-normal store-state trace opts)
    ))

(defn reset-store! []
  (send store (constantly {})))

(defmulti run-query (fn [query opts]
                  (first query)))

(defmethod run-query :get [[_ var-or-symbol version & others] opts]
  (println (list* (symbol var-or-symbol) version others))
  (if (nil? version)
    (get @store (symbol var-or-symbol))
    (get-in @store (list* (symbol var-or-symbol) version others))))

(defmethod run-query :default [_ _]
  (println "Unknown query format"))

(def in-memory-store
  (reify
    api/TraceStore
    (store! [_this trace opts]
      (when trace
        (send store save trace opts)))
    (query [_this q opts]
      (run-query q opts)
      )
    ))

(defn calls-to
  "Return a sequence of traced calls to `var-or-symbol` across all versions"
  [var-or-symbol]
  (let [all-versions (get @store (symbol var-or-symbol))]
    (mapcat (fn [[_version calls]] (map val (dissoc calls :entries))) all-versions)))

(defn last-call-to
  "Return the latest call to `var-or-symbol` across all versions"
  [var-or-symbol]
  (let [all-versions (get @store (symbol var-or-symbol))]
    (->> all-versions
         (map (fn [[_ calls]] (-> calls (dissoc :entries) last val)))
         (sort-by :traci/start #(compare %2 %1))
         first
         )))

(defn all-traces []
  (->> (tree-seq (fn [n]
                   (not (:traci/id n)))
                 (fn [n] (vals n))
                 @store)
       (filter :traci/id)))

(defn callees [var-or-symbol]
  (let [trace (last-call-to var-or-symbol)
        {:traci/keys [id version]} trace]
    (->> (all-traces)
         (filter #(and (= (:traci/caller-id %) id)
                       (= (:traci/version %) version)
                       )))))