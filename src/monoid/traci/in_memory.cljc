(ns monoid.traci.in-memory
  "In-memory trace capture. Uses an agent to store all traces, keyed by ns/fn"
  (:require
    [clojure.data.priority-map :as priority-map]
    [monoid.traci.api :as api]))

(def store (agent {}))

(defn- datafy [{:traci/keys [callees]} trace]
  "Make caller and caller-id navigable. Also assoc in already existing keys"
  (with-meta (assoc trace :traci/callees callees) {'clojure.core.protocols/nav
                    (fn [trace k v]
                      (case k
                        (:traci/caller :traci/caller-id)
                        (get-in @store [(:traci/caller trace) (:traci/version trace) (:traci/caller-id trace)])
                        :traci/callees
                        (map #(get-in @store %) (:traci/callees trace))
                        v
                        )
                      )}))

(defn store-trace [store {:traci/keys [name version id caller-id caller] :as trace}  opts]
  (let [manage-fn   (if-let [max (::max-traces-per-version opts)]
                      (fn [traces]
                        (if (-> (count traces) (> max))
                          (rest traces)
                          traces))
                      identity)]
    (-> store
        (update-in [name version]
                   (fnil (fn [traces]
                           (update (manage-fn traces) id datafy trace))
                         (priority-map/priority-map-keyfn :traci/start)))
        (update-in [caller version caller-id :traci/callees]
                   (fnil conj [])
                   [name version id]
                   #_ (fnil (fn [callees]
                           (conj callees [name version id])) []) ;; TODO size limit?
                   ))))

(defn store-max
  "Store a list of IDs alongside trace and version in insertion order"
  [store trace max-count]
  (let [path [(:traci/name trace) (:traci/version trace)]
        entries (get-in store (conj path :entries) '())
        ]
    (if (>= (count entries) max-count)
      (update-in store path (fn [traces]
                              (-> traces
                                  (assoc :entries (conj (butlast entries) (:traci/id trace)))
                                  (assoc (:traci/id trace) (datafy nil trace)) ;; TODO - retain callees
                                  (dissoc (last entries))   ;; remove old trace
                                  )))
      (update-in store path (fn [traces]
                              (-> traces
                                  (update :entries conj (:traci/id trace))
                                  (assoc (:traci/id trace) (datafy nil trace)))
                              )))))

(defn store-normal [store-state {:traci/keys [name version id caller-id caller] :as trace} opts]
  (-> store-state
      ;; record the trace
      (update-in
        [name version id]
        datafy
        trace)
      ;; update callee of caller with path to trace
      (update-in
        [caller version caller-id :traci/callees]
        (fnil conj [])
        [name version id]
        ))

  #_(update-in
    store-state
    [(:traci/name trace) (:traci/version trace)]
    assoc
    (:traci/id trace)
    (datafy trace))
  )

(defn save [store-state trace opts]
  (store-trace store-state trace opts)
#_  (if-let [max-count (::max-traces-per-version opts)]
    (store-max store-state trace max-count)
    (store-normal store-state trace opts)
    ))

(defn reset-store! []
  (send store (constantly {})))

(def in-memory-store
  (reify
    api/TraceStore
    (store! [_this trace opts]
      (when trace
        (send store save trace opts)))))

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

#_(defn last-trace
  "Return the latest trace recorded from the REPL"
  []
  (let [root @store
        (->> (get root nil))
        ]
    )

  )

(defn find-trace [all-traces caller-id version]
  (filter #(and (= (:traci/caller-id %) caller-id)
                (= (:traci/version %) version)
                ) all-traces)
  )

(defn callees [var-or-symbol]
  (let [trace (last-call-to var-or-symbol)
        {:traci/keys [id version]} trace
        current-store @store
        ]
    (->> (:traci/callees trace)
         (mapv #(get-in current-store %))
         )))


(defn call-tree* [trace]
  (let [callees (find-trace (all-traces) (:traci/id trace) (:traci/version trace))]
    (mapv (fn [t] (recur t)) callees)))


(defn call-tree [var-or-symbol]
  (let [trace (last-call-to var-or-symbol)]
    (call-tree* trace)
    )
  )

(defn call-tree' [var-or-symbol]
  (tree-seq (constantly true) #(callees (if (:traci/id %) (:traci/name %) %)) var-or-symbol))