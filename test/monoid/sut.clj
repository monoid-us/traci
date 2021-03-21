(ns monoid.sut
  (:require
    [monoid.traci :as traci]
    [monoid.traci.in-memory :as in-memory]
    [clojure.test :refer :all]))

(defn person [first last]
  #:person{:first first :last last})

(defn befriend [person new-friend]
  (update person :friends (fnil conj #{}) new-friend))

(defn rescue [person other-person]
  (update person :rescued (fnil conj #{}) other-person))

(defn befriend-all [person & others]
  (reduce (fn [p next]
            (befriend p next)) person others))

(defn ep4 []
  (let [luke (person "Luke" "Skywalker")
        obi-wan (person "Obiwan" "Kenobi")
        han (person "Han" "Solo")
        leia (person "Leia" "Organa")
        chewie (person "Chewbacca" "")
        ]
    (-> luke
        (befriend obi-wan)
        (befriend-all han leia chewie)
        (befriend-all obi-wan)
        (rescue leia)
        )
    )
  )

(defmulti force-sensitivity :person/last)
(defmethod force-sensitivity "Skywalker" [person]
  :strong)

(defmethod force-sensitivity "Kenobi" [person]
  :medium)

(defmethod force-sensitivity :default [person]
  (println "Last" (:last person))
  :none)


(defn reset-all [opts]
  (in-memory/reset-store!)
  (traci/untrace-ns *ns*)
  (traci/trace-ns *ns* opts))

(defn run-and-query []
  (reset-all {})
  (ep4)
  (ep4)
  (ep4)
  (in-memory/calls-to #'ep4)
  )

(defn run-times [n]
  (reset-all {::in-memory/max-traces-per-version 5})
  (dotimes [c n]
    (force-sensitivity (ep4))))

(defn throw-up []
  (ep4)
  (throw (Exception. "Need to throw")))

(comment
  (let [han (person "Han" "Solo")
        luke (person "Luke" "Skywalker")
        ]
    (-> han
        (befriend luke))))

(comment
  (def m (in-memory/last-call-to #'person))
  )
