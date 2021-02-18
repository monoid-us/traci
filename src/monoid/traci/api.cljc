(ns monoid.traci.api
  "Internal API used to interact with trace storage"
  )

(defprotocol TraceStore
  (store! [this trace options])
  (query [this q options]))
