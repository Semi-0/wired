(ns graph.compiler-2-runtime.boundary
  "Boundary effect request and receipt data for compiler-2 runtime ports.")

(defn tui-write-effect-request
  [effect-id text-id payload epoch]
  {:boundary/effect true
   :boundary/id effect-id
   :boundary/port :tui
   :boundary/kind :tui/write-block
   :boundary/target {:text-id text-id}
   :boundary/payload payload
   :boundary/epoch epoch})

(defn tui-display-effect-request
  [effect-id display-id payload tick]
  {:boundary/effect true
   :boundary/id effect-id
   :boundary/port :tui
   :boundary/kind :tui/write-display
   :boundary/target {:display-id display-id}
   :boundary/payload payload
   :boundary/tick tick
   :boundary/epoch tick})

(defn xr-effect-request
  [effect-id trace-graph receipt-id epoch]
  {:boundary/effect true
   :boundary/id effect-id
   :boundary/port :xr
   :boundary/kind :xr/launch-trace
   :boundary/payload {:graph trace-graph}
   :boundary/receipt-id receipt-id
   :boundary/epoch epoch})

(defn xr-receipt
  [request status]
  {:boundary/receipt true
   :boundary/id (:boundary/id request)
   :boundary/port (:boundary/port request)
   :boundary/kind (:boundary/kind request)
   :boundary/status status
   :boundary/epoch (:boundary/epoch request)})
