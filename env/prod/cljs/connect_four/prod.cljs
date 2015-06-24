(ns connect-four.prod
  (:require [connect-four.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
