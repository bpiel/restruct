(ns com.billpiel.restruct.core
  (:require [com.billpiel.restruct.destruct :as d]
            [com.billpiel.restruct.construct :as c]))


(defn restruct
  [value in-pattern out-pattern & [filter-fn]]
  (->> value
       (d/destruct in-pattern)
       (filter (or filter-fn
                   (constantly true)))
       (c/construct out-pattern)))
