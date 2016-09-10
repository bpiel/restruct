(ns com.billpiel.restruct.construct)

(defn fmap
  [f m]
  (into {}
        (map (fn [[k v]]
               [k (f v)])
             m)))

(defn mk-merge-meta
  [p default]
  {::merge (or (some->> p meta keys (some #{:map :seq :scalar}))
               (some-> p meta ::merge #{:map :seq :scalar})
               default)})

(declare construct*)

(defn construct-scalar
  [p v]
  (with-meta
    (mapv #(get %
                p
                (if (symbol? p)
                  ::nope
                  p))
          v)
    (mk-merge-meta p :scalar)))

(defn construct-sequential
  [p v]
  (with-meta
    (apply concat
           (for [v' v]
             (map #(construct* % [v'])
                  p)))
    (mk-merge-meta p :seq)))

(defn mk-merge-meta
  [p default]
  {::merge (or (some->> p meta keys (some #{:map :seq :scalar}))
               (some-> p meta ::merge #{:map :seq :scalar})
               default)})

(defn f-map-keys-by
  [f p m]
  (into {}
        (f (fn [[k v]] (p k))
           m)))

(defn construct-map
  [p v]
  (with-meta
    (for [v' v]
      (->> v'
           (clojure.set/rename-keys p)
           (f-map-keys-by remove symbol?)
           (fmap #(construct* % [v']))))
    (mk-merge-meta p :map)))

(defn construct*
  [p v]
  (cond
    (map? p) (construct-map p v)
    (sequential? p) (construct-sequential p v)
    :else (construct-scalar p v)))

#_ (defn merge-output-map-vals
  [& vs]
  (fmap merge-output
        (apply merge
               (apply concat vs))))

(defn merge-output-map-vals
  [& vs]
  (with-meta (apply concat vs)
    (mk-merge-meta
     (-> vs first)
     :scalar)))

(defn merge-output
  [v]
  (condp = (-> v meta ::merge)
    :map (fmap merge-output (apply merge-with
                                   merge-output-map-vals
                                   v))
    :seq (with-meta (vec (remove #{::nope}
                                 (map merge-output v)))
           (meta v))
    :scalar (first v)
    v))

(defn construct
  [p v]
  (merge-output (construct* p v)))
