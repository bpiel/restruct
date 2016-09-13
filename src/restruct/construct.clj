(ns com.billpiel.restruct.construct)

(defn fmap
  [f m]
  (into {}
        (map (fn [[k v]]
               [k (f v)])
             m)))

(defn mk-merge-meta
  [p default]
  (let [m (meta p)]
    (merge {::merge (or (some->> m keys (some #{:map :seq :scalar}))
                        (some-> m ::merge #{:map :seq :scalar})
                        default)}
           (select-keys m [:first :last :vec]))))

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

(defn merge-output-map-vals
  [& vs]
  (with-meta (apply concat vs)
    (mk-merge-meta
     (-> vs first)
     :scalar)))

(defn get-merge-meta
  [v]
  (let [m (meta v)]
    (or (some #{:first :last :vec} (keys m))
        (-> m ::merge #{:map :seq :scalar}))))

(defn ->first
  [[v]]
  (if (coll? v)
    (first v)
    v))

(defn ->last
  [[v]]
  (if (coll? v)
    (last v)
    v))

(defn ->vec
  [[v]]
  (cond (vector? v) v
        (coll? v) (vec v)
        :else [v]))

(defn merge-scalar
  [v]
  (if (-> v count (= 1))
    (first v)
    (with-meta (vec v)
      {::container true})))

(defn merge-output
  [v]
  (condp = (get-merge-meta v)
    :map (fmap merge-output (apply merge-with
                                   merge-output-map-vals
                                   v))
    :seq (with-meta (vec (remove #{::nope}
                                 (map merge-output v)))
           (meta v))
    :first (->first v)
    :last (->last v)
    :vec (->vec v)
    :scalar (merge-scalar v)
    v))

(defn construct
  [p v]
  (merge-output (construct* p v)))
