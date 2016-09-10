(ns com.billpiel.restruct.destruct)

(declare destruct*)


(defn recur-merge-input
  [v context]
  (let [ms (filter map? v)
        ss (remove map? v)
        r (apply merge context ms)]
    (if (empty? ss)
      [r]
      (mapcat #(recur-merge-input % r) ss))))

(defn destruct-map-no-syms
  [p v]
  (let [ks (keys p)]
    (destruct*
     (mapv (partial get p) ks)
     (mapv (partial get v) ks))))

(defn destruct-map-hybrid
  [p v]
  (let [non-sym-keys (->> p keys (remove symbol?))
        p-no-sym (select-keys p non-sym-keys)
        p-syms (apply dissoc p non-sym-keys)
        v-no-sym (select-keys v non-sym-keys)
        v-syms (apply dissoc v non-sym-keys)]
    (let [x (recur-merge-input (destruct* p-no-sym v-no-sym) {})]
      (vec (for [x' x
                 y (destruct* p-syms v-syms)]
             (into [x'] y))))))

(defn apply-match-meta-mods
  [p v]
  (if (-> p meta ::nil-kv)
    [p (assoc v nil nil)]
    [p v]))

(defn destruct-map
  [p v]
  (let [[p' v'] (apply-match-meta-mods p v)
        p-keys (keys p')
        some-syms (some symbol? p-keys)
        some-non-syms (some (complement symbol?) p-keys)]
    (cond
      (and some-syms some-non-syms) (destruct-map-hybrid p' v')
      some-syms (destruct* (cycle p') v')
      some-non-syms (destruct-map-no-syms p' v'))))

(defn apply-input-meta
  [p]
  (if (some-> p first meta ::&)
    (repeat (first p))
    p))

(defn destruct-sequential
  [p v]
  (->> v
       (interleave (apply-input-meta p))
       (partition 2)
       (map (partial apply destruct*))))

(defn destruct-scalar
  [p v]
  {p v})

(defn destruct*
  [p v]
  (cond
    (map? p) (destruct-map p v)
;    (-> p meta ::&) (destruct-sequential-repeat p v)
   (sequential? p) (destruct-sequential p v)
   :else (destruct-scalar p v)))


(defn destruct
  [p v]
  (recur-merge-input (destruct* p v) {}))
