(ns binary-search)

(defn middle [xs]
  (quot (count xs) 2))

(defn sf- [x xs]
  (cond
    (empty? xs) nil
    (= 1 (count xs)) (if (= (first xs) x) 0 nil)
    :else (let [zs (vec xs)
                m (middle zs)
                as (subvec zs 0 m)
                bs (subvec zs m)
                in-as (sf- x as)
                in-bs (sf- x bs)]
            (cond
              in-as in-as
              in-bs (+ m in-bs)
              :else nil))))

(defn search-for [x xs]
  (if-let [result (sf- x xs)]
    result
    (throw (Exception. "not found"))))

