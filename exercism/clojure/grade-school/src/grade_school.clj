(ns grade-school)

(defn add [db s g]
  (update db g #(vec (conj % s))))

(defn grade [db g]
  (db g []))

(defn sorted [db]
  (into (sorted-map) (for [[g ss] db] [g (sort ss)])))
