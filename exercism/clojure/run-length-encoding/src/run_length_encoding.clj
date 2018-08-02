(ns run-length-encoding)

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (->> plain-text
       (partition-by identity)
       (map (fn [xs]
              (let [c (count xs)
                    x (first xs)]
                (if (= c 1)
                  x
                  (str c x)))))
       (apply str)))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [cipher-text]
  (->> cipher-text
       (re-seq #"([0-9]*)([^0-9])")
       (mapcat (fn [[_ c x]]
                (let [i (if-not (empty? c)
                          (Integer/parseInt c)
                          1)]
                  (repeat i x))))
       (apply str)))
