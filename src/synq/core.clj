(ns synq.core)

(defn build-coin-map
  [amount]
  (let [x (cond
            (integer? amount) amount
            (string? amount) (try
                               (Integer/parseInt amount)
                               (catch Exception e
                                 (throw (ex-info "Not supported string" {}))))
            :else (throw (ex-info "Only supports integer or convertable string" {})))]

    {:amount x
     :coins []}))

(defn update-coins
  [coin-changer denom]
  (let [num (-> coin-changer
                :amount
                (quot denom))]
    (-> coin-changer
        (update-in [:coins] #(concat % (repeat num denom)))
        (update-in [:amount] #(rem % denom)))))

(defn change-coins
  [amount & [coins]]
  (try
    (let [coin-map (build-coin-map amount)]
      (->> (or coins [200 100 25 10 5 1])
           (sort >)
           (reduce update-coins coin-map)
           :coins
           frequencies))
    (catch Exception e
      (println "Unexpected error"))))

(defn print-hierarchy-tree
  ([src]
   (print-hierarchy-tree
    src
    ""
    nil
    (some (fn [[a b]] (when (nil? b) a)) src)))
  ([src path last? node-name]
   (println (str path (when-not (nil? last?) (if last? "└" "├")) node-name))
   (let [children (filter (fn [[a b]] (= node-name b)) src)
         cnt (count children)]
     (doseq [i (range cnt)]
       (print-hierarchy-tree
        src
        (str path (when-not (nil? last?) (if last?  " " "│")))
        (= (dec cnt) i)
        (first (nth children i)))))))

(defn -main
  [& args]
  (println (change-coins 53))
  (println (change-coins 653))
  (println (change-coins 63))
  (println (change-coins 132))
  (print-hierarchy-tree
   [["A" nil]
    ["B" "A"]
    ["C" "B"]
    ["D" "C"]])
  (print-hierarchy-tree
   [["A" nil]
    ["B" "A"]
    ["C" "B"]
    ["D" "B"]
    ["E" "A"]])
  (print-hierarchy-tree
   [["A" nil]
    ["B" "A"]
    ["C" "B"]
    ["D" "C"]
    ["E" "A"]])
  (print-hierarchy-tree
   [["A" nil]
    ["B" "A"]
    ["C" "B"]
    ["D" "C"]
    ["E" "B"]]))
