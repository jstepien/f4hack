(ns hack.core
  (:require [clojure.string :as str]))

;; Hacking logic

(defn bit-vectors [on len]
  (cond (< on 1) [(vec (repeat len false))]
        (< len 1) nil
        (pos? on) (concat
                    (for [vec (bit-vectors (dec on) (dec len))]
                      (conj vec true))
                    (for [vec (bit-vectors on (dec len))]
                      (conj vec false)))))

(defn match->regex [word hits]
  (let [patterns (for [bits (bit-vectors hits (count word))]
                   (->> (for [[bit idx] (map vector bits (range))]
                          (if bit
                            (subs word idx (inc idx))
                            (str "[^" (get word idx) "]")))
                        (apply str)))]
    (re-pattern (str "^(" (str/join \| patterns) ")$"))))

(defn results->predicate [results]
  (let [regexps (for [[word hit] results]
                  (match->regex word hit))]
    (fn [word]
      (every? #(re-matches % word) regexps))))

(defn suggestions [words results]
  (let [possible? (results->predicate results)]
    (->> (for [word words
               :when (possible? word)]
           [word (count (filter (results->predicate (merge {word 0} results))
                                words))])
         (sort-by second))))

;; Testing and benchmarking

(def words
  ["about"
   "aware"
   "began"
   "being"
   "bells"
   "celts"
   "chalk"
   "crazy"
   "didnt"
   "doing"
   "dwell"
   "earth"
   "eight"
   "every"
   "first"
   "games"
   "great"
   "heard"
   "hence"
   "heres"
   "irish"
   "jutes"
   "learn"
   "local"
   "looks"
   "meant"
   "media"
   "meeny"
   "mouth"
   "never"
   "night"
   "ninth"
   "norse"
   "other"
   "queer"
   "quite"
   "risky"
   "rural"
   "sense"
   "shred"
   "since"
   "speak"
   "spoke"
   "still"
   "stuff"
   "their"
   "there"
   "thing"
   "think"
   "those"
   "today"
   "usage"
   "welsh"
   "whats"
   "which"
   "women"
   "words"
   "world"
   "would"])

(defn similarity [as bs]
  (apply + (map (fn [a b] (if (= a b) 1 0)) as bs)))

(def counter (atom []))

(defn summary! []
  (let [vals @counter]
    (println "median" (nth (sort vals) (bit-shift-right (count vals) 1))
             "mean" (/ (reduce + vals) (count vals))
             "max" (last (sort vals)))))

(defn test! [n]
  (let [goal (first (shuffle words))]
    (time (loop [results {}]
      (let [[[sug _]] (suggestions words results)]
        (if (= sug goal)
          (do
            (println "success after" (count results) "attempts:" goal)
            (swap! counter conj (count results)))
          (recur (assoc results sug (similarity goal sug))))))))
  (if (pos? n)
    (js/setTimeout #(test! (dec n)) 5)
    (summary!)))

;; User interface

(defn notify [msg]
  (set! (.-innerHTML (.getElementById js/document "msg")) msg))

(defn react [textarea]
  (try
    (let [input (.-value textarea)
          pairs (for [line (str/split input "\n")]
                  (str/split line " "))
          words (->> pairs (map first) (filter seq))
          results (->> pairs
                       (filter #(< 1 (count %)))
                       (map (fn [[word sim]]
                              [word (js/parseInt sim)]))
                       (into {}))
          [[suggestion]] (suggestions words results)
          strong #(str "<strong>" % "</strong>")
          solutions (filter (results->predicate results) words)]
      (if (seq solutions)
        (->> ["You should try " (strong suggestion) "."
              "<br>"
              "Possible solutions: "
              (str/join ", " (map strong solutions))]
             (apply str)
             notify)
        (notify "I have no clue how to solve this.")))
    (catch :default e
      (notify "errorrrororror"))))

(defn seed! [textarea]
  (set! (.-value textarea)
        (str/join "\n" ["first"
                        "norse"
                        "women"
                        "their"
                        "looks"
                        "thing 0"
                        "mouth"
                        "about 1"
                        "eight"
                        "media"
                        "heres"
                        "would"])))

;; Off we go

(defn main []
  #_(enable-console-print!)
  #_(test! 200)
  (let [textarea (.getElementById js/document "words")
        reaction #(react textarea)]
    (if (empty? (.-value textarea))
      (seed! textarea))
    (js/setTimeout reaction 5)
    (doseq [prop ["onkeyup" "onchange" "onblur"]]
      (aset textarea prop reaction))))

(set! (.-onload js/window) main)
