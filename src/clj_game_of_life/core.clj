(ns clj-game-of-life.core
  (:gen-class))

(defn get-cell
  "Retrieve the contents of a given cell."
  [game-state x y]
  (get-in game-state [y x]))

(defn is-dead?
  ""
  [value]
  (or (nil? value) (false? value)))

(defn get-neighbors
    "Get the eight neighbors of the specified cell as a list of coordinate
    pairs (list of lists)."
    [game-state x y]
    (list (list (- x 1) (- y 1))
    (list x (- y 1))
    (list (+ x 1) (- y 1))
    (list (- x 1) y)
    (list x (+ y 1))
    (list (- x 1) (+ y 1))
    (list x (+ y 1))
    (list (+ x 1) (+ y 1))))

(defn get-neighbors-values
  "Get the eight neighbors of the specified cell."
  [game-state x y]
  (map #(get-cell game-state (first %) (second %)) (get-neighbors game-state x y)))

(defn get-count-living-neighbors
  "Calls get-neighbors to get a sequence of neighbors, returns the
  number of them that are alive."
  [game-state x y]
  (reduce + (map #(if (true? %) 1 0)
    (get-neighbors-values game-state x y))))

(defn get-list-live-cells
  "Given the game state, return a list of coordinate pairs of living cells. For
  example: ((0 1) (0 3) (1 0) (5 6))"
  [game-state]
  (loop [remaining-keys (keys game-state) all-cells []]
    (if (empty? remaining-keys)
      all-cells
      (recur (rest remaining-keys)
        (concat all-cells
          (let [my-key (first remaining-keys)]
            (map #(list % my-key) (keys (get game-state my-key)))))))))

(defn get-list-potentially-pregnant-cells
  "Gets the list of currently-dead cells that are adjacent to currently-living
  cells. These are the ones that may come alive in the next iteration."
  [game-state living-cells]
  (filter #(is-dead? (get-cell game-state (first %) (second %)))
    (set (apply concat
      (map #(get-neighbors game-state (first %) (second %)) living-cells)))))

(defn determine-next-game-state-living
  "Given the current game state, a list of living cells, and a working version
  of the next game state, compute the next values for those positions."
  [old-game-state living-cells next-game-state]
  (loop [remaining living-cells nexter-game-state next-game-state]
    (if (empty? remaining)
      nexter-game-state
      (recur (rest remaining)
        (assoc-in nexter-game-state [(first (first remaining)) (second (first remaining))]
          (let [num-living-neighbors (get-count-living-neighbors old-game-state (first (first remaining)) (second (first remaining)))]
            (if (or (= num-living-neighbors 2) (= num-living-neighbors 3))
                true
                false)))))))

(defn determine-next-game-state-dead
  "Given the current game state, a list of dead cells, and a working version
  of the next game state, compute the next values for those positions."
  [old-game-state dead-cells next-game-state]
  (loop [remaining dead-cells nexter-game-state next-game-state]
    (if (empty? remaining)
      nexter-game-state
      (recur (rest remaining)
        (assoc-in nexter-game-state [(first (first remaining)) (second (first remaining))]
          (if (= 3 (get-count-living-neighbors old-game-state (first (first remaining)) (second (first remaining))))
            true
            false))))))

(defn determine-next-game-state
  "This is the msot important function of this program. It takes the current
  game-state and computes the new game-state by getting a list of living cells,
  then a list of the dead neighbors of those living cells. With that combined
  list, it simply applies the rules of Conway's Game of Life to each relevant
  cell."
  [old-game-state]
  (let [living-cells (get-list-live-cells old-game-state)]
    (let [dead-cells (get-list-potentially-pregnant-cells
        old-game-state living-cells)]
      (determine-next-game-state-dead old-game-state dead-cells
        (determine-next-game-state-living old-game-state living-cells old-game-state)))))


(defn test-get-cell
  ""
  []
  (let [game-state
        {0 {0 "0, 0"
            1 "1, 0"
            2 "2, 0"},
         1 {0 "0, 1"
            1 "1, 1"
            2 "2, 1"},
         3 {-5 "-5, 3" 4 "4, 3"},
         4 {-10 "-10, 4" 10 "10, 4"},
         6 {4 "4, 6"},
         7 {7 "7, 7"}}]
    (println (str "Game state: " game-state))
    (println (str "nth game-state 3 " (get game-state 3)))
    (println (str "nth game-state 4 3 " (get (get game-state 3) 4)))
    (println (str "0, 0: " (get-cell game-state 0 0)))
    (println (str "-10, 4: " (get-cell game-state -10 4)))
    (println (str "4, 6: " (get-cell game-state 4 6)))
    (println (str "7, 7: " (get-cell game-state 7 7)))
    (println (str "Get neighbors of 1, 1: " (get-neighbors game-state 1 1)))
    (println (str "Get count of living neighbors of 1, 1: "
      (get-count-living-neighbors game-state 1 1)))))

(defn visualize-game-state
  ""
  [game-state min-x max-x min-y max-y]
  (loop [y min-y retval ""]
    (if (= y max-y)
      retval
      (recur (inc y) (str retval "\n"
        (loop [x min-x subval ""]
          (if (= x max-x)
            subval
            (recur (inc x) (str subval
              (if (is-dead? (get-in game-state [y x]))
                "_"
                "*"))))))))))

(defn test-next-game-state
  ""
  []
  (let [game-state
      {0 {0 true}
       1 {0 true}
       2 {0 true}}
    ]
    (println (str "Next game state: " (determine-next-game-state game-state)))
  ))

(defn -main
  "Run a simulation of Conway's Game of Life."
  [& args]
  ;(test-get-cell)
  (test-next-game-state)
  ;(println (determine-next-game-state {0 {1 true} 1 {1 true} 2 {1 true}}))
  (println (visualize-game-state {0 {1 true} 1 {1 true} 2 {1 true}} 0 4 0 4))
  (println (visualize-game-state
    (determine-next-game-state {0 {1 true} 1 {1 true} 2 {1 true}}) 0 4 0 4))
  (println
    (get-count-living-neighbors {0 {1 true} 1 {1 true} 2 {1 true}} 1 2))
  )
