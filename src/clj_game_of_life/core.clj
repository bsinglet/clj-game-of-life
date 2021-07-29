(ns clj-game-of-life.core
  (:gen-class))

(defn get-cell
  "Retrieve the contents of a given cell."
  [game-state x y]
  (get-in game-state [y x]))

(defn get-neighbors
  "Get the eight neighbors of the specified cell."
  [game-state x y]
  [(get-cell game-state (- x 1) (- y 1))
  (get-cell game-state x (- y 1))
  (get-cell game-state (+ x 1) (- y 1))
  (get-cell game-state (- x 1) y)
  (get-cell game-state x (+ y 1))
  (get-cell game-state (- x 1) (+ y 1))
  (get-cell game-state x (+ y 1))
  (get-cell game-state (+ x 1) (+ y 1))
  ])

(defn get-count-living-neighbors
  "Calls get-neighbors to get a sequence of neighbors, returns the
  number of them that are alive."
  [game-state x y]
  (loop [remaining (get-neighbors game-state x y) total 0]
    (if (empty? remaining)
      total
      (recur (rest remaining)
        (if (first remaining)
          (inc total)
          total)))))

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

(defn determine-next-game-state
  ""
  [old-game-state]
  (loop [remaining old-game-state new-game-state {}]
    (recur nil nil)))

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

(defn prototype-get-living-cells
  "This is a prototype of get-list-live-cells I wrote so I could build up the
  functionality from an outer map with a single key to the full-scale solution."
  []
  (let [game-state
        {0 {0 true, 1 nil, 2 true}
         1 {0 true, 1 nil, 2 true}}]
        (loop [remaining-keys (keys game-state) all-cells []]
          (if (empty? remaining-keys)
            all-cells
            (recur (rest remaining-keys)
              (concat all-cells
                (let [my-key (first remaining-keys)]
                  (map #(list % my-key) (keys (get game-state my-key))))))))))

(defn -main
  "Run a simulation of Conway's Game of Life."
  [& args]
  (test-get-cell)
  (println (str "prototype-get-living-cells: "
      (take 6 (prototype-get-living-cells)))))
