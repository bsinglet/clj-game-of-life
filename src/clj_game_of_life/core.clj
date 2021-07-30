(ns clj-game-of-life.core
  (:gen-class))

(defn get-cell
  "Retrieve the contents of a given cell."
  [game-state x y]
  (get-in game-state [y x]))

(defn is-dead?
  "This function allows dead cells to be represented as either false or nil."
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
  "Some test calls to get-cell and related functions to determine they operate
  as expected."
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
  "Displays an ASCII image of a given game-state."
  ([game-state]
    (visualize-game-state game-state -5 5 -5 5)
    )
  ([game-state min-x max-x min-y max-y]
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
                "*")))))))))))

(defn get-block-game-state
  "Returns the still-life pattern of a 2x2 block."
  []
  {0 {0 true 1 true} 1 {0 true 1 true}})

(defn get-beehive-game-state
  "Returns the still-life pattern of a beehive.
  _**_
  *__*
  _**_"
  []
  {0 {1 true 2 true} 1 {0 true 3 true} 2 {1 true 2 true}})

(defn get-loaf-game-state
  "Returns the still-life pattern of a loaf.
  _**_
  *__*
  _*_*
  __*_"
  []
  {0 {1 true 2 true} 1 {0 true 3 true} 2 {1 true 3 true} 3 {2 true})

(defn get-boat-game-state
  "Returns the still-life pattern of a boat.
  **_
  *_*
  _*_"
  []
  {0 {0 true 1 true} 1 {0 true 2 true} 2 {1 true}})

(defn get-tub-game-state
  "Returns the still-life pattern of a tub.
  _*_
  *_*
  _*_"
  []
  {0 {1 true} 1 {0 true 2 true} 2 {1 true}})

(defn get-blinker-game-state
  "Returns the oscillator pattern of a blinker, which is a 1x3 vertical line
  that switches betwen a 3x1 horizontal line, then repeats (period 2.)"
  []
  {0 {1 true} 1 {1 true} 2 {1 true}})

(defn get-toad-game-state
  "Returns the oscillator pattern of a toad (period 2.)
  _***
  ***_"
  []
  {0 {1 true 2 true 3 true} 1 {0 true 1 true 2 true}})

(defn get-beacon-game-state
  "Returns the oscillator pattern of a beacon (period 2.)
  **__
  **__
  __**
  __**"
  []
  {0 {0 true 1 true} 1 {0 true 1 true} 2 {2 true 3 true} 3 {2 true 3 true}})

(defn get-pulsar-game-state
  "Returns the oscillator pattern of a pulsar (period 3.)
  __***___***__
  _____________
  *____*_*____*
  *____*_*____*
  *____*_*____*
  __***___***__
  _____________
  __***___***__
  _____________
  *____*_*____*
  *____*_*____*
  *____*_*____*
  __***___***__"
  []
  {0 {2 true 3 true 4 true 8 true 9 true 10 true}
   2 {0 true 5 true 7 true 12 true}
   3 {0 true 5 true 7 true 12 true}
   4 {0 true 5 true 7 true 12 true}
   5 {2 true 3 true 4 true 8 true 9 true 10 true}
   7 {2 true 3 true 4 true 8 true 9 true 10 true}
   8 {0 true 5 true 7 true 12 true}
   9 {0 true 5 true 7 true 12 true}
   10 {0 true 5 true 7 true 12 true}
   12 {2 true 3 true 4 true 8 true 9 true 10 true}})

(defn get-pentadecathlon-game-state
  "Returns the oscillator pattern of a penta-decathlon, which is a period 15
  oscillator."
  []
  {0 {1 true 2 true 3 true}
   1 {0 true 4 true}
   2 {0 true 4 true}
   3 {1 true 2 true 3 true}
   8 {1 true 2 true 3 true}
   9 {0 true 4 true}
   10 {0 true 4 true}
   11 {1 true 2 true 3 true}
   })

(defn get-glider-game-state
  "Returns the spaceship pattern of a glider, which is a pattern that moves
  across the universe while changing shape."
  []
  {0 {1 true}
   1 {2 true}
   2 {0 true 1 true 2 true}}
  )

(defn get-light-weight-spaceship-game-state
  "Returns the pattern of a light-weight spaceship (LWSS)."
  []
  {0 {2 true 3 true}
   1 {0 true 1 true 3 true 4 true}
   2 {0 true 1 true 2 true 3 true}
   3 {1 true 2 true}})

(defn get-middle-weight-spaceship-game-state
  "Returns the pattern of a middle-weight spaceship (MWSS)."
  []
  {0 {3 true 4 true}
   1 {0 true 1 true 2 true 4 true 5 true}
   2 {0 true 1 true 2 true 3 true 4 true}
   3 {1 true 2 true 3 true}})

(defn get-heavy-weight-spaceship-game-state
  "Returns the pattern of a heavy-weight spaceship (HWSS)."
  []
  {0 {4 true 5 true}
   1 {0 true 1 true 2 true 3 true 5 true 6 true}
   2 {0 true 1 true 2 true 3 true 4 true 5 true}
   3 {1 true 2 true 3 true 4 true}})

(defn test-next-game-state
  "A series of tests to see how the determine-next-game-state function operates."
  []
  (let [game-state
      {0 {1 true}
       1 {1 true}
       2 {1 true}}
    ]
    (println (str "Next game state: " (determine-next-game-state game-state)))
    (println (visualize-game-state game-state))
    (println (visualize-game-state (determine-next-game-state game-state)))
    (println "")
    (println (visualize-game-state (get-block-game-state)))
    (println (visualize-game-state (determine-next-game-state (get-block-game-state))))
    (println "")
    (println (visualize-game-state (get-beehive-game-state)))
    (println (visualize-game-state (determine-next-game-state (get-beehive-game-state))))
    (println "")
    (println (visualize-game-state (get-toad-game-state)))
    (println (visualize-game-state (determine-next-game-state (get-toad-game-state))))
  ))

(defn -main
  "Run a simulation of Conway's Game of Life."
  [& args]
  (test-next-game-state)
  )
