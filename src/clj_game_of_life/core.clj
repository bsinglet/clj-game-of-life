(ns clj-game-of-life.core
  (:gen-class))

(defn get-cell
  "Retrieve the contents of a given cell."
  [game-state x y]
  (some (fn [[sub_x sub_y]] (and (= sub_x x) (= sub_y y))) game-state))

(defn is-dead?
  "This function allows dead cells to be represented as either false or nil."
  [value]
  (or (nil? value) (false? value)))

(defn get-neighbors
    "Get the eight neighbors of the specified cell as a list of coordinate
    pairs (list of lists)."
    [game-state x y]
    (list (list (- x 1) (- y 1))  ; upper-left
    (list x (- y 1))              ; above
    (list (+ x 1) (- y 1))        ; upper-right
    (list (- x 1) y)              ; left
    (list (+ x 1) y)            ; right
    (list (- x 1) (+ y 1))        ; lower-left
    (list x (+ y 1))              ; below
    (list (+ x 1) (+ y 1))))      ; lower-right

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

(defn get-list-potentially-pregnant-cells
  "Gets the list of currently-dead cells that are adjacent to currently-living
  cells. These are the ones that may come alive in the next iteration."
  [game-state]
  (filter #(is-dead? (get-cell game-state (first %) (second %)))
    (set (apply concat
      (map #(get-neighbors game-state (first %) (second %)) game-state)))))

(defn determine-next-game-state-living
  "Given the current game state, a list of living cells, and a working version
  of the next game state, compute the next values for those positions."
  [old-game-state next-game-state]
  (loop [remaining old-game-state nexter-game-state []]
    (if (empty? remaining)
      nexter-game-state
      (recur (rest remaining)
        (let [num-living-neighbors (get-count-living-neighbors old-game-state (first (first remaining)) (second (first remaining)))]
          (if (or (= num-living-neighbors 2) (= num-living-neighbors 3))
            (do
              (println (str "Living cell (" (first (first remaining)) ", " (second (first remaining)) ") survives."))
              (conj nexter-game-state (first remaining)))
            (do
              (println (str "Living cell (" (first (first remaining)) ", " (second (first remaining))
                ") dies because it has " num-living-neighbors " living neighbors."))
              nexter-game-state)))))))

(defn determine-next-game-state-dead
  "Given the current game state, a list of dead cells, and a working version
  of the next game state, compute the next values for those positions."
  [old-game-state dead-cells next-game-state]
  (loop [remaining dead-cells nexter-game-state next-game-state]
    (if (empty? remaining)
      nexter-game-state
      (recur (rest remaining)
        (let [num-living-neighbors (get-count-living-neighbors old-game-state (first (first remaining)) (second (first remaining)))]
          (if (= 3 num-living-neighbors)
            (do
              (println (str "Dead cell (" (first (first remaining)) ", " (second (first remaining))
                ") is born because it has " num-living-neighbors " living neighbors."))
              (conj nexter-game-state (first remaining)))
            (do
              (println (str "Dead cell (" (first (first remaining)) ", " (second (first remaining)) ") stays dead."))
              nexter-game-state)))))))

(defn determine-next-game-state
  "This is the msot important function of this program. It takes the current
  game-state and computes the new game-state by getting a list of living cells,
  then a list of the dead neighbors of those living cells. With that combined
  list, it simply applies the rules of Conway's Game of Life to each relevant
  cell."
  [old-game-state]
  (let [dead-cells (get-list-potentially-pregnant-cells old-game-state)]
      (determine-next-game-state-dead old-game-state dead-cells
        (determine-next-game-state-living old-game-state old-game-state))))

(defn test-get-cell
  "Some test calls to get-cell and related functions to determine they operate
  as expected."
  []
  (let [game-state
        ['(0 0) '(1 0) '(2 0)
         '(0 1) '(1 1) '(2 1)
         '(-5 3) '(4 3)
         '(-10 4) '(10 4)
         '(4 6)
         '(7 7)]]
    (println (str "Game state: " game-state))
    (println (str "nth game-state 3 " (get game-state 3)))
    (println (str "nth game-state 4 " (get 4)))
    (println (str "0, 0: " (get-cell game-state 0 0)))
    (println (str "-10, 4: " (get-cell game-state -10 4)))
    (println (str "4, 6: " (get-cell game-state 4 6)))
    (println (str "7, 7: " (get-cell game-state 7 7)))
    (println (str "Get neighbors of 1, 1: " (get-neighbors game-state 1 1)))
    (println (str "Get count of living neighbors of 1, 1: "
      (get-count-living-neighbors game-state 1 1)))))

(defn get-bounding-box
  "Given a game-state, returns a list containing the min-x, max-x, min-y, and
  max-y, in that order."
  [game-state]
  (let [x-values (map (fn [[x y]] x) game-state)
        y-values (map (fn [[x y]] y) game-state)]
    [(apply min x-values)
     (apply max x-values)
     (apply min y-values)
     (apply max y-values)]))

(defn visualize-game-state
  "Displays an ASCII image of a given game-state."
  ([game-state]
    (let [bounding-box (get-bounding-box game-state)]
      (let [min-x (nth bounding-box 0)
            max-x (nth bounding-box 1)
            min-y (nth bounding-box 2)
            max-y (nth bounding-box 3)]
          (visualize-game-state game-state min-x max-x min-y max-y)))
    )
  ([game-state min-x max-x min-y max-y]
  (loop [y min-y retval ""]
    (if (> y max-y)
      retval
      (recur (inc y) (str retval "\n"
        (loop [x min-x subval ""]
          (if (> x max-x)
            subval
            (recur (inc x) (str subval
              (if (is-dead? (get-cell game-state [y x]))
                "_"
                "*")))))))))))

(defn get-block-game-state
  "Returns the still-life pattern of a 2x2 block."
  []
  ['(0 0) '(1 0)
   '(0 1) '(1 1)])

(defn get-beehive-game-state
  "Returns the still-life pattern of a beehive.
  _**_
  *__*
  _**_"
  []
  ['(1 0) '(2 0)
   '(0 1) '(3 1)
   '(1 2) '(2 2)])

(defn get-loaf-game-state
  "Returns the still-life pattern of a loaf.
  _**_
  *__*
  _*_*
  __*_"
  []
  ['(1 0) '(2 0)
   '(0 1) '(3 1)
   '(1 2) '(3 2)
   '(2 3)])

(defn get-boat-game-state
  "Returns the still-life pattern of a boat.
  **_
  *_*
  _*_"
  []
  ['(0 0) '(1 0)
   '(0 1) '(2 1)
   '(1 2)])

(defn get-tub-game-state
  "Returns the still-life pattern of a tub.
  _*_
  *_*
  _*_"
  []
  ['(1 0)
   '(0 1) '(2 1)
   '(1 2)])

(defn get-blinker-game-state
  "Returns the oscillator pattern of a blinker, which is a 1x3 vertical line
  that switches betwen a 3x1 horizontal line, then repeats (period 2.)"
  []
  ['(1 0)
   '(1 1)
   '(1 2)])

(defn get-toad-game-state
  "Returns the oscillator pattern of a toad (period 2.)
  _***
  ***_"
  []
  ['(1 0) '(2 0) '(3 0)
   '(0 1) '(1 1) '(2 1)])

(defn get-beacon-game-state
  "Returns the oscillator pattern of a beacon (period 2.)
  **__
  **__
  __**
  __**"
  []
  ['(0 0) '(1 0)
   '(0 1) '(1 1)
   '(2 2) '(3 2)
   '(2 3) '(3 3)])

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
  ['(2 0) '(3 0) '(4 0) '(8 0) '(9 0) '(10 0)
   '(0 2) '(5 2) '(7 2) '(12 2)
   '(0 3) '(5 3) '(7 3) '(12 3)
   '(0 4) '(5 4) '(7 4) '(12 4)
   '(2 5) '(3 5) '(4 5) '(8 5) '(9 5) '(10 5)
   '(2 7) '(3 7) '(4 7) '(8 7) '(9 7) '(10 7)
   '(0 8) '(5 8) '(7 8) '(12 8)
   '(0 9) '(5 9) '(7 9) '(12 9)
   '(0 10) '(5 10) '(7 10) '(12 10)
   '(2 12) '(3 12) '(4 12) '(8 12) '(9 12) '(10 12)])

(defn get-pentadecathlon-game-state
  "Returns the oscillator pattern of a penta-decathlon, which is a period 15
  oscillator."
  []
  ['(1 0) '(2 0) '(3 0)
   '(0 1) '(4 1)
   '(0 2) '(4 2)
   '(1 3) '(2 3) '(3 3)
   '(1 8) '(2 8) '(3 8)
   '(0 9) '(4 9)
   '(0 10) '(4 10)
   '(1 11) '(2 11) '(3 11)])

(defn get-glider-game-state
  "Returns the spaceship pattern of a glider, which is a pattern that moves
  across the universe while changing shape."
  []
  ['(1 0) '(2 1)
  '(0 2) '(1 2) '(2 2)])

(defn get-light-weight-spaceship-game-state
  "Returns the pattern of a light-weight spaceship (LWSS)."
  []
  ['(2 0) '(3 0)
   '(0 1) '(1 1) '(3 1) '(4 1)
   '(0 2) '(1 2) '(3 2) '(4 2)
   '(1 3) '(2 3)])

(defn get-middle-weight-spaceship-game-state
  "Returns the pattern of a middle-weight spaceship (MWSS)."
  []
  ['(3 0) '(4 0)
   '(0 1) '(1 1) '(2 1) '(4 1) '(5 1)
   '(0 2) '(1 2) '(2 2) '(4 2) '(5 2)
   '(1 3) '(2 3) '(3 3)])

(defn get-heavy-weight-spaceship-game-state
  "Returns the pattern of a heavy-weight spaceship (HWSS)."
  []
  ['(4 0) '(5 0)
   '(0 1) '(1 1) '(2 1) '(3 1) '(5 1) '(6 1)
   '(0 2) '(1 2) '(2 2) '(3 2) '(5 2) '(6 2)
   '(1 3) '(2 3) '(3 3) '(4 3)])

(defn get-gosper-glider-gun-game-state
  "Returns the first known glider gun, discovered by Bill Gosper in 1970."
  []
  ['(24 0)
   '(22 1) '(24 1)
   '(12 2) '(13 2) '(20 2) '(21 2) '(34 2) '(35 2)
   '(11 3) '(15 3) '(20 3) '(21 3) '(34 3) '(35 3)
   '(0 4) '(1 4) '(10 4) '(16 4) '(20 4) '(21 4)
   '(0 5) '(1 5) '(10 5) '(14 5) '(16 5) '(17 5) '(22 5) '(24 5)
   '(10 6) '(16 6) '(24 6)
   '(11 7) '(15 7)
   '(12 8) '(13 8)])

(defn visualize-pattern-and-next
  "This function prints the given game-state and the following one. It also
  returns the next game state, with the dead cells removed so you can better
  inspect the result, or chain multiple calls to this function."
  [game-state]
  (let [next-game-state (determine-next-game-state game-state)]
    (println "")
    (println (visualize-game-state game-state))
    (println (visualize-game-state next-game-state))
    ; we return a cleaned-up version of the next state, to make debugging
    ; easier.
    next-game-state)
  )

(defn test-next-game-state
  "A series of tests to see how the determine-next-game-state function operates."
  []
  (loop [patterns [(get-block-game-state) (get-beehive-game-state)
      (get-loaf-game-state) (get-boat-game-state) (get-boat-game-state)
      (get-tub-game-state) (get-blinker-game-state) (get-toad-game-state)
      (get-beacon-game-state) (get-pulsar-game-state)
      (get-pentadecathlon-game-state) (get-glider-game-state)
      (get-light-weight-spaceship-game-state)
      (get-middle-weight-spaceship-game-state)
      (get-heavy-weight-spaceship-game-state)
      ]]
    (if (empty? patterns)
      nil
      (do
        (visualize-pattern-and-next (first patterns))
        (recur (rest patterns))))))

(defn -main
  "Run a simulation of Conway's Game of Life."
  [& args]
  (test-next-game-state)
  )
