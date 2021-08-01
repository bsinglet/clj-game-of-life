(defn remove-non-trues
  "Given a map, only keep values that are true."
  [my-map]
  (reduce (fn [new-map [key val]]
      (if (true? val)
        (assoc new-map key val)
        new-map))
      {}
      my-map))

(defn remove-empty
  "Given a map of maps, only keep values that are non-empty."
  [my-map]
  (reduce (fn [new-map [key val]]
      (if (not (empty? val))
        (assoc new-map key val)
        new-map))
      {}
      my-map))

(defn filter-cells
  "Given a game-state, remove the dead-cells. This is intended to be called by
  remove-dead-cells, which removes any empty rows on the board."
  [game-state]
  (loop [remaining-keys (keys game-state) retval {}]
    (if (empty? remaining-keys)
      retval
      (recur (rest remaining-keys)
        (assoc retval (first remaining-keys)
        (remove-non-trues (get game-state (first remaining-keys))))))))

(defn remove-dead-cells
  "This function just purges the dead cells from the map to make it easier for
  a human to read."
  [game-state]
  (remove-empty (filter-cells game-state)))

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

(defn get-all-sub-keys
  "Given a map of maps, returns a list of all the inner-keys."
  [game-state]
  (apply concat (map #(keys %) (vals game-state))))
