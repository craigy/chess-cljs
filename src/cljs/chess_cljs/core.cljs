(ns chess-cljs.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.reader :as reader]
            [clojure.string :as str]))

(defn col-index
  [col]
  ({'a 0 'b 1 'c 2 'd 3 'e 4 'f 5 'g 6 'h 7
    \a 0 \b 1 \c 2 \d 3 \e 4 \f 5 \g 6 \h 7} col))

(defn col-name [col] (['a 'b 'c 'd 'e 'f 'g 'h] col))

(defn parse-row
  ([row-num row-str] (parse-row row-num row-str 0))
  ([row-num row-str col]
    (if (empty? row-str) {}
      (if (number? (reader/read-string (first row-str)))
        (parse-row
          row-num
          (rest row-str)
          (+ (int col)
             (reader/read-string
               (str (first row-str)))))
        (conj
          (hash-map
            (str
              (col-name col)
              (inc row-num))
            (first row-str))
          (parse-row
            row-num
            (rest row-str)
            (inc col)))))))

(defn parse-position [position-str]
  (apply
    conj
    (map-indexed
      parse-row
      (reverse (str/split position-str #"/")))))

(defn parse-color [active-color-str]
  (or
    (when (= active-color-str "w") "white")
    (when (= active-color-str "b") "black")))

(defn print-color [color]
  (or
    (when (= color "white") "w")
    (when (= color "black") "b")))

(defn parse-fen [fen-str]
  (let [fen-split (str/split fen-str #" ")]
    {:position (parse-position (nth fen-split 0))
     :active-color (parse-color (nth fen-split 1))
     :castling (nth fen-split 2)
     :en-passant (nth fen-split 3)
     :halfmove-clock (nth fen-split 4)
     :fullmove (nth fen-split 5)}))

(defn row [square]
  (dec
    (reader/read-string
      (str (last square)))))

(defn col [square]
  (col-index (first square)))

(defn rc
  ([square]
    (if (nil? square)
      nil
      [(row square) (col square)])))

(defn on-board?
  ([r c]
    (if
      (or (nil? r) (nil? c))
      false
      (and (<= 0 r 7) (<= 0 c 7))))
  ([square] (on-board? (row square) (col square))))

(defn named-square
  ([[r c]] (named-square r c))
  ([row col]
    (if (on-board? row col)
      (str (col-name col) (inc row))
      nil)))

(defn rc-on-board?
  ([[r c]] (on-board? r c)))

(defn occupied?
  ([position square]
    (contains? position square))
  ([position r c]
    (occupied? position (named-square r c))))

(defn piece-color
  ([piece]
    (get
      { \K "white"
       \Q "white"
       \R "white"
       \B "white"
       \N "white"
       \P "white"
       \k "black"
       \q "black"
       \r "black"
       \b "black"
       \n "black"
       \p "black" }
      piece)))

(defn white? [piece]
  (contains? #{ "white" \K \Q \R \B \N \P "w" } piece))

(defn black? [piece]
  (contains? #{ "blacK" \k \q \r \b \n \p "b" } piece))

(defn is-king? [piece]
  (contains? #{ \K \k } piece ))

(defn is-queen? [piece]
  (contains? #{ \Q \q } piece ))

(defn is-rook? [piece]
  (contains? #{ \R \r } piece ))

(defn is-bishop? [piece]
  (contains? #{ \B \b } piece ))

(defn is-knight? [piece]
  (contains? #{ \N \n } piece ))

(defn is-pawn? [piece]
  (contains? #{ \P \p } piece ))

(defn enemy?
  [me them] (if (white? me) (black? them) (white? them)))

(defn movable? [position [r c]] (and (rc-on-board? [r c]) (not (occupied? position r c))))

(defn enemy-on?
  ([position color [r c]] (and (on-board? r c) (enemy? color (position (named-square r c))))))

(defn up-seq [r c] (partition 2 (interleave (iterate inc (inc r)) (iterate identity c))))

(defn right-seq [r c] (partition 2 (interleave (iterate dec (dec r)) (iterate identity c))))

(defn down-seq [r c] (partition 2 (interleave (iterate identity r) (iterate inc (inc c)))))

(defn left-seq [r c] (partition 2 (interleave (iterate identity r) (iterate dec (dec c)))))

(defn ne-seq [r c] (partition 2 (interleave (iterate inc (inc r)) (iterate inc (inc c)))))

(defn se-seq [r c] (partition 2 (interleave (iterate dec (dec r)) (iterate inc (inc c)))))

(defn sw-seq [r c] (partition 2 (interleave (iterate dec (dec r)) (iterate dec (dec c)))))

(defn nw-seq [r c] (partition 2 (interleave (iterate inc (inc r)) (iterate dec (dec c)))))

(defn take-while-incl
  [pred incl coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) (take-while-incl pred incl (rest s)))
        (when (incl (first s))
          (list (first s)))))))

(defn line-moves
  [square-seq position color r c]
  (take-while-incl
    (partial movable? position)
    (partial enemy-on? position color)
    (square-seq r c)))

(defn bishop-moves
  ([square position color r c]
    (map named-square
         (concat
           (line-moves ne-seq position color r c)
           (line-moves se-seq position color r c)
           (line-moves sw-seq position color r c)
           (line-moves nw-seq position color r c)))))

(defn rook-moves
  ([square position color r c]
    (map named-square
         (concat
           (line-moves up-seq position color r c)
           (line-moves right-seq position color r c)
           (line-moves down-seq position color r c)
           (line-moves left-seq position color r c)))))

(defn queen-moves
  [square position color r c]
  (concat
    (bishop-moves square position color r c)
    (rook-moves square position color r c)))

(defn knight-moves
  ([square position color r c]
    (map named-square
         (filter
           (partial movable? position)
           (list
             [(+ r 2) (inc c)]
             [(inc r) (+ c 2)]
             [(dec r) (+ c 2)]
             [(- r 2) (inc c)]
             [(- r 2) (dec c)]
             [(dec r) (- c 2)]
             [(inc r) (- c 2)]
             [(+ r 2) (dec c)])))))

(defn pawn-moves
  ([square position color r c]
    (def diff (if (white? color) inc dec))
    (def one
      (named-square
        (diff (row square))
        (col square)))
    (def two
      (named-square
        (diff (diff (row square)))
        (col square)))
    (def left (named-square (diff (row square)) (dec c)))
    (def right (named-square (diff (row square)) (inc c)))
    (remove nil?
            (flatten
              (list
                (when
                  (not (occupied? position one))
                  (list one (when (not (occupied? position two)) two)))
                (when (enemy-on? position color (rc left)) left)
                (when (enemy-on? position color (rc right)) right))))))

(defn ekm
  ([square position color r c]
    '()))

(def moves-map
  {\K ekm
   \Q queen-moves
   \R rook-moves
   \B bishop-moves
   \N knight-moves
   \P pawn-moves
   \k ekm
   \q queen-moves
   \r rook-moves
   \b bishop-moves
   \n knight-moves
   \p pawn-moves })

(defn non-king-moves
  ([square position color r c]
    (let [piece (position square)]
      (if (= (piece-color piece) color)
        ((moves-map (position square)) square position color r c)
        '())))
  ([position color]
    (mapcat
      (fn [[k v]] (non-king-moves position color k))
      position))
  ([position color square]
    {:pre [(not (nil? square))]}
    (let [piece (position square)
          r (row square)
          c (col square)]
      (partition
        2
        (interleave
          (iterate identity square)
          (seq (non-king-moves square position color r c)))))))

(defn switch-color [color]
  (or
    (when (= color "white") "black")
    (when (= color "black") "white")))

(defn in-check?
  [position color]
  (let [piece (if (white? color) \K \k)]
    (reduce
      (fn
        ([r n]
          (or r (= (position (last n)) piece)))
        ([] false))
      false
      (non-king-moves
        position
        (switch-color color)))))

(defn move [position [source target]]
  {:pre [(not (nil? source)) (not (nil? target))] }
  (if (contains? position source)
    (let [piece (get position source)]
      (dissoc (assoc position target piece) source))
    (throw
      (ex-info
        (str "Piece does not exist at " source)
        {:type :invalid-move }))))

(defn promote
  [position square]
  (let [piece (if (white? (piece-color (position square))) \Q \q)]
    (assoc position square piece)))

(defn move-with-effects
  [position [source target]]
  (let [piece (get position source)]
      (let [result (dissoc (assoc position target piece) source)]
        (cond
          (and (= \K piece) (= "e1" source ) (= "c1" target)) (move result ["a1" "d1"])
          (and (= \K piece) (= "e1" source ) (= "g1" target)) (move result ["h1" "f1"])
          (and (= \k piece) (= "e8" source ) (= "c8" target)) (move result ["a8" "d8"])
          (and (= \k piece) (= "e8" source ) (= "g8" target)) (move result ["h8" "f8"])
          (and (= \P piece) (= 7 (row target))) (promote result target)
          (and (= \k piece) (= 0 (row target))) (promote result target)
          true result))))

(defn king-moves
  ([square position color r c]
    (flatten
      (list
        (map
          named-square
          (filter
            (partial movable? position)
            (list
              [(inc r) c]
              [(inc r) (inc c)]
              [r (inc c)]
              [(dec r) (inc c)]
              [(dec r) c]
              [(dec r) (dec c)]
              [r (dec c)]
              [(inc r) (dec c)])))
        (if (not (in-check? position color))
          (if
            (white? color)
            (flatten
              (list
                (when
                  (and
                    (= (position "e1") \K)
                    (not (in-check? (move position ["e1" "f1"]) color))
                    (= (position "h1") \R))
                  "g1")
                (when
                  (and
                    (= (position "e1") \K)
                    (not (in-check? (move position ["e1" "d1"]) color))
                    (= (position "a1") \R))
                  "c1")))
            (flatten
              (list
                (when
                  (and
                    (= (position "e8") \k)
                    (not (in-check? (move position ["e8" "f8"]) color))
                    (= (position "h8") \r))
                  "g8")
                (when
                  (and
                    (= (position "e8") \k)
                    (not (in-check? (move position ["e8" "d8"]) color))
                    (= (position "a8") \r))
                  "c8")))))))))

(defn piece-moves
  [square position color]
  (let [piece (position square)
        r (row square)
        c (col square)]
    (if (= (piece-color piece) color)
      (if (is-king? piece)
        (king-moves square position color r c)
        (non-king-moves square position color r c)))))

(defn moves
  ([position color]
    (mapcat
      (fn [[k v]] (moves position color k))
      position))
  ([position color square]
    (partition
      2
      (interleave
        (iterate identity square)
        (seq (piece-moves square position color))))))

(defn legal-move?
  [position color themove]
  (and
    (not (nil? themove))
    (not (nil? (first themove)))
    (not (nil? (last themove)))
    (not (in-check? (move position themove) color))))

(defn legal-moves
  [position color]
  (let [all-moves (moves position color)]
    (filter
      (partial legal-move? position color)
      all-moves)))

(defn in-checkmate?
  [position color]
  (and
    (in-check? position color)
    (empty? (legal-moves position color))))

(defn in-stalemate?
  [position color]
  (and
    (not (in-check? position color))
    (empty? (legal-moves position color))))

(defn print-position [position]
  (apply str
         (map
           (fn [r]
             (str
               (apply str
                      (map
                        (fn [f]
                          (def square (str (col-name f) (inc r)))
                          (if
                            (contains? position square)
                            (position square)
                            (str " ")))
                        (range 8)))
               "\n"))
           (reverse (range 8)))))

(defn ppb [position] (print (print-position position)))

(def start-fen
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defn position-to-array [position]
  (apply vector
    (map
      (fn [r]
        (apply vector
          (map
            (fn [c]
              (let [square (str (col-name c) (inc r))]
                {:piece (when (contains? position square) (keyword (str (position square)))) :row r :col c}))
            (range 8))))
      (reverse (range 8)))))

(defn print-row [row]
  (let [res
    (reduce
      (fn [[acc prev] x]
        (let [piece (if (:piece x) (name (:piece x)) nil)]
        (if
          (nil? piece)
          [acc (inc prev)]
          [(if (> prev 0)
            (str acc prev piece)
            (str acc piece)) 0])))
      ["" 0]
      row)]
    (str (first res) (when (> (last res) 0) (last res)))))

(defn board-to-fen [board]
  (str
    (str/join "/"
      (map
        print-row
        (position-to-array (:position board))))
    " "
    (print-color (:active-color board))
    " KQkq - 0 1"))

(def piece-score
  { :k 0 :K 0
    :q 9 :Q 9
    :r 5 :R 5
    :b 3 :B 3
    :n 3 :N 3
    :p 1 :P 1 })

(defn material-score [position color]
  (reduce
    (fn [s [k v]]
      (+ s
         (* (if (= (piece-color k) color)
              (piece-score (keyword (str k)))
              0)
            v)))
    0
    (frequencies (vals position))))

(defn score-position [position color]
  (if (in-checkmate? position color)
    Infinity
    (material-score position color)))

(defn best-score [moves]
  (reduce
    (fn [best curr]
      (let [bs (second best)
            cs (second curr)]
        (if (< cs bs)
          best
          curr)))
    moves))

(defn depth-score [position color depth]
  (if (> depth 0)
    (best-score
      (map (fn [move]
             (let [[history score]
                   (depth-score
                     (move-with-effects position move)
                     (switch-color color)
                     (dec depth))]
               [(conj history move) (- score)]))
           (legal-moves position color)))
    ['() (score-position position color)]))

(defonce app-state
  (let [board (parse-fen start-fen)]
  (atom {:text "Hello Chestnut!"
         :move-input ""
         :message ""
         :result nil
         :move-history '()
         :fen start-fen
         :legal-moves (set (legal-moves (:position board) (:active-color board)))
         :position-array (position-to-array (:position board)) })))

(defn square-class [data cursor]
  (let [over (:over cursor)
        select (:selected cursor)
        row (:row data)
        col (:col data)
        selected (:selected data)
        hover (:over data)
        classes (cond
                  selected ["light-square-highlight", "dark-square-highlight"]
                  hover ["light-square-hover", "dark-square-hover"]
                  :else ["light-square", "dark-square"])]
    (get classes (mod (+ row col) 2))))

(defn parse-move [move]
  (str/split (str move) "-"))

(defn make-move [move cursor]
  (when move
    (let [board (parse-fen (:fen cursor))
          curr-legal-moves (:legal-moves cursor)]
      (if
        (contains? curr-legal-moves move)
        (let [new-board (assoc board :position (move-with-effects (:position board) move) :active-color (switch-color (:active-color board)))
              new-fen (board-to-fen new-board)]
          (om/update! cursor :fen new-fen)
          (om/update! cursor :message "")
          (om/transact! cursor :move-history (fn [history] (concat history (list (str (first move) "-" (last move))))))
          (let [new-legal-moves (set (legal-moves (:position new-board) (:active-color new-board))) ]
            (om/update! cursor :legal-moves new-legal-moves)
            (when (empty? new-legal-moves) (om/update! cursor :result "Checkmate")))
          (om/update! cursor :position-array (position-to-array (:position (parse-fen new-fen)))))
        (do
          (om/update! cursor :position-array (position-to-array (:position board)))
          (om/update! cursor :message (str "Illegal move " (first move) "-" (last move))))))))

(defn make-move-button [data owner]
  (let [move (-> (om/get-node owner "move-input")
                        .-value
                        parse-move)]
    (make-move move data)))

(defn handle-square-click [data owner cursor]
  (let [previous (:selected cursor)
        current [(:row data) (:col data)]]
    (if previous
      (do
        (make-move [(named-square previous) (named-square current)] cursor)
        (om/update! cursor :selected nil))
      (do
        (om/update! data :selected true)
        (om/update! cursor :selected current)))
    (om/refresh! owner)))

(defn square-view [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (let [cursor (om/root-cursor app-state)]
        (dom/div
          #js {:className (square-class data cursor)
               :style #js { :width 49 :height 49 }
               :onMouseOver (fn [event]
                              (om/update! data :over true))
               :onMouseOut (fn [event]
                             (om/update! data :over false))
               :onClick #(handle-square-click data owner cursor) }
          (when (:piece data)
            (dom/img
              #js {:src (str "img/" (name (:piece data)) ".svg")
                   :style #js {:width "49px" :height "49px"} })))))))

(defn row-view [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "row"}
        (apply dom/div nil (om/build-all square-view data))))))

(defn handle-change [data owner]
  (let [board (-> (om/get-node owner "fen")
                      .-value
                      parse-fen
                      (get :position)
                      position-to-array)]
    (om/update! data :board board)))

(defn legal-move-view [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/li nil (str data)))))

(defn move-history-view [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/li nil (str (first data) " " (last data))))))

(defn handle-move-input-change [e owner {:keys [text]}]
  (om/set-state! owner :move-input (.. e -target -value)))

(defn board-view [data owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (let [position-array (position-to-array (:position (parse-fen (:fen data))))]
        (dom/div nil
        (dom/div #js {:className "board"
                      :style #js {:width "392px"
                                  :height "392px"
                                  :float "left"
                                  :border "2px solid #000000"
                                  :boxSizing "content-box"}}
          (apply dom/div nil (om/build-all row-view (:position-array data))))
        (dom/div nil (str (str/capitalize (:active-color (parse-fen (:fen data)))) " to move"))
        (dom/div nil (:message data))
        (dom/div nil (:result data))
        (dom/div nil nil)
        (dom/div #js { :style #js { :float "left" } }
                 (apply dom/ol nil (om/build-all move-history-view (partition 2 2 '("") (:move-history data)))))
        )))))

(defn main []
  (om/root board-view app-state
    {:target (. js/document (getElementById "app"))}))
