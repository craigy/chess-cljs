(ns chess-cljs.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defonce app-state 
  (atom {:text "Hello Chestnut!"
         :board [[{:piece :r :row 0 :col 0} {:piece :n :row 0 :col 1} {:piece :b :row 0 :col 2} {:piece :q :row 0 :col 3} {:piece :k :row 0 :col 4} {:piece :b :row 0 :col 5} {:piece :n :row 0 :col 6} {:piece :r :row 0 :col 7}]
                 [{:piece :p :row 1 :col 0} {:piece :p :row 1 :col 1} {:piece :p :row 1 :col 2} {:piece :p :row 1 :col 3} {:piece :p :row 1 :col 4} {:piece :p :row 1 :col 5} {:piece :p :row 1 :col 6} {:piece :p :row 1 :col 7}]
                 [{:piece nil :row 2 :col 0} {:piece nil :row 2 :col 1} {:piece nil :row 2 :col 2} {:piece nil :row 2 :col 3} {:piece nil :row 2 :col 4} {:piece nil :row 2 :col 5} {:piece nil :row 2 :col 6} {:piece nil :row 2 :col 7}]
                 [{:piece nil :row 3 :col 0} {:piece nil :row 3 :col 1} {:piece nil :row 3 :col 2} {:piece nil :row 3 :col 3} {:piece nil :row 3 :col 4} {:piece nil :row 3 :col 5} {:piece nil :row 3 :col 6} {:piece nil :row 3 :col 7}]
                 [{:piece nil :row 4 :col 0} {:piece nil :row 4 :col 1} {:piece nil :row 4 :col 2} {:piece nil :row 4 :col 3} {:piece nil :row 4 :col 4} {:piece nil :row 4 :col 5} {:piece nil :row 4 :col 6} {:piece nil :row 4 :col 7}]
                 [{:piece nil :row 5 :col 0} {:piece nil :row 5 :col 1} {:piece nil :row 5 :col 2} {:piece nil :row 5 :col 3} {:piece nil :row 5 :col 4} {:piece nil :row 5 :col 5} {:piece nil :row 5 :col 6} {:piece nil :row 5 :col 7}]
                 [{:piece :P :row 6 :col 0} {:piece :P :row 6 :col 1} {:piece :P :row 6 :col 2} {:piece :P :row 6 :col 3} {:piece :P :row 6 :col 4} {:piece :P :row 6 :col 5} {:piece :P :row 6 :col 6} {:piece :P :row 6 :col 7}]
                 [{:piece :R :row 7 :col 0} {:piece :N :row 7 :col 1} {:piece :B :row 7 :col 2} {:piece :Q :row 7 :col 3} {:piece :K :row 7 :col 4} {:piece :B :row 7 :col 5} {:piece :N :row 7 :col 6} {:piece :R :row 7 :col 7}]]}))

(defn square-color [arg]
  (let [colors ["#B58863" "#F0D9B5"]
        row (:row arg)
        col (:col arg)]
    (get colors (mod (+ row col) 2))))

(defn square-view [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/div 
        #js {:className "square" 
             :style #js {:float "left" 
                         :position "relative" 
                         :background-color (square-color data)
                         :width 49 
                         :height 49}}
        (when (:piece data) (dom/img #js {:src (str "img/" (name (:piece data)) ".svg")
                                          :style #js {:width "49px" :height "49px"}}))))))

(defn row-view [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "row"} 
        (apply dom/div nil (om/build-all square-view data))))))

(defn board-view [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "board" 
                    :style #js {:float "left"
                                :border "2px solid #000000" 
                                :box-sizing "content-box"}}
        (apply dom/div nil (om/build-all row-view (:board data)))))))

(defn main []
  (om/root board-view app-state
    {:target (. js/document (getElementById "app"))}))
