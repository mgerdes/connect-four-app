(ns connect-four.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType])
  (:import goog.History))

;; -------------------------
;; Views

(defn num-pieces-in-col [col board]
  (count (filter #(= (% :col) col) board)))

(defn insert [color col board]
  (let [row (num-pieces-in-col col board)
        new-piece {:row row, 
                   :col col, 
                   :color color}]
    (conj board new-piece)))

(defn get-piece [row col board]
  (first (filter 
           #(and 
              (= (% :row) row) 
              (= (% :col) col)) 
           board)))

(defn get-piece-color [row col board]
  (:color (get-piece row col board)))

(defn four-connected? [col color board]
  (let [row (dec (num-pieces-in-col col board))
        funcs [[{:row-fn identity, :col-fn inc}, {:row-fn identity, :col-fn dec}]
               [{:row-fn inc, :col-fn identity}, {:row-fn dec, :col-fn identity}]
               [{:row-fn inc, :col-fn inc}, {:row-fn dec, :col-fn dec}]
               [{:row-fn dec, :col-fn inc}, {:row-fn inc, :col-fn dec}]]]
    (some #(>= % 4)
          (for [func funcs]
            (+ -1
               (apply + 
                      (for [f func]
                        (loop [row row col col piece-count 0]
                          (if (= (get-piece-color row col board) color)
                            (recur ((:row-fn f) row) ((:col-fn f) col) (inc piece-count))
                            piece-count)))))))))

(defn all-children [initial-board color max-depth]
  (map (fn [col]
         (let [next-color (if (= :red color) :blue :red)
               board (insert color col initial-board)
               is-winner? (four-connected? col color board) 
               children (if (or (zero? max-depth) is-winner?) 
                          nil
                          (all-children board next-color (dec max-depth)))
               score (if children 
                       (if (= color :red) 
                         (apply min (map #(get-in % [:score :value]) children))
                         (apply max (map #(get-in % [:score :value]) children))) 
                       (if is-winner? (if (= :red color) 1000 -1000) 0))]
           {:board board
            :score {:col col 
                    :value score}
            :children children})) (range 7)))

(def hover-column (atom 0))
(def board (atom []))
(def move-num (atom 0))
(def is-winner? (atom false))

(defn col-full? [col board]
  (> (num-pieces-in-col col board) 6))

(defn valid-input? [col]
  (not (col-full? col @board)))

(defn best-col [board]
  (:col (reduce 
          #(if (> (:value %1) (:value %2)) %1 %2) 
          (filter 
            #(valid-input? (:col %1))
            (map :score (all-children board :red 3))))))

(defn is-visible-indicator [indicator-num]
  (if (= @hover-column indicator-num)
    "visible-indicator"
    "invisible-indicator"))

(defn check-for-winner [col color]
  (when 
    (four-connected? col color @board) 
    (reset! is-winner? true)))

(defn board-html-easy [board-to-draw]
  [:div {:id "board" 
         :class ""}
   [:div {:class "invisible"} (str @hover-column @board)]
   (for [col (range 7)]
     [:div {:class (str "col " "col-" col)
            :on-mouse-over #(swap! hover-column (fn [x] col))
            :on-click #(do
                         (swap! move-num inc) 
                         (swap! board (partial insert :blue col))
                         (check-for-winner col :blue)
                         (let [best-col (best-col @board)]
                           (swap! board (partial insert :red best-col))
                           (check-for-winner best-col :red)))}
      [:div {:class (str "indicator " "indicator-" col " " (is-visible-indicator col))}]
      (for [row (reverse (range 7))]
        [:div {:class
               (str 
                 "piece "
                 (let [piece-color (get-piece-color row col board-to-draw)]
                   (if piece-color
                     (name piece-color)
                     "empty")))}])])])

(defn timer-component []
  (let [seconds-elapsed (atom 0)]
    (fn []
      (js/setTimeout #(swap! seconds-elapsed inc) 1000)
      [:div
       "Seconds Elapsed: " @seconds-elapsed])))

(defn home-page []
  (do
    [:div {:class "center"} [:h2 (str "CONNECT FOUR!")] [:h5 (str "(Against a moderately smart AI)")]
     [:div (board-html-easy @board)]]))

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
      EventType/NAVIGATE
      (fn [event]
        (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app
(defn mount-root []
  (reagent/render-component [current-page] (.getElementById js/document "app")))

(defn init! []
  (hook-browser-navigation!)
  (mount-root))
