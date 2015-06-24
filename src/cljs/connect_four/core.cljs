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

(def hover-column (atom 0))
(def board (atom (insert :blue 6 (insert :blue 0 (insert :blue 0 [])))))

(defn col-off-board? [col]
  (or (> col 6) (< col 0)))

(defn col-full? [col board]
  (> (num-pieces-in-col col board) 7))

(defn invalid-input [col board]
  (or (col-off-board? col) (col-full? col board)))

(defn is-visible-indicator [indicator-num]
  (if (= @hover-column indicator-num)
    "visible-indicator"
    "invisible-indicator"))

(defn board-html-easy [board-to-draw]
  [:div {:id "board"}
   (for [col (range 7)]
     [:div {:class (str "col " "col-" col)
            :on-mouse-over #(swap! hover-column (fn [x] col))
            :on-click #(swap! board (partial insert :red col))}
      [:div {:class (str "indicator " "indicator-" col " " (is-visible-indicator col))}]
      (for [row (reverse (range 7))]
        [:div {:class
               (str 
                 "piece "
                 (let [piece-color (get-piece-color row col board-to-draw)]
                   (if piece-color
                     (name piece-color)
                     "empty")))}])])])

  (defn home-page []
    [:div [:h2 "Welcome to connect-four"]
     [:div [:a {:href "#/about"} "go to about page"]]])

  (defn about-page []
    [:div [:h2 "About connect-four"]
     [:div [:a {:href "#/"} "hello captain im a good what what"]]
     [:div [:a {:href "#/connect-four"} "play connect four"]]
     [:div [:a {:href "#/connect-four-2"} "play connect four2"]] ])

  (defn connect-four []
    (do
      [:div [:h2 (str "THIS IS CONNECT FOUR!" @hover-column)]
       [:div (board-html-easy @board)]]))

  (defn connect-four-2 []
    [:div [:h2 "THIS IS CONNECT FOUR 2!"]])

  (defn current-page []
    [:div [(session/get :current-page)]])

  ;; -------------------------
  ;; Routes
  (secretary/set-config! :prefix "#")

  (secretary/defroute "/" []
    (session/put! :current-page #'home-page))

  (secretary/defroute "/about" []
    (session/put! :current-page #'about-page))

  (secretary/defroute "/connect-four" []
    (session/put! :current-page #'connect-four))

  (secretary/defroute "/connect-four-2" []
    (session/put! :current-page #'connect-four-2))

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
    (reagent/render [current-page] (.getElementById js/document "app")))

  (defn init! []
    (hook-browser-navigation!)
    (mount-root))
