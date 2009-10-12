(import (rnrs)
        (ironscheme clr)
        (ironscheme clr shorthand))

(clr-reference System.Drawing)
(clr-reference System.Windows.Forms)

(clr-using System.Drawing)
(clr-using System.Windows.Forms)

(define dim-board '(90 90))
(define dim-screen '(600 600))
(define dim-scale (map / dim-board dim-screen))

(define-record-type cell 
  (fields 
    (mutable x)
    (mutable y)
    (mutable state)))

(define (render-cell g cell)
  
)
#|

(import '(javax.swing JFrame JPanel)
        '(java.awt Color Graphics)
        '(java.awt.image BufferedImage))

(def dim-board   [ 90   90])
(def dim-screen  [600  600])
(def dim-scale   (vec (map / dim-screen dim-board)))

(defn fmap [f coll] (doall (map f coll)))

(defn render-cell [#^Graphics g cell]
  (let [[state x y] cell
        x  (inc (* x (dim-scale 0)))
        y  (inc (* y (dim-scale 1)))]
    (doto g
      (.setColor (if (= state :dying) Color/GRAY Color/WHITE))
      (.fillRect x y (dec (dim-scale 0)) (dec (dim-scale 1))))))

(defn render [g img bg stage]
  (.setColor bg Color/BLACK)
  (.fillRect bg 0 0 (dim-screen 0) (dim-screen 1))
  (fmap (fn [col]
          (fmap #(when (not= :off (% 0))
                   (render-cell bg %)) col)) stage)
  (.drawImage g img 0 0 nil))

(def board
     (for [x (range (dim-board 0))]
       (for [y (range (dim-board 1))]
         [(if (< 50 (rand-int 100)) :on :off) x y])))

(defn active-neighbors [above [left _ right] below]
  (count
   (filter #(= :on (% 0))
           (concat above [left right] below))))

(defn torus-window [coll]
  (partition 3 1 (concat [(last coll)] coll [(first coll)])))

(defn rules [above current below]
  (let [[self x y]  (second current)]
    (cond
      (= :on    self)                              [:dying x y]
      (= :dying self)                              [:off   x y]
      (= 2 (active-neighbors above current below)) [:on    x y]
      :else                                        [:off   x y])))

(defn step [board]
  (doall
   (pmap (fn [window]
          (apply #(doall (apply map rules %&))
                 (doall (map torus-window window))))
        (torus-window board))))

(defn activity-loop [surface stage]
  (while true
    (swap! stage step)
    (.repaint surface)))

(let [stage (atom board)
      frame (JFrame.)
      img   (BufferedImage. (dim-screen 0) (dim-screen 1) (BufferedImage/TYPE_INT_ARGB))
      bg    (.getGraphics img)
      panel (doto (proxy [JPanel] [] (paint [g] (render g img bg @stage))))]
  (doto frame (.add panel) .pack (.setSize (dim-screen 0) (dim-screen 1)) .show
        (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
  (future (activity-loop panel stage)))

|#
