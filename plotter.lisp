(in-package :plotter)

(defconstant +screen-width+ 1000)
(defconstant +screen-height+ 1000)
(defconstant +world-width+ 10000)
(defconstant +world-height+ 10000)

(defun clamp (clamp-val min-val max-val)
  (setf clamp-val (min clamp-val max-val))
  (setf clamp-val (max clamp-val min-val)))

(defun plot-xy (function &key (x-bound-min (- +world-width+)) (x-bound-max +world-height+) (plot-color raylib:+red+) (scaling t))
  "This is the core function of plotter used to draw any function onto the screen.
   ARGUMENTS: FINISH THIS NEXT"
  (loop :for x-in :from x-bound-min :upto x-bound-max :do
           (cond (scaling (raylib:draw-circle (floor x-in) 
                                              (floor  (* (* -1 (funcall function (* x-in 0.02))) 50)) 
                                                8.0 
                                                plot-color))
                 (t (raylib:draw-circle (floor x-in) 
                                        (floor  (* (* -1 (funcall function x-in)) 50)) 
                                        8.0 
                                        plot-color)))))

(defun camera-width-height (camera)
  (let ((camera-zoom (raylib:camera-2d-zoom camera)))
    (values (/ +screen-width+ camera-zoom) (/ +screen-height+ camera-zoom))))

(defun camera-top-left-xy (camera)
  (let ((target (raylib:camera-2d-target camera)))
    (multiple-value-bind (width height) (camera-width-height camera)
      (let ((width/2 (/ width 2))
            (height/2 (/ height 2)))
        (values (- (raylib:vector2-x target) width/2) (- (raylib:vector2-y target) height/2))))))

(defun graph-function (camera)
  "This function passes the x-min and x-max of the screen so that minimal drawing is taking place"
  (multiple-value-bind (tl-x tl-y) (camera-top-left-xy camera)
    (declare (ignore tl-y))
    (multiple-value-bind (br-x br-y) (camera-width-height camera)
      (declare (ignore br-y))
      (plot-xy #'(lambda (x) (* 1 (sin x))) :x-bound-min tl-x :x-bound-max (+ tl-x br-x))
      (plot-xy #'(lambda (x) x ) :x-bound-min tl-x :x-bound-max (+ tl-x br-x) :plot-color raylib:+blue+))))

(defun handle-input (camera)
  (let ((cam-speed (/ 10.0 (raylib:camera-2d-zoom camera))))
    (cond ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :a))
           (decf (raylib:vector2-x (raylib:camera-2d-target camera)) cam-speed))
          ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :d))
           (incf (raylib:vector2-x (raylib:camera-2d-target camera)) cam-speed)))
    (cond ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :w))
           (decf (raylib:vector2-y (raylib:camera-2d-target camera)) cam-speed))
          ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :s))
           (incf (raylib:vector2-y (raylib:camera-2d-target camera)) cam-speed)))))

(defun draw-boarder (thickness)
  (raylib:draw-rectangle 0 0 +screen-width+ thickness raylib:+darkblue+)
  (raylib:draw-rectangle 0 (- +screen-height+ thickness) +screen-width+ thickness raylib:+darkblue+)
  (raylib:draw-rectangle 0 thickness thickness (- +screen-height+ thickness)  raylib:+darkblue+)
  (raylib:draw-rectangle (- +screen-height+ thickness) thickness thickness (- +screen-height+ thickness)  raylib:+darkblue+))

(defun draw-text (width height tl-x tl-y)
  (raylib:draw-text (format nil "TL Cam X: ~A TL Cam Y: ~A" tl-x tl-y) 
                    10 (- +screen-height+ 30) 20 raylib:+raywhite+)
  (raylib:draw-text (format nil "Cam Width: ~A Cam Height: ~A" width height) 
                    (floor (/ +screen-width+ 2)) (- +screen-height+ 30) 20 raylib:+raywhite+))

(defun get-grid-spaced-world (camera spacing)
  (multiple-value-bind (tl-x tl-y) (camera-top-left-xy camera)
    (list (mod tl-x spacing)
          (mod tl-y spacing))))

(defun draw-grid-lines (start-xy end-xy spacing)
  (let ((tl-x (first start-xy))
        (tl-y (second start-xy))
        (br-x (first end-xy))
        (br-y (second end-xy)))
    (loop :for pos-x :from tl-x :below (+ tl-x br-x) :by spacing
          :do (raylib:draw-line pos-x tl-y pos-x (+ tl-y br-y) raylib:+black+))
    (loop :for pos-y :from tl-y :below (+ tl-y br-y) :by spacing
          :do (raylib:draw-line tl-x pos-y (+ tl-x br-x) pos-y raylib:+black+))))

(defun main ()
  (raylib:with-window ("plotter" (+screen-width+ +screen-height+))
    (let ((camera (raylib:make-camera-2d :offset (raylib:make-vector2 :x (float (/ +screen-width+ 2))
                                                                      :y (float (/ +screen-height+ 2)))
                                         :target (raylib:make-vector2 :x 0.0
                                                                      :y 0.0)
                                         :rotation 0.0
                                         :zoom 1.0)))
      (raylib:set-target-fps 60)
      (loop :until (raylib:window-should-close) :do 
               (incf (raylib:camera-2d-zoom camera) (* (raylib:get-mouse-wheel-move) 0.05))
               (setf (raylib:camera-2d-zoom camera) (clamp (raylib:camera-2d-zoom camera) 0.1 3.0))
               (handle-input camera)
               (raylib:with-drawing
                 (raylib:clear-background raylib:+white+)
                 (multiple-value-bind (tl-x tl-y) (camera-top-left-xy camera)
                   (raylib:with-mode-2d camera
                     (let ((spacing 30))
                       (multiple-value-bind (tl-x tl-y) (camera-top-left-xy camera)
                         (let* ((xy-ret (get-grid-spaced-world camera spacing))
                                (x (floor (first xy-ret)))
                                (y (floor (second xy-ret))))
                           (multiple-value-bind (width height) (camera-width-height camera)
                             (draw-grid-lines (list (floor (- tl-x x)) (floor (- tl-y y)))
                                              (list (floor width) (floor height))
                                              spacing)))))
                     (raylib:draw-circle 0 0 80.0 raylib:+gold+)
                     (graph-function camera))
                   (draw-boarder 40)
                   (raylib:draw-text (format nil "Zoom: ~A" (raylib:camera-2d-zoom camera)) 10 20 20 raylib:+raywhite+)
                   (multiple-value-bind (width height) (camera-width-height camera)
                     (draw-text width height tl-x tl-y)
                     )))))))

(defun repl-main ()
  (main))
