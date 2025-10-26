(in-package :plotter)

(defconstant +screen-width+ 1000)
(defconstant +screen-height+ 1000)
(defconstant +world-width+ 10000)
(defconstant +world-height+ 10000)

(defun clamp (clamp-val min-val max-val)
  (setf clamp-val (min clamp-val max-val))
  (setf clamp-val (max clamp-val min-val)))

(defun world-space (x y)
  (values world-x world-y))

(defun world-space-center ()
  (let ((x-world-center (/ +world-width+ 2))
        (y-world-center (/ +world-height+ 2)))
    (vector x-world-center y-world-center)))

(defun draw-grid-lines (spacing thickness)
  (declare (ignore thickness))
  (loop :for pos-x :from spacing :below +screen-width+ :by spacing
        :do (raylib:draw-line pos-x 0 pos-x +screen-height+ raylib:+black+))
  (loop :for pos-y :from spacing :below +screen-height+ :by spacing
        :do (raylib:draw-line 0 pos-y +screen-width+ pos-y raylib:+black+)))

(defun plot-xy (function &optional (x-bound-min (- +world-width+)) (x-bound-max +world-height+))
  (loop :for x-in :from x-bound-min :upto x-bound-max :do
           (raylib:draw-circle (floor x-in) (floor  (+ (* (* -1 (funcall function x-in)) 100) (/ +screen-height+ 2))) 20.0 raylib:+red+))
  )

(defun graph-function ()
  (plot-xy #'(lambda (x) (* 1 (sin (* x 0.05))))))

(defun handle-input (camera)
  (cond ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :a))
         (incf (raylib:vector2-x (raylib:camera-2d-offset camera)) 5.0))
        ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :d))
         (incf (raylib:vector2-x (raylib:camera-2d-offset camera)) -5.0)))
  (cond ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :w))
         (incf (raylib:vector2-y (raylib:camera-2d-offset camera)) 5.0))
        ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :s))
         (incf (raylib:vector2-y (raylib:camera-2d-offset camera)) -5.0))))

(defun main ()
  (raylib:with-window ("plotter" (+screen-width+ +screen-height+))
    (let ((camera (raylib:make-camera-2d :offset (raylib:make-vector2 :x (float (/ +screen-width+ 2))
                                                                      :y (float (/ +screen-height+ 2)))
                                         :target (raylib:make-vector2 :x (float (/ +world-width+ 2))
                                                                      :y (float (/ +world-height+ 2)))
                                         :rotation 0.0
                                         :zoom 1.0)))
      (raylib:set-target-fps 60)
      (loop :until (raylib:window-should-close) :do 
               (incf (raylib:camera-2d-zoom camera) (* (raylib:get-mouse-wheel-move) 0.05))
               (setf (raylib:camera-2d-zoom camera) (clamp (raylib:camera-2d-zoom camera) 0.1 3.0))
               (handle-input camera)
               (raylib:with-drawing
                 (raylib:clear-background raylib:+white+)
                 (raylib:with-mode-2d camera
                   (let ((screen-pos (raylib:camera-position camera))) ;; vector 3
                     (raylib:draw-circle (floor (raylib:vector3-x screen-pos)) 
                                         (floor (raylib:vector3-y screen-pos)) 
                                         200.0 
                                         raylib:+pink+)) ;; This is a vector 3
                   (draw-grid-lines 50 0)
                   (graph-function)
                   ))))))
