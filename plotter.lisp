(in-package :plotter)

(defconstant +screen-width+ 1000)
(defconstant +screen-height+ 1000)
(defconstant +world-width+ 10000)
(defconstant +world-height+ 10000)

(defun clamp (clamp-val min-val max-val)
  (setf clamp-val (min clamp-val max-val))
  (setf clamp-val (max clamp-val min-val)))

(defun world-space-center ()
  (let ((x-world-center (/ +world-width+ 2))
        (y-world-center (/ +world-height+ 2)))
    (vector x-world-center y-world-center)))

(defun vector-to-raylib-vector2 (vector)
  (raylib:make-vector2 :x (first vector) :y (second vector)))

(defun draw-grid-lines (start-xy spacing)
  (loop :for pos-x :from start-xy :below +screen-width+ :by spacing
        :do (raylib:draw-line pos-x start-xy pos-x +screen-height+ raylib:+black+))
  (loop :for pos-y :from start-xy :below +screen-height+ :by spacing
        :do (raylib:draw-line start-xy pos-y +screen-width+ pos-y raylib:+black+)))

(defun plot-xy (function &optional (x-bound-min (- +world-width+)) (x-bound-max +world-height+))
  (loop :for x-in :from x-bound-min :upto x-bound-max :do
           (raylib:draw-circle (floor x-in) (floor  (+ (* (* -1 (funcall function x-in)) 100) (/ +screen-height+ 2))) 20.0 raylib:+red+)))

(defun camera-width-height (camera-zoom)
  (values (/ +screen-width+ camera-zoom) (/ +screen-height+ camera-zoom)))

(defun camera-top-left-xy (camera)
  (let ((target (raylib:camera-2d-target camera))
        (zoom (raylib:camera-2d-zoom camera)))
    (multiple-value-bind (width height) (camera-width-height zoom)
      (let ((width/2 (/ width 2))
            (height/2 (/ height 2)))
        (values (- (raylib:vector2-x target) width/2) (- (raylib:vector2-y target) height/2))))))

(defun graph-function ()
  (plot-xy #'(lambda (x) (* 1 (sin (* x 0.05))))))

(defun handle-input (camera)
  (let ((cam-speed 10.0))
    (cond ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :a))
           (decf (raylib:vector2-x (raylib:camera-2d-target camera)) cam-speed))
          ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :d))
           (incf (raylib:vector2-x (raylib:camera-2d-target camera)) cam-speed)))
    (cond ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :w))
           (decf (raylib:vector2-y (raylib:camera-2d-target camera)) cam-speed))
          ((raylib:is-key-down #.(cffi:foreign-enum-value 'raylib:keyboard-key :s))
           (incf (raylib:vector2-y (raylib:camera-2d-target camera)) cam-speed)))))

(defun main ()
  (raylib:with-window ("plotter" (+screen-width+ +screen-height+))
    (let ((camera (raylib:make-camera-2d :offset (raylib:make-vector2 :x (float (/ +screen-width+ 2))
                                                                      :y (float (/ +screen-height+ 2)))
                                         :target (raylib:make-vector2 :x 0.0
                                                                      :y 0.0)
                                         :rotation 0.0
                                         :zoom 1.0)))
      (raylib:set-target-fps 60)
      (let ((prev-pos nil))
        (loop :until (raylib:window-should-close) :do 
                 (incf (raylib:camera-2d-zoom camera) (* (raylib:get-mouse-wheel-move) 0.05))
                 (setf (raylib:camera-2d-zoom camera) (clamp (raylib:camera-2d-zoom camera) 0.1 3.0))
                 (handle-input camera)
                 (raylib:with-drawing
                   (raylib:clear-background raylib:+white+)
                   (raylib:draw-text (format nil "Zoom: ~A" (raylib:camera-2d-zoom camera)) 20 20 30 raylib:+red+)
                   (multiple-value-bind (width height) (camera-top-left-xy camera)
                     (raylib:draw-text (format nil "TL Cam X: ~A~%TL Cam Y: ~A" width height) 
                                       (- +screen-width+ 300) 20 30 raylib:+red+)
                     (raylib:with-mode-2d camera
                       (draw-grid-lines (floor width) 50)
                       (let ((screen-pos (raylib:camera-position camera))) ;; vector 3
                         (raylib:draw-circle 0 
                                             0 
                                             200.0 
                                             raylib:+pink+)
                         (setf prev-pos screen-pos)) ;; This is a vector 3
                       (graph-function)
                       ))))))))
