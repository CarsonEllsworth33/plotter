(defpackage :plotter
  (:use :usocket :cl #:claw-raylib)
  (:documentation "Plotting software that takes in data from sockets.")
  (:export :main))