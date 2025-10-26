(defpackage  :plotter-system
  (:use :cl))

(in-package :plotter-system)

(asdf:defsystem "plotter" 
  :depends-on ("usocket"
               "bordeaux-threads"
               "claw-raylib")
  :components ((:file "package")
               (:file "plotter")))