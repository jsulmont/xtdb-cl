(in-package :xtdb-cl)

;; Define the classes
(defclass shape () ())

(defclass circle (shape)
  ((radius :accessor radius :initarg :radius :initform 0)))

(defclass rectangle (shape)
  ((width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)))

;; Define contexts
(defclass console-context () ())
(defclass gui-context () ())

;; Define the generic function
(defgeneric draw (shape context))

;; Methods for drawing a circle
(defmethod draw ((s circle) (c console-context))
  (format t "Drawing a circle with radius ~A on the console.~%" (radius s)))

(defmethod draw ((s circle) (c gui-context))
  (format t "Drawing a circle with radius ~A on the GUI.~%" (radius s)))

;; Methods for drawing a rectangle
(defmethod draw ((s rectangle) (c console-context))
  (format t "Drawing a rectangle with width ~A and height ~A on the console.~%" (width s) (height s)))

(defmethod draw ((s rectangle) (c gui-context))
  (format t "Drawing a rectangle with width ~A and height ~A on the GUI.~%" (width s) (height s)))

;; Usage
(let ((c (make-instance 'circle :radius 5))
      (r (make-instance 'rectangle :width 10 :height 20))
      (console (make-instance 'console-context))
      (gui (make-instance 'gui-context)))
  (draw c console)  ; Drawing a circle on the console
  (draw r gui))     ; Drawing a rectangle on the GUI
