(defclass shape () ())

(defclass circle (shape)
  ((radius
    :initarg :radius
    :initform 0)))

(defclass rectangle (shape)
  ((length
    :initarg :length
    :initform 0)
   (breadth
    :initarg :breadth
    :initform 0)))

(defclass triangle (shape)
  ((a
    :initarg :a
    :initform 0)
   (b
    :initarg :b
    :initform 0)
   (c
    :initarg :c
    :initform 0)))

(defgeneric area (shape)
  (:documentation "calculate the area of the given shape."))

(defgeneric perimeter (shape)
  (:documentation "calculate the perimetere of the given shape."))

(defmethod area ((shape circle))
  (* pi
     (slot-value shape 'radius)
     (slot-value shape 'radius)))

(defmethod area ((shape rectangle))
  (* (slot-value shape 'length)
     (slot-value shape 'breadth)))

(defmethod area ((shape triangle))
  (let* ((a (slot-value shape 'a))
	 (b (slot-value shape 'b))
	 (c (slot-value shape 'c))
	 (s (/ (+  a b c) 2)))
    (sqrt (* s (- s a) (- s b) (- s c)))))

(defmethod perimeter ((shape circle))
  (* 2
     pi
     (slot-value shape 'radius)))

(defmethod perimeter ((shape rectangle))
  (* 2 (+ (slot-value shape 'length)
	  (slot-value shape 'breadth))))

(defmethod perimeter ((shape triangle))
  (+ (slot-value shape 'a)
     (slot-value shape 'b)
     (slot-value shape 'c)))

(defun shape-demo ()
  (let ((circle (make-instance 'circle :radius 10))
	(rectangle (make-instance 'rectangle :length 10 :breadth 5))
	(triangle (make-instance 'triangle :a 3 :b 5 :c 7)))
    (format t "Circle, area: ~f, perimeter: ~f~%" (area circle) (perimeter circle))
    (format t "Rectangle, area: ~d, perimeter: ~d~%" (area rectangle) (perimeter rectangle))
    (format t "Triangle, area: ~d, perimeter: ~d~%" (area triangle) (perimeter triangle))))
