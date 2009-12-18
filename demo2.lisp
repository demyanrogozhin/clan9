; Objects in Space
(defconstant basis 3 "Standard basis")

(defun point (x y &optional z) "Return 3d vector from 2 of 3 coords, z is 0 by default"
	   (let ((coords (list x y (cond (z z) (t .0)))))
		 (make-array
		  basis :fill-pointer t :element-type 'single-float
		  :initial-contents (loop for i in coords collect
								 (cond ((floatp i) i)	  ; if xyz is float, or
									   ((numberp i) (float i))))))) ; make float

(defun x (p) (aref p 0))				; Return X coord of 'point
(defun y (p) (aref p 1))				; same for Y
(defun z (p) (aref p 2))				; and same for Z

(defun segment-length (a b)				; Distance between two 'points
  (sqrt (+ (expt (- (x a) (x b)) 2)
		   (expt (- (y a) (y b)) 2)
		   (expt (- (z a) (z b)) 2))))

(defun distance (j k)					; Distance between two 'objs
  (- (segment-length (slot-value j 'position) (slot-value k 'position))
	 (slot-value j 'radius)
	 (slot-value k 'radius)))

(defun vect-sum (v1 v2)
  (point (+ (x v1) (x v2)) (+ (y v1) (y v2)) (+ (z v1) (z v2))))
;(type-of (point 1 2)) ;=> (VECTOR SINGLE-FLOAT 3)

(defgeneric animate (obj)
  (:documentation "Move object."))

(defvar *space* nil "Objects holder")

(defclass obj nil
  ((mass :type float :initform 10 :initarg :m)
   (position :type (VECTOR SINGLE-FLOAT 3) :initform (point 0 0) :initarg :p)
   (radius :type float :initform 1 :initarg :r))
  (:documentation "Basic object with fixed position"))

(defmethod initialize-instance
			 :after ((self obj) &key)
		   (block check-cross
			 (dolist (other *space*)
			   (when (> 0 (distance self other))
				 (return-from check-cross)))
			 (push self *space*)))

(defmethod animate ((self obj)) nil)

(defclass fobj (obj)
  ((v :type (VECTOR SINGLE-FLOAT 3) :initform (point 0 0) :initarg :v))
  (:documentation "Flying object"))

(defmethod animate ((self fobj))
  (with-slots (position v) self
  (setf position
		(vect-sum position v))))

(defun animate-objects ()
  (dolist (obj *space*)
	(animate obj)))

(defun draw-space-objects ()
  (dolist (obj *space*)
	(with-slots ((p position) (r radius)) obj
	  (sdl-gfx:draw-aa-circle-*			; draw each object
	   (floor (x p)) (floor (y p)) r :surface sdl:*default-surface* :color sdl:*YELLOW*))))

(defun demo2 ()
  "Our second SDL demo, objects in OOP terms draw 3d space on surface"
  (let ((mouse-x 0) (mouse-y 0) (width 320) (height 240))
    (sdl:with-init ()
	  (sdl:window width height :title-caption "clan9: Demo II")
	  (setf (sdl:frame-rate) 30) ;; can't tell if that improves the feeling of scene
      (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
	  (setf *space* nil)
	  (make-instance 'obj :p (point (/ width 2) (/ height 2)) :r 5 :m 1000) ;create first object in center
	  (make-instance 'fobj :p (point (/ width 20) (/ height 20)) :r 1 :m 10 :v (point .5 .5)) ; Asteroid

	  (sdl:with-events ()
		(:quit-event () t)
		(:mouse-motion-event (:X x :Y y)
							 (setf mouse-x x
								   mouse-y y))
		(:mouse-button-up-event (:X x :Y y) ; object on click, see how it Flow
								(make-instance 'fobj
											   :p (point x y)
											   :r (random 10)
											   :v (point
												   (- (random 2.0) 1)
												   (- (random 2.0) 1))))
		(:video-expose-event ()
							 (sdl:update-display))
		(:idle ()
			   (sdl:clear-display sdl:*black*)
			   (animate-objects)
			   (sdl:with-surface (disp sdl:*default-display*) ;(map (sdl:create-surface 1000 1000))
				 (draw-space-objects))
			   (sdl:update-display))))))
