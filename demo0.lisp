;; Modified Gearcs example from CL-GLUT-EXAMPLES
(defpackage #:clan9
  (:use #:cl)
  (:export #:run-demo))

(in-package #:clan9)

(defconstant +pif+ (coerce pi 'single-float))

(defun draw-gear (inner-radius outer-radius width n-teeth tooth-depth)
  "Draw a gear."
  (declare (single-float inner-radius outer-radius width tooth-depth)
           (fixnum n-teeth))
  (let ((r0 inner-radius)
        (r1 (- outer-radius (/ tooth-depth 2.0)))
        (r2 (+ outer-radius (/ tooth-depth 2.0)))
        (da (/ (* 2.0 +pif+) n-teeth 4.0)))
    (gl:shade-model :flat)
    (gl:normal 0 0 1)
    ;; Draw front face.
    (gl:with-primitives :quad-strip
      (dotimes (i (1+ n-teeth))
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* width 0.5)))))
    ;; Draw front sides of teeth.
    (gl:with-primitives :quads
      (dotimes (i n-teeth)
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
          (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                     (* width 0.5))
          (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                     (* r2 (sin (+ angle (* 2 da))))
                     (* width 0.5))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* width 0.5)))))
    (gl:normal 0 0 -1)
    ;; Draw back face.
    (gl:with-primitives :quad-strip
      (dotimes (i (1+ n-teeth))
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width -0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                       (* r1 (sin (+ angle (* 3 da))))
                       (* width -0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5)))))
    ;; Draw back sides of teeth.
    (gl:with-primitives :quads
      (dotimes (i n-teeth)
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* (- width) 0.5))
          (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                     (* r2 (sin (+ angle (* 2 da))))
                     (* (- width) 0.5))
          (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                     (* (- width) 0.5))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5)))))
    ;; Draw outward faces of teeth.
    (gl:with-primitives :quad-strip
      (dotimes (i n-teeth)
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5))
          (let* ((u (- (* r2 (cos (+ angle da))) (* r1 (cos angle))))
                 (v (- (* r2 (sin (+ angle da))) (* r1 (sin angle))))
                 (len (sqrt (+ (* u u) (* v v)))))
            (setq u (/ u len))
            (setq v (/ u len))
            (gl:normal v (- u) 0.0)
            (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                       (* width 0.5))
            (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                       (* (- width) 0.5))
            (gl:normal (cos angle) (sin angle) 0.0)
            (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                       (* r2 (sin (+ angle (* 2 da))))
                       (* width 0.5))
            (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                       (* r2 (sin (+ angle (* 2 da))))
                       (* (- width) 0.5))
            (setq u (- (* r1 (cos (+ angle (* 3 da))))
                       (* r2 (cos (+ angle (* 2 da))))))
            (setq v (- (* r1 (sin (+ angle (* 3 da))))
                       (* r2 (sin (+ angle (* 2 da))))))
            (gl:normal v (- u) 0.0)
            (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                       (* r1 (sin (+ angle (* 3 da))))
                       (* width 0.5))
            (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                       (* r1 (sin (+ angle (* 3 da))))
                       (* (- width) 0.5))
            (gl:normal (cos angle) (sin angle) 0.0))))
      (gl:vertex (* r1 (cos 0)) (* r1 (sin 0)) (* width 0.5))
      (gl:vertex (* r1 (cos 0)) (* r1 (sin 0)) (* (- width) 0.5)))
    ;; Draw inside radius cylinder.
    (gl:shade-model :smooth)
    (gl:with-primitives :quad-strip
      (dotimes (i (1+ n-teeth))
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:normal (- (cos angle)) (- (sin angle)) 0.0)
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* (- width) 0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5)))))))

(defclass demo-window (glut:window)
  ((view-rotx :initform 0.0)
   (view-roty :initform 0.0)
   (view-rotz :initform 0.0)
   gear0
   (angle :initform 0.0)
   (count :initform 1)
   (t0 :initform 0))
  (:default-initargs :title "Demo #0 Window" :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((window demo-window))
  (with-slots (gear0) window
    (gl:light :light0 :position #(5.0 5.0 10.0 0.0))
    (gl:enable :cull-face :lighting :light0 :depth-test)
    ;; gear
    (setq gear0 (gl:gen-lists 1))
    (gl:with-new-list (gear0 :compile)
      (gl:material :front :ambient-and-diffuse #(0.1 0.6 0.1 1.0)) ; color
      (draw-gear 0.6 0.7 0.2 10 0.1))
    (gl:enable :normalize)))

(defmethod glut:display ((window demo-window))
  (with-slots (view-rotx view-roty view-rotz angle gear0)
      window
    (gl:clear :color-buffer :depth-buffer)
    (gl:with-pushed-matrix
      (gl:rotate view-rotx 1 0 0)
      (gl:rotate view-roty 0 1 0)
      (gl:rotate view-rotz 0 0 1)
      (gl:with-pushed-matrix ; gear
        (gl:translate -1 -1 0)
        (gl:rotate angle 0 0 0.1)
        (gl:call-list gear0)))
    (glut:swap-buffers)))

(defmethod glut:idle ((window demo-window))
  (incf (slot-value window 'angle) 0.1)
  (glut:post-redisplay))

(defmethod glut:keyboard ((window demo-window) key x y)
  (declare (ignore x y))
  (case key
    (#\z (incf (slot-value window 'view-rotz) 5.0)
         (glut:post-redisplay))
    (#\Z (decf (slot-value window 'view-rotz) 5.0)
         (glut:post-redisplay))
    (#\Esc (glut:destroy-current-window))))

(defmethod glut:special ((window demo-window) special-key x y)
  (declare (ignore x y))
  (with-slots (view-rotx view-roty) window
    (case special-key
      (:key-up (incf view-rotx 5.0))
      (:key-down (decf view-rotx 5.0))
      (:key-left (incf view-roty 5.0))
      (:key-right (decf view-roty 5.0)))
    (glut:post-redisplay)))

(defmethod glut:reshape ((w demo-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((h (/ height width)))
    (gl:frustum -1 1 (- h) h 5 60))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate 0 0 -40))

(defmethod glut:visibility ((w demo-window) state)
  (case state
    (:visible (glut:enable-event w :idle))
    (t (glut:disable-event w :idle))))

(defmethod glut:mouse ((w demo-window) button state x y)
  (when (and (eq button :left-button) (eq state :down))
	(format *terminal-io* "click X: ~D Y: ~D ~%"
			x y)))

(defmethod glut:passive-motion  ((w demo-window) x y)
  (format *terminal-io* "move X: ~D Y: ~D ~%" x y))

(defmethod glut:motion ((w demo-window) x y)
  (format *terminal-io* "pick X: ~D Y: ~D ~%" x y))


(defun demo0 ()
  (glut:display-window (make-instance 'demo-window)))

(defun run-demo ()
  (let ((glut:*run-main-loop-after-display* nil))
	(demo0)
	(glut:main-loop)))