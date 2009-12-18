(defpackage #:clan9
  (:use :cl)
  (:export #:demo1))
(in-package #:clan9)

(defvar *objects* nil) ;all object in 2d space
;; Each object has coordinats
(defstruct object
  x y)

(defun distance (j k)
  "Euclidean distance between object j and k returned"
  (sqrt (+ (expt (- (object-x j) (object-x k)) 2)
	   (expt (- (object-y j) (object-y k)) 2))))

(defun draw-link (j k color)
  "Draw link if objects closer enough"
  (if (> 50 (distance j k))
      (sdl:draw-line-*
       (object-x j) (object-y j) (object-x k) (object-y k)
       :surface sdl:*default-surface* :color color)))

(defun draw-space-objects ()
  "Crawl a list of all object and draw each one"
  (dolist (o *objects*)
    (dolist (a *objects*)				;try to link each object with each another
      (draw-link o a sdl:*cyan*)))		;I know, this is simple but so stupid.
  (dolist (o *objects*)
    (sdl:draw-filled-circle-*			;draw each object
     (object-x o) (object-y o) 9 :surface sdl:*default-surface* :color sdl:*green*)))

(defun draw-cursor (x y)
  ;; Link no objects near by
  (dolist (o *objects*)
    (draw-link o (make-object :x x :y y) sdl:*yellow*))
  ;; Item position
  (sdl:draw-filled-circle-* x y 10 :surface sdl:*default-surface* :color sdl:*black*)
  (sdl:draw-circle-* x y 10 :surface sdl:*default-surface* :color sdl:*red*)
  ;; Distance circle
  (sdl:draw-circle-* x y 40 :surface sdl:*default-surface* :color sdl:*white* :aa t))

(defun demo1 ()
  "Our first SDL demo, draw objects in 2d space"
  (let ((mouse-x 0) (mouse-y 0))
    (sdl:with-init ()
	  (setf *objects* nil)
	  (push (make-object :x 160 :y 120) *objects*) ;create first object in center
	  (sdl:window 320 240 :title-caption "clan9")
	  (setf (sdl:frame-rate) 30) ;; can't tell if that improves the feeling of scene
	  (sdl:update-display)
	  (sdl:with-events ()
		(:quit-event () t)
		(:mouse-motion-event (:X x :Y y)
				     (setf mouse-x x
					   mouse-y y))
		(:mouse-button-up-event (:X x :Y y)
					(push (make-object :x x :y y) *objects*)) ;create object on click
		(:video-expose-event ()
				     (sdl:update-display))
		(:idle ()
		       (sdl:clear-display sdl:*black*)
		       (draw-space-objects)
		       (draw-cursor mouse-x mouse-y)
		       (sdl:update-display))))))

;; TODO Objects crossing workaround.
;;      Don't draw link more than once
;;      Don't redraw world each frame
;;      Rewrite in OOP terms, srtuct->class
