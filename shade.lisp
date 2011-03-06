;; shader example from freeglut smooth_opengl3.c


(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :asdf)
  (require :cl-opengl)
  (require :cl-glut)
  (require :cl-glu)
  (push "/home/martin/0225/grov/v4l2/" asdf:*central-registry*)
  (require :video)
  )


(defpackage :run
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut))

(in-package :run)


(defclass fenster (window)
  ((cursor-position :accessor cursor-position 
		    :initform (make-array 2 :element-type 'fixnum)
		    :type (simple-array fixnum (2)))
   (draw-func :accessor draw-func
	      :initarg :draw-func
	      :initform #'(lambda ()   
			    (with-primitive :lines
			      (color 1 0 0) (vertex 0 0 0) (vertex 1 0 0)
			      (color 0 1 0) (vertex 0 0 0) (vertex 0 1 0)
			      (color 0 0 1) (vertex 0 0 0) (vertex 0 0 1)))
	      :type function)))

(defgeneric set-view (w &key 2d))
(defmethod set-view ((w fenster) &key (2d t))
      (load-identity)
      (viewport 0 0 (width w) (height w))
      (matrix-mode :projection)
      (load-identity)
      (if 2d
	  (ortho 0 (width w) (height w) 0 -1 1)
	  (progn (glu:perspective 40 (/ (width w) (height w)) 3 100)
		 (glu:look-at 20 30 -5
			      0 0 0
			      0 0 1)))
      (matrix-mode :modelview)
      (load-identity))

(defmethod display ((w fenster))
  (clear :color-buffer-bit :depth-buffer-bit)
  (load-identity)
  
  (funcall (draw-func w))
    
  (swap-buffers)
  (post-redisplay))

(defmethod reshape ((w fenster) x y)
  (setf (width w) x
	(height w) y)
  (set-view w))

(defmethod display-window :before ((w fenster))
  (set-view w))

(defmethod passive-motion ((w fenster) x y)
  (setf (aref (cursor-position w) 0) x
	(aref (cursor-position w) 1) (- (height w) y)))

(defmethod keyboard ((w fenster) key x y)
  (case key
    (#\Esc (destroy-current-window))))

(defmacro with-gui ((w &optional (h w) (x 0) (y 0)) &body body)
  `(display-window 
    (make-instance 'fenster
		   :mode '(:double :rgb :depth)
		   :width ,w :height ,h
		   :pos-x ,x :pos-y ,y 
		   :draw-func #'(lambda ()
				  ,@body))))



(defparameter *vertex-shader*
"void main(){
	gl_Position    = gl_ModelViewProjectionMatrix * gl_Vertex;
	gl_FrontColor  = gl_Color;
	gl_TexCoord[0] = gl_MultiTexCoord0;
}
")

(defparameter *fragment-shader*
  "uniform sampler2D textureImage;
void main()
{
        vec4 q=texture2D( textureImage, gl_TexCoord[0].st );
	gl_FragColor = vec4(q.x-q.z,q.x-q.z,q.x-q.z,255); 
}
")

(defmacro set-mat (m &rest rest)
  (let ((res nil)
	(i 0))
    (dolist (e rest)
      (push `(aref ,m ,i) res)
      (push e res)
      (incf i))
    `(setf ,@(reverse res))))

(defvar *reinitialize* t)
(defvar *update-texture* nil)
(defvar *tex* nil)
(defvar *tex-size* (list (/ 640 2) 480
			 :texture-2d :rgba :rgba :unsigned-byte))
#+nil
(video:init)
#+nil
(video:set-format video:*fd*)
#+nil 
(video:uninit)

(defun run-main-loop ()
  (unwind-protect
       (progn 
	 (video:init)
	 (video:start-capturing)
	 (dotimes (i 10000)
	   (video:exchange-queue video:*fd*
				 #'(lambda (index)
				     (setf *update-texture* (first (elt video::*bufs* index)))
				     (sleep (/ 60))))))
    (progn
      (video:stop-capturing)
      (video:uninit))))
#+nil ;; data is actually yuyv in one rgba quadrupel, so 
(let ((m (make-array (list (second *tex-size*) (first *tex-size*) 4))))
  (destructuring-bind (h w c) (array-dimensions m)
   (dotimes (i w)
     (dotimes (j h)
       (setf 
	(aref m j i 0) 100
	(aref m j i 1) (mod i 255)
	(aref m j i 2) (mod j 255)
	(aref m j i 3) 255))))
  (setf *update-texture* (sb-ext:array-storage-vector m))
  nil)

#+nil
(sb-thread:make-thread #'(lambda () (run-main-loop))
		       :name "capture")


(let* (;; (projection-matrix-ind 0)
       ;; (projection-matrix (make-array 16 :element-type 'single-float))
       ;; (matrices (make-array 1 :element-type '(simple-array single-float (16))
       ;; 			     :initial-contents (list projection-matrix)))
       ;; (color-index 0)
       ;; (vertex-index 0)
       ;; (vertex-buffer-name 0)
       ;; (num-color-components 3)
       ;; (num-vertex-components 2)
       ;; (stride (* 4 (+ num-color-components num-vertex-components)))
       ;; (array (let* ((l '(1 0 0
       ;; 			  5 5
       ;; 			  0 1 0
       ;; 			  25 5
       ;; 			  0 0 1
       ;; 			  5 25)))
       ;; 		(make-array (length l)
       ;; 			    :element-type 'single-float
       ;; 			    :initial-contents (mapcar #'float l))))
       ;; (varray (gl::make-gl-array :pointer (sb-sys:vector-sap array)
       ;; 				    :size (length array)
       ;; 				    :type :float))
       ;; (num-elements (floor (length array) stride))
       )
  #+nil(defun init-buffer ()
    (let ((vbn (first (gen-buffers 1))))
      (bind-buffer :array-buffer vbn)
      (buffer-data :array-buffer :static-draw varray))
    (check-error "init-buffer"))
  (defun compile-and-check (shader)
    (compile-shader shader)
    (when (eq :false (get-shader shader :compile-status))
      (error (get-shader-info-log shader))))
  (defun link-and-check (program)
    (link-program program)
    (when (eq :false (get-program program :link-status))
      (error (get-program-info-log program))))
  (defun my-create-program (vertex-shader fragment-shader)
    (let ((program (gl:create-program)))
      (unless (= 0 vertex-shader)
	(attach-shader program vertex-shader))
      (unless (= 0 fragment-shader)
	(attach-shader program fragment-shader))
      (link-and-check program)
      program))
  (defun compile-shader-source (type stringlist)
    (let ((shader (create-shader type)))
      (shader-source shader stringlist)
      (compile-and-check shader)
      shader))
  (defun init-shader ()
    (let* ((v (compile-shader-source :vertex-shader *vertex-shader*))
	   (f (compile-shader-source :fragment-shader *fragment-shader*))
	   (p (my-create-program v f)))
      (use-program p)
      ;; (setf projection-matrix-ind (get-uniform-location 
      ;; 				   p "fg_ProjectionMatrix")
      ;; 	    color-index (get-attrib-location p "fg_Color")
      ;; 	    vertex-index (get-attrib-location p "fg_Vertex"))
      ;; (enable-vertex-attrib-array color-index)
      ;; (enable-vertex-attrib-array vertex-index)
      (check-error "init-shader")))
  (defun init-rendering ()
    (clear-color .1 .3 .3 1))
  (defun load-ortho-f (m l r b tt n f)
    (declare (type (simple-array single-float (16)) m)
	     (type single-float l r b tt n f))
    (set-mat m
	     (/ 2s0 (- r l)) 0s0 0s0 0s0
	     0s0 (/ 2s0 (- tt b)) 0s0 0s0
	     0s0 0s0 (/ -2s0 (- f n)) 0s0
	     (/ (+ r l) (- l r))
	     (/ (+ tt b) (- b tt))
	     (/ (+ f n) (- n f))
	     1s0))
  (defun load-ortho-2df (m l r b tt)
    (declare (type (simple-array single-float (16)) m)
	     (type single-float l r b tt))
    (load-ortho-f m l r b tt -1s0 1s0))
  (defun init-view ()
    (let ((h 800)
	  (w 800))
     (viewport 0 0 w h)
     (matrix-mode :projection)
     (load-identity)
     (ortho 0 w h 0 -1 1)
     (matrix-mode :modelview)
     (load-identity)
     #+nil (load-ortho-2df projection-matrix 0s0 30s0 0s0 (/ (* 30s0 h) w))))
  (defun init-tex (&optional (w 640) (h 480))
    (when *tex*
      (delete-textures (list *tex*))
      (setf *tex* nil))
    (unless *tex*
      (setf *tex* (first (gen-textures 1)))
      (active-texture :texture0)
      (bind-texture :texture-2d *tex*)
      (tex-parameter :texture-2d :texture-min-filter :nearest)
      (tex-parameter :texture-2d :texture-mag-filter :nearest)
      (destructuring-bind (ww hh target internal-format external-format type)
	  *tex-size*
	(tex-image-2d :texture-2d 0 internal-format ww hh 0
		     external-format type (if *update-texture*
					      *update-texture*
					      (sb-sys:int-sap 0))))))
  (defun my-init ()
    (init-view)
    ;(init-buffer)
    (init-shader)
    (init-rendering)
    (init-tex))
  #+nil (defun triangle ()
    ;(uniform-matrix projection-matrix-ind 4 matrices nil)
    (bind-buffer :array-buffer vertex-buffer-name)
    (vertex-attrib-pointer color-index num-color-components
			      :float :false stride (sb-sys:int-sap 0))
    (vertex-attrib-pointer vertex-index num-vertex-components 
			      :float :false stride (sb-sys:int-sap 
						    (* 4 num-color-components)))
    (draw-arrays :triangles 0 num-elements)
    (check-error "triangle"))
  (defun draw-tex ()
    (when *tex*
     (enable :texture-2d)
     (bind-texture :texture-2d *tex*)
     (let* ((x 0)
	    (y 0)
	    (w 640)
	    (h 480)
	    (wt 1)
	    (ht 1)
	    (q 1 #+nil (/ h w)))
       (with-primitive :quads
	 (color 1 1 1)
	 (tex-coord 0 0) (vertex x y)
	 (tex-coord wt 0) (vertex w y)
	 (tex-coord wt ht) (vertex w (* q h))
	 (tex-coord 0 ht) (vertex x (* q h))))
     (disable :texture-2d)))
  (defun update-texture ()
    (when *update-texture*
      (destructuring-bind (w h target internal-format external-format type) *tex-size*
	(declare (ignore internal-format))
	(tex-sub-image-2d target 0 0 0 w h external-format type *update-texture*))
      (setf *update-texture* nil)))
  (defun draw ()
    (when *reinitialize*
      (my-init)
      (setf *reinitialize* nil))
    (clear :color-buffer-bit)
  ;  (triangle)
   #+NIL (with-primitive :line-loop
      (vertex 0 0)
      (vertex 1 0)
      (vertex 0 1))
   (update-texture)
   (with-pushed-matrix
     (translate 100 200 0)
     (draw-tex))
   (with-primitive :triangles
     (color .3 .9 .9)
      (vertex 10 10)
      (color 1 1 0)
      (vertex 100 10)
      (color 1 0 1)
      (vertex 10 100))
    (check-error "draw"))
  (setf *reinitialize* t))

#+Nil
(sb-thread:make-thread #'(lambda () (with-gui (800 800)
				 (draw)))
		       :name "ogl")
