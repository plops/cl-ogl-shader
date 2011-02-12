;; shader example from freeglut smooth_opengl3.c

(eval-when (:compile-toplevel :execute :load-toplevel)
  (push "/home/martin/floh-mit/0102/woropt-cyb-0628/" asdf:*central-registry*)
  (require :gui))

(defparameter *vertex-shader*
  "#version 140
uniform mat4 fg_ProjectionMatrix;
in vec4 fg_Color;
in vec4 fg_Vertex;
smooth out vec4 fg_SmoothColor;
void main ()
{
  fg_SmoothColor=fg_Color;
  gl_Position=fg_ProjectionMatrix * fg_Vertex;
}")

(defparameter *fragment-shader*
  "#version 140
smooth in vec4 fg_SmoothColor;
out vec4 fg_FragColor;
void main(void)
{
  fg_FragColor = fg_SmoothColor;
}")

(defmacro set-mat (m &rest rest)
  (let ((res nil)
	(i 0))
    (dolist (e rest)
      (push `(aref ,m ,i) res)
      (push e res)
      (incf i))
    `(setf ,@(reverse res))))

(defvar *reinitialize* t)

(let* ((projection-matrix-ind 0)
       (projection-matrix (make-array 16 :element-type 'single-float))
       (matrices (make-array 1 :element-type '(simple-array single-float (16))
			     :initial-contents (list projection-matrix)))
       (color-index 0)
       (vertex-index 0)
       (vertex-buffer-name 0)
       (num-color-components 3)
       (num-vertex-components 2)
       (stride (* 4 (+ num-color-components num-vertex-components)))
       (array (let* ((l '(1 0 0
			   5 5
			   0 1 0
			   25 5
			   0 0 1
			   5 25)))
		(make-array (length l)
			    :element-type 'single-float
			    :initial-contents (mapcar #'float l))))
       (varray (gl::make-gl-array :pointer (sb-sys:vector-sap array)
				    :size (length array)
				    :type :float))
       (num-elements (floor (length array) stride)))
  (defun init-buffer ()
    (let ((vbn (first (gl:gen-buffers 1))))
      (gl:bind-buffer :array-buffer vbn)
      (gl:buffer-data :array-buffer :static-draw varray))
    (gl:check-error "init-buffer"))
  (defun compile-and-check (shader)
    (gl:compile-shader shader)
    (when (eq :false (gl:get-shader shader :compile-status))
      (error (gl:get-shader-info-log shader))))
  (defun link-and-check (program)
    (gl:link-program program)
    (when (eq :false (gl:get-program program :link-status))
      (error (gl:get-program-info-log program))))
  (defun create-program (vertex-shader fragment-shader)
    (let ((program (gl:create-program)))
      (unless (= 0 vertex-shader)
	(gl:attach-shader program vertex-shader))
      (unless (= 0 fragment-shader)
	(gl:attach-shader program fragment-shader))
      (link-and-check program)
      program))
  (defun compile-shader-source (type stringlist)
    (let ((shader (gl:create-shader type)))
      (gl:shader-source shader stringlist)
      (compile-and-check shader)
      shader))
  (defun init-shader ()
    (let* ((v (compile-shader-source :vertex-shader *vertex-shader*))
	   (f (compile-shader-source :fragment-shader *fragment-shader*))
	   (p (create-program v f)))
      (gl:use-program p)
      (setf projection-matrix-ind (gl:get-uniform-location 
				   p "fg_ProjectionMatrix")
	    color-index (gl:get-attrib-location p "fg_Color")
	    vertex-index (gl:get-attrib-location p "fg_Vertex"))
      (gl:enable-vertex-attrib-array color-index)
      (gl:enable-vertex-attrib-array vertex-index)
      (gl:check-error "init-shader")))
  (defun init-rendering ()
    (gl:clear-color .2 0 0 1))
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
    (let ((h 512)
	  (w 512))
     (gl:viewport 0 0 w h)
     (load-ortho-2df projection-matrix 0s0 30s0 0s0 (/ (* 30s0 h) w))))
  (defun init ()
    (init-view)
    (init-buffer)
    (init-shader)
    (init-rendering))
  (defun triangle ()
    (gl:uniform-matrix projection-matrix-ind 4 matrices nil)
    (gl:bind-buffer :array-buffer vertex-buffer-name)
    (gl:vertex-attrib-pointer color-index num-color-components
			      :float :false stride (sb-sys:int-sap 0))
    (gl:vertex-attrib-pointer vertex-index num-vertex-components 
			      :float :false stride (sb-sys:int-sap 
						    (* 4 num-color-components)))
    (gl:draw-arrays :triangles 0 num-elements)
    (gl:check-error "triangle"))
  (defun draw ()
    (when *reinitialize*
      (init)
      (setf *reinitialize* nil))
    (gl:clear :color-buffer-bit)
    (triangle)
    (gl:check-error "draw"))
  (setf *reinitialize* t))

#+nil
(sb-thread:make-thread #'(lambda () (gui:with-gui 
				 (draw)))
		       :name "ogl")