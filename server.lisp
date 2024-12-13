(in-package :marguerite)

(defparameter *port* 4267)
(defparameter *default-title* "Raven")
(defvar *db-path* "db.sqlite")
(defvar *db* (sqlite:connect *db-path*))

(defun start-marguerite ()
  (start (make-instance 'hunchentoot:easy-acceptor :port *port*)))

(defun is-logged-in ()
  (session-value :username))

(defmacro with-layout ((title) &body body)
  (let* ((header-stuff (car (execute-to-list *db* "select header, presentation from home")))
	 (header (car header-stuff))
	 (presentation (cadr header-stuff)))
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head
       (:meta :name "viewport" :content "width=device-width,initial-scale=1")
       (:link :rel "stylesheet" :href "style.css")
       (:title (str (if ,title (concatenate 'string ,*default-title* ,title) ,*default-title*))))
      (:body
       (:main
	(:header
	 (if (is-logged-in)
	     (:a :href "/logout" :style "color: red" (:h1 (str ,header)))
	     (:a :href "/login" (:h1 (str ,header))))
	 (if (is-logged-in)
	     (htm
	      (:form
	       :id "change-presentation" :method "post" :action "change-presentation"
	       (:input :type "text" :name "text" :value ,presentation)
	       (:input :type "submit")))
	     (htm (:h5 (str ,presentation)))))
	(:article ,@body)))))))

(define-easy-handler (home-page :uri "/") ()
  (let ((pictures (execute-to-list *db* "select id, category, pos from images order by category asc, pos asc"))
	(categories (execute-to-list *db* "select unique(category) from images order by category asc")))
    (with-layout (nil)
      (:div
       (when (is-logged-in)
	 (:form
	  :id "add-image" :method "post" :action "add-image" :enctype "multipart/form-data"
	  (:input :type "text" :name "id")
	  (:br)
	  (:input :type "text" :name "category")
	  (:br)
	  (:input :type "file" :name "data" :accept "image/png,image/jpg")
	  (:br)
	  (:input :type "submit")))
       (let ((current pictures))
	 (dolist (category categories)
	   (htm
	    (:div
	     :id (str (car category))
	     (:h3 (str (car category)))
	     (do* ((category-pictures current (cdr category-pictures))
		   (picture (car category-pictures) (when category-pictures (car category-pictures))))
		  ((or (null category-pictures) (not (equal (cadr picture) category)))
		   (setf current category-pictures))
	       (htm (:img :href (concatenate 'string "/i/" (car picture)))))))))))))

(hunchentoot:define-easy-handler
    (image
     :uri (lambda (request)
	    (let ((uri (hunchentoot:request-uri request)))
	      (string= (and (> (length uri) 3) (subseq uri 0 2)) "/i"))))
    ()
  (setf (hunchentoot:content-type*) "image/png")
  (let ((image-name
	 (url-decode
	  (subseq (hunchentoot:request-uri hunchentoot:*request*) 3))))
    (sqlite:execute-single
     *db*
     "select file from images where id = ?" image-name)))
