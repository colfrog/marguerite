(in-package :marguerite)

(defparameter *port* 4267)
(defvar *db-path* "db.sqlite")
(defvar *db* (sqlite:connect *db-path*))

(defun start-marguerite ()
  (start (make-instance 'hunchentoot:easy-acceptor :port *port*)))

(defun is-logged-in ()
  (session-value :username))

(push (create-static-file-dispatcher-and-handler
       "/style.css" "public/style.css")
      hunchentoot:*dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/imageOverlay.js" "public/imageOverlay.js")
      hunchentoot:*dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/imageReordering.js" "public/imageReordering.js")
      hunchentoot:*dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/favicon.ico" "public/favicon.ico")
      hunchentoot:*dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/background.jpg" "public/background.jpg")
      hunchentoot:*dispatch-table*)

(defmacro with-layout ((title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (let* ((header-stuff (car (execute-to-list *db* "select header, presentation from home")))
	    (categories (execute-to-list *db* "select distinct category from images order by category asc"))
	    (header (car header-stuff))
	    (presentation (cadr header-stuff)))
       (htm
	(:html
	 (:head
	  (:meta :name "viewport" :content "width=device-width,initial-scale=1")
	  (:link :rel "stylesheet" :href "style.css")
	  (:title (str (if ,title (concatenate 'string header ,title) header))))
	 (:body
	  (:main
	   (:header
	    (if (is-logged-in)
		(htm (:a :href "/logout" :style "color: red" (:h1 (str header))))
		(htm (:a :href "/login" (:h1 (str header)))))
	    (if (is-logged-in)
		(htm
		 (:form
		  :id "change-presentation" :method "post" :action "change-presentation"
		  (:input :type "text" :style "width: 50%" :name "text" :value presentation)
		  (:input :type "submit")))
		(htm (:h5 (str presentation)))))
	   (:nav
	    (dolist (category categories)
	      (htm
	       (:a :href (concatenate 'string "/#" (car category)) (str (car category))))))
	   (:article ,@body))
	  (:div :id "image-overlay" :onclick "closeOverlay()")
	  (:footer (:small (:b "&copy; Laurent Cimon 2024") (:br) "Designed with &hearts; and &lambda;"))
	  (:script :src "imageOverlay.js")
	  (when (is-logged-in) (htm (:script :src "imageReordering.js")))))))))

(define-easy-handler (home-page :uri "/") ()
  (let ((pictures (execute-to-list *db* "select id, category, pos from images order by category asc, pos asc")))
    (with-layout (nil)
      (:div
       :id "main-view"
       (when (is-logged-in)
	 (htm
	  (:h3 "Add an image")
	  (:form
	   :id "add-image" :method "post" :action "add-image" :enctype "multipart/form-data"
	   (:label :for "category" "Category: ")
	   (:input :type "text" :name "category")
	   (:br)
	   (:input :type "file" :name "file" :accept "image/png,image/jpg")
	   (:br)
	   (:input :type "submit"))))
       (let ((current pictures))
	 (dolist (category categories)
	   (htm
	    (:div
	     :id (car category)
	     (if (is-logged-in)
		 (:form
		  :id "change-category-name" :method "post" :action "change-category-name"
		  (:input :type "hidden" :name "old-category" :value (car category))
		  (:input :type "text" :name "new-category" :value (car category))
		  (:input :type "submit" :value "Change category name"))
		 (:h3 (str (car category))))
	     (:div :class "image-list"
		   (do* ((category-pictures current (cdr category-pictures))
			 (picture (car category-pictures) (when category-pictures (car category-pictures))))
			((or (null category-pictures) (not (equal (cadr picture) (car category))))
			 (setf current category-pictures))
		     (htm (:div
			   :id (car picture) :class "image-div" :style (when (is-logged-in) "border: 2px solid red")
			   (:img :src (concatenate 'string "/i/" (car picture))
				 :id (car picture)
				 :draggable "false"
				 :onmousedown (if (is-logged-in)
						  (concatenate 'string "dragStart(\"" (car picture) "\")")
						  (concatenate 'string "showOverlay(\"" "/i/" (car picture) "\")")))
			   (when (is-logged-in)
			     (htm
			      (:form
			       :id "change-category" :method "post" :action "change-category"
			       (:input :type "hidden" :name "id" :value (car picture))
			       (:label :for "category" "Category: ")
			       (:select
				:size "1" :name "category"
				(dolist (category-option categories)
				  (htm (:option :value (car category-option) (str (car category-option))))))
			       (:br)
			       (:input :type "submit" :value "Change category"))
			      (:form
			       :id "delete-image" :method "post" :action "delete-image"
			       (:input :type "hidden" :name "id" :value (car picture))
			       (:input :type "submit" :value "Delete"))))))))))))))))

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

(hunchentoot:define-easy-handler (add-image :uri "/add-image")
    ((category :request-type :post)
     (file :request-type :post))
  (when (and (is-logged-in) category file)
    (with-open-file (stream (car file) :element-type 'unsigned-byte)
      (let ((buffer (make-array (file-length stream) :initial-element nil)))
	(read-sequence buffer stream)
	(execute-non-query *db* "insert into images (id, pos, category, file) values (?1, (select count(*) from images where category = ?2), ?2, ?3)" (cadr file) category buffer)))
    (redirect (concatenate 'string "/#" category))))

(hunchentoot:define-easy-handler (change-presentation :uri "/change-presentation")
    ((text :request-type :post))
  (when (and (is-logged-in) text)
    (execute-non-query *db* "update home set presentation = ?" text)
    (redirect "/")))

(hunchentoot:define-easy-handler (update-image-position :uri "/update-image-position")
    ((id :request-type :post)
     (pos :request-type :post)
     (category :request-type :post))
  (when (and (is-logged-in) id pos category)
    (execute-non-query *db* "update images set pos = pos + 1 where category = ? and pos >= ?" category pos)
    (execute-non-query *db* "update images set pos = ? where id = ?" (parse-integer pos) id)))

(hunchentoot:define-easy-handler (change-category :uri "/change-category")
    ((id :request-type :post)
     (category :request-type :post))
  (when (and (is-logged-in) id category)
    (execute-non-query *db* "update images set category = ? where id = ?" category id)
    (redirect "/")))

(hunchentoot:define-easy-handler (delete-image :uri "/delete-image")
    ((id :request-type :post))
  (when (and (is-logged-in) id)
    (execute-non-query *db* "delete from images where id = ?" id)
    (redirect "/")))

(hunchentoot:define-easy-handler (change-category-name :uri "change-category-name")
    ((old-category :request-type :post)
     (new-category :request-type :post))
  (when (and (logged-in) old-category new-category)
    (execute-non-query *db* "update images set category = ? where category = ?" new-category old-category)
    (redirect "/")))
