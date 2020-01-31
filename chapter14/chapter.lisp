;;; Chapter 14 - Files and File I/O

;;; reading file data - character streams

(defun read-first-line-from-file (filename)
  (let ((in (open filename)))
    (format t "~a~%" (read-line in))
    (close in)))

(defun read-first-line-from-file-that-may-not-exist (filename)
  (let ((in (open filename
		  :if-does-not-exist nil)))
    (when in (format t "~a~%" (read-line in))
	  (close in))))

(defun print-all-lines-in-file (filename)
  (let ((in (open filename
		  :if-does-not-exist nil)))
    (when in (loop for line = (read-line in nil)
		while line do (format t "~a~%" line))
	  (close in))))

;;; reading binary data

(defun read-as-binary-file (filename)
  (let ((in (open filename
		  :if-does-not-exist nil
		  :element-type '(unsigned-byte 8))))
    (when in
      (loop for b = (read-byte in nil)
	 while b do (format t "~x" b)))
    (close in)))

;;; file output

(defun copy-file (src target)
  (let ((in (open src
		  :direction :input
		  :if-does-not-exist nil
		  :element-type '(unsigned-byte 8))))
    (when in
      (let ((out (open target
		       :direction :output
		       :if-exists :supersede
		       :element-type '(unsigned-byte 8))))
	(loop for b = (read-byte in nil)
	   while b do (write-byte b out))
	(close out))
      (close in))))

(defun safe-copy-file (src target)
  (with-open-file (in src
		      :direction :input
		      :element-type '(unsigned-byte 8)
		      :if-does-not-exist nil)
    (when in
      (with-open-file (out target
			   :direction :output
			   :if-exists :supersede
			   :element-type '(unsigned-byte 8))
	(loop for b = (read-byte in nil)
	   while b do (write-byte b out))))))

;;; pathnames - (host, device, directory, name, type, version).

(assert (equalp (pathname "chapter.lisp") #p "chapter.lisp"))
(assert (equalp (namestring (pathname "chapter.lisp")) "chapter.lisp"))

(defun pathname-designator-functions-example ()
  (let* ((namestring "/Users/z0ltan/foo/bar/baz/quux.txt")
	 (host (pathname-host namestring))
	 (device (pathname-device namestring))
	 (directory (pathname-directory namestring))
	 (name (pathname-name namestring))
	 (type (pathname-type namestring))
	 (version (pathname-version namestring)))
    (format t "Host: ~a, Device: ~a, Directory: ~a, Name: ~a, Type: ~a, Version: ~a~%"
	    host
	    device
	    directory
	    name
	    type
	    version)))
(defun namestring-functions-example ()
  (let* ((namestr "/Users/z0ltan/.cargo/.crates.toml")
	 (path (pathname namestr))
	 (dir (directory-namestring path))
	 (file (file-namestring path)))
    (format t "dir: ~a, file: ~a, namestring: ~a~%"
	    dir
	    file
	    (namestring path))))

(defun constructing-pathnames-example (directory file extension absolute)
  (let ((path (make-pathname
	       :directory `(,(if absolute :absolute :relative) ,directory)
	       :name file
	       :type extension)))
    (format t "path = ~a~%" path)))

(assert (equalp (merge-pathnames #p "foo/bar.html" #p "/www/html/")
		#p "/www/html/foo/bar.html"))

(assert (equalp (merge-pathnames #p "foo/bar.html" #p "/www/html") ; note
		#p "/www/foo/bar.html"))

(assert (equalp (merge-pathnames #p "foo/bar.html" #p "html/")
		#p "html/foo/bar.html"))

(assert (equalp (merge-pathnames #p "foo/bar.html" #p "html") ; note
		#p "foo/bar.html"))

(assert (equalp (enough-namestring #p "/www/html/foo/bar.html" #p "/www/html/")
		"foo/bar.html"))

;; idiomatic way to create a new relative path under a different root
(assert (equalp (merge-pathnames
		 (enough-namestring #p "/www/html/foo/bar/baz.html" #p "/www/")
		 #p "/www-backups/")
		#p "/www-backups/html/foo/bar/baz.html"))

(defun get-file-length (filename)
  (with-open-file (in filename
		      :element-type '(unsigned-byte 8))
    (file-length in)))

(defun read-string-as-float ()
  (let ((s (make-string-input-stream "1.2345")))
    (unwind-protect (read s)
      (close s))))

(defun better-read-string-as-float ()
  (with-input-from-string (s "1.2345")
    (read s)))

(defun write-to-string-example ()
  (with-output-to-string (s)
    (format s "Hello, world")
    (format s "~d" 199)))


