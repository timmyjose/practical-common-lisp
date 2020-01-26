(defun write-to-file (message filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print message out))))

(defun read-from-file (filename)
  (with-open-file (in filename
		      :direction :input)
    (with-standard-io-syntax
      (format t "~a~%" (read in)))))
