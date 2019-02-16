;; License: MIT
;; Based on gauth at https://github.com/pcarrier/gauth

(declaim (optimize (speed 3) (safety 1) (debug 0) (space 0) (compilation-speed 0)))

(in-package :cl-gauth)

(declaim (ftype (function nil (values (unsigned-byte 64) (unsigned-byte 64))) time-stamp))
(defun time-stamp ()
  (let ((current-time (- (the (unsigned-byte 32) (get-universal-time))
			 (the (unsigned-byte 32) #.(encode-universal-time 0 0 0 1 1 1970 0)))))
    (declare (type (unsigned-byte 32) current-time))
    (truncate current-time 30)))

(declaim (ftype (function (string) string) normalize-secret))
(defun normalize-secret (secret)
  (let* ((no-padding (remove-if (lambda (x) (char= x #\Space)) (string-upcase secret)))
	 (pad-length (- 8 (rem (length no-padding) 8))))
    (if (< pad-length 8)
	(with-output-to-string (out)
	  (write-string no-padding out)
	  (dotimes (i pad-length)
	    (write-char #\= out)))
	no-padding)))

(declaim (ftype (function (string (unsigned-byte 32)) string) auth-code))
(defun auth-code (secret time)
  (let ((key (cl-base32:base32-to-bytes secret))
	(msg (make-array 8 :element-type '(unsigned-byte 8)))
	hash offset trunc)
    (declare (type (simple-array (unsigned-byte 8)) key msg))
    ;; msg is time stored in Big-Endian format
    (dotimes (i 8) (setf (elt msg i) (ldb (byte 8 (* 8 (- 7 i))) time)))
    (setf hash
	  (let ((hmac (ironclad:make-hmac key 'ironclad:sha1)))
	    (ironclad:update-hmac hmac msg)
	    (the (simple-array (unsigned-byte 8)) (ironclad:hmac-digest hmac))))
    (setf offset (logand (elt hash 19) #xf))
    (setf trunc (subseq hash offset (+ offset 4)))
    (setf (elt trunc 0) (logand (elt trunc 0) #x7f))
    ;; now lets build up number
    (format nil "~6,'0d"
	    (mod
	     (logior (ash (elt trunc 0) 24)
		     (ash (elt trunc 1) 16)
		     (ash (elt trunc 2) 8)
		     (ash (elt trunc 3) 0))
	     1000000))))

(defun main ()
  (if (not (or
	    (uiop/filesystem:file-exists-p (merge-pathnames ".config/gauth.csv.aes256" (user-homedir-pathname)))
	    (uiop/filesystem:file-exists-p (merge-pathnames ".config/gauth.csv" (user-homedir-pathname)))))
      (error "Missing gauth config file!"))
  (let* ((file-content
	   (if (uiop/filesystem:file-exists-p
		(merge-pathnames ".config/gauth.csv.aes256" (user-homedir-pathname)))
	       (with-input-from-string (in
					(uiop/run-program:run-program 
					 `("openssl" "enc" "-d" "-aes256" "-md" "sha256" "-in"
						     ,(uiop/pathname:unix-namestring
						       (merge-pathnames ".config/gauth.csv.aes256"
									(user-homedir-pathname))))
					 :input :interactive
					 :output '(:string :stripped t)
					 :error-output t))
		 (loop for line = (read-line in nil nil) until (null line) collecting line))
	       (with-open-file (in (merge-pathnames ".config/gauth.csv" (user-homedir-pathname))
				   :direction :input)
		 (loop for line = (read-line in nil nil) until (null line) collecting line))))
	 (max-length (max 10 (loop for item in file-content maximizing (length (elt (split-sequence:split-sequence #\: item) 0))))))
    (multiple-value-bind (current-time progress) (time-stamp)
      (format t (format nil "~~~dt prev   curr   next~~%" max-length))
      (loop for line in file-content do
	   (let* ((seq (split-sequence:split-sequence #\: line))
		  (name (elt seq 0)) (secret (normalize-secret (elt seq 1))))
	     (format t "~@? ~a ~a ~a~%"
		     (format nil "~~~da" max-length)
		     name
		     (auth-code secret (1- current-time))
		     (auth-code secret current-time)
		     (auth-code secret (1+ current-time)))))
      (format t "[~29a]~%" (with-output-to-string (out) (dotimes (i progress) (write-char #\= out))))
      (terpri))))

(defun entrypoint ()
  (handler-case
      (main)
    (error (e)
      (format *error-output* "An error occured: ~A~%" e)
      (uiop:quit -1))))
