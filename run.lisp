#!/usr/local/bin/sbcl --script
(defmacro muffle (&body body)
  `(let* ((*standard-output* (make-broadcast-stream))
	  (*debug-io* *standard-output*))
     (handler-bind
	 ((warning (lambda (c) (muffle-warning c))))
       ,@body)))

(muffle (require "asdf"))
(muffle (asdf:operate 'asdf:load-op "cl-gauth" :verbose nil))

(handler-case
    (cl-gauth::main)
  (error (e) (format *debug-io* "An error occured: ~S~%" e)))

