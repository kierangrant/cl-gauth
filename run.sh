#!/bin/sh
sbcl --noinform --lose-on-corruption --no-userinit --no-sysinit --noprint --disable-debugger \
     --eval """
(defmacro muffle (&body body)
  \`(let* ((*standard-output* (make-broadcast-stream))
   (*debug-io* *standard-output*))
     (handler-bind
  ((warning (lambda (c) (muffle-warning c))))
       ,@body)))""" \
	   --eval """
(muffle (require \"asdf\"))""" \
	   --eval """
(muffle
 (if (uiop/filesystem:file-exists-p
      (merge-pathnames \"quicklisp/setup.lisp\"
         (user-homedir-pathname)))
       (load
 (merge-pathnames \"quicklisp/setup.lisp\"
    (user-homedir-pathname))))) """ \
	   --eval """
(muffle
 (if (and (find-package \"QL\") (find-symbol \"QUICKLOAD\" (find-package \"QL\")))
       (funcall (find-symbol \"QUICKLOAD\" (find-package \"QL\")) \"cl-gauth\" :vebose nil :silent t)
   (asdf:operate 'asdf:load-op \"cl-gauth\" :verbose nil)))
""" \
	   --eval """
(handler-case
    (cl-gauth::main)
  (error (e) (format *debug-io* \"An error occured: ~A~%\" e)))
""" --quit
