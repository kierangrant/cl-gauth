(in-package :cl-user)

(defpackage :cl-gauth
  (:use :cl)
  (:export
   #:time-stamp
   #:normalize-secret
   #:auth-code))
