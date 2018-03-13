(defsystem "cl-gauth"
    :author "Kieran Grant"
    :description "Port of gauth. Local laptop/desktop Google Authenticator written in LISP"
    :license "MIT"
    :components ((:file "package")
		 (:file "gauth" :depends-on ("package")))
    :depends-on ("ironclad" "cl-base32" "split-sequence"))
