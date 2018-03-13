(defsystem "cl-gauth"
    :author "Kieran Grant"
    :components ((:file "package")
		 (:file "gauth" :depends-on ("package")))
    :depends-on ("ironclad" "cl-base32" "split-sequence"))
