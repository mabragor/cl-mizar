;;;; cl-mizar.asd

(asdf:defsystem #:cl-mizar
  :version "0.1"
  :serial t
  :description "Port of MIZAR to Common Lisp"
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPLv3"
  :depends-on (#:esrap-liquid #:iterate #:cl-interpol
			      #:defmacro-enhance #:cl-read-macro-tokens #:zs3)
  :components ((:file "package")
	       (:file "parsing-macro")
	       (:file "parsing-macro-2")
	       (:file "parsing")
               (:file "cl-mizar")))

