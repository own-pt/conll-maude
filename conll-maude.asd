;;;; conll-maude.asd

(asdf:defsystem #:conll-maude
  :description "Converts CONLL files to Maude."
  :author "Fabricio Chalub <fchalub@br.ibm.com>"
  :license "IBM"
  :depends-on (#:fare-csv
               #:cl-ppcre
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "conll-maude")))

