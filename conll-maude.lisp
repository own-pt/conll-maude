;;;; conll-maude.lisp

(in-package #:conll-maude)

(defun make-id (context sentence token)
  (format nil "id('f~a-s~a-t~a)"
          (cl-ppcre:regex-replace-all "[^A-Za-z0-9-]" (string-downcase (string-trim '(#\- #\. #\, #\; #\` #\' #\space) context)) "-")
          sentence
          token))

(defun clean-dep-rel (str)
  (cl-ppcre:regex-replace-all "[^A-Za-z0-9-]" (string-downcase (string-trim '(#\- #\. #\, #\; #\` #\' #\space) str)) "-"))

(defparameter *clauses* nil)

(defun emit-maude (clause text)
  (push text (gethash clause *clauses*)))

(defun write-maude (out modulename)
  (format out "mod ~a is~%" modulename)
  (format out "inc FACTS .~%")
  (format out "eq init = ")
  (maphash (lambda (k clauses) 
             (dolist (c clauses) (format out "~a " c))) *clauses*)
  (format out ".~%")
  (format out "endm~%"))

(defun maude-qid (str)
  (format nil "'~a"
          (cond
              ((string= str ",") "COMMA")
              ((string= str "``") "BQ")
              ((string= str ".") "STOP")
              ((string= str "'") "SQUOTE")
              ((string= str "''") "SQUOTE")
              ((string= str "\"") "DQUOTE")
              ((string= str "\"\"") "DQUOTE")
              (t str))))

(defun maude-string (str &optional (downcase t))
  (format nil "\"~a\"" (cl-ppcre:regex-replace-all "[^A-Za-z0-9]" (if downcase (string-downcase str) str) "-")))

(defun convert-line (context sentence line)
  (destructuring-bind (word-index token lemma pos ner head dep-rel) line
    (let ((word-index-id (make-id context sentence word-index))
          (head-id (make-id context sentence head)))
      (emit-maude "idx" (format nil "index(~a,~a)" word-index-id word-index))
      (emit-maude "token" (format nil "token(~a,~a)" word-index-id (maude-string token nil)))
      (emit-maude "lemma" (format nil "lemma(~a,~a)" word-index-id (maude-string lemma)))
      (emit-maude "pos" (format nil "pos(~a,~a)" word-index-id (maude-qid pos)))
      (emit-maude "ner" (format nil "ner(~a,~a)" word-index-id (maude-qid ner)))
      (emit-maude dep-rel (format nil "dependency('~a,~a,~a)" (clean-dep-rel dep-rel) word-index-id head-id)))))

(defun is-comment (line)
  (starts-with-subseq "#" line))

(defun clean-whitespace (line)
  (string-trim '(#\space #\tab) line))

(defun valid-line (line)
  (> (length line) 0))

(defun parse-csv (line)
  (fare-csv:with-rfc4180-csv-syntax ()
    (let ((fare-csv:*separator* #\Tab))
      (with-input-from-string (s line)
        (fare-csv:read-csv-line s)))))

(defun convert-filename (context filename-in filename-out)
  (setf *clauses* (make-hash-table :test #'equal))
  (let ((sentence 0))
    (with-open-file (fin filename-in :direction :input)
      (loop for line = (read-line fin nil :eof)
         until (eq line :eof)
         do 
           (progn
             (unless (is-comment line) 
               (if (valid-line line)
                   (convert-line context sentence (parse-csv line))
                   (incf sentence)))))))

  (with-open-file (fout filename-out :direction :output :if-exists :supersede)
    (write-maude fout (pathname-name filename-in))))

;; (convert-filename "sample" "/home/fcbr/repos/conll-maude/complex.conll" "complex.pl")
