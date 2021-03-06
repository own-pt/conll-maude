;; Copyright 2016 IBM

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;;; conll-maude.lisp

(in-package #:conll-maude)

(defun make-id (context prefix id)
  (format nil "id('f~a-~a~a)"
          (cl-ppcre:regex-replace-all "[^A-Za-z0-9-]" (string-downcase (string-trim '(#\- #\. #\, #\; #\` #\' #\space) context)) "-") prefix id))

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

;; . ID: Word index, integer starting at 1 for each new sentence; may be a range for multiword tokens; may be a decimal number for empty nodes.
;; . FORM: Word form or punctuation symbol.
;; . LEMMA: Lemma or stem of word form.
;; . UPOSTAG: Universal part-of-speech tag.
;; . XPOSTAG: Language-specific part-of-speech tag; underscore if not available.
;; . FEATS: List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.
;; . HEAD: Head of the current word, which is either a value of ID or zero (0).
;; . DEPREL: Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.
;; . DEPS: Enhanced dependency graph in the form of a list of head-deprel pairs.
;; . MISC: Any other annotation.

(defun convert-line (context sentence line)
  (destructuring-bind (word-index token lemma upos xpos feats head dep-rel deps misc) line
    (let ((word-index-id (make-id context "i" word-index))
          (sentence-id (make-id context "s" sentence))
          (head-id (make-id context "i" head)))
      (emit-maude "idx" (format nil "index(~a,~a,~a)" sentence-id word-index-id word-index))
      (emit-maude "token" (format nil "token(~a,~a,~a)" sentence-id word-index-id (maude-string token nil)))
      (emit-maude "lemma" (format nil "lemma(~a,~a,~a)" sentence-id word-index-id (maude-string lemma)))
      (emit-maude "upos" (format nil "upos(~a,~a,~a)" sentence-id word-index-id (maude-qid upos)))
      (emit-maude "xpos" (format nil "xpos(~a,~a,~a)" sentence-id word-index-id (maude-qid xpos)))
      (emit-maude "deps" (format nil "xpos(~a,~a,~a)" sentence-id word-index-id (maude-qid deps)))
      (emit-maude "misc" (format nil "xpos(~a,~a,~a)" sentence-id word-index-id (maude-qid misc)))
      (emit-maude dep-rel (format nil "dependency('~a,~a,~a,~a)" (clean-dep-rel dep-rel) sentence-id word-index-id head-id)))))

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

;; (convert-filename "sample" "complex.conll" "complex.pl")
