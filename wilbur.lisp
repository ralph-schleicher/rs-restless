;;; wilbur.lisp --- Wilbur extensions

;; Copyright (C) 2021 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :nox) ;a.k.a. Wilbur

(defun unicode-whitespace-char-p (char)
  "Return true if CHAR is a whitespace character.
Argument CHAR has to be a character object."
  (declare (type character char))
  #+cl-unicode
  (cl-unicode:has-binary-property char (quote #.(cl-unicode:property-symbol "White_Space")))
  #-cl-unicode
  (or (char= char #\Space) (not (graphic-char-p char))))

(defun collapse-whitespace (string)
  "Remove leading and trailing whitespace characters in STRING
and replace any intermediate sequence of whitespace characters
with a single space character."
  (with-output-to-string (stream)
    (loop :for char :across string
	  :with state = :start
	  :do (cond ((eq state :parse)
		     (if (unicode-whitespace-char-p char)
			 (setf state :space)
		       (princ char stream)))
		    ((not (unicode-whitespace-char-p char))
		     ;; Found a non-whitespace character after skipping
		     ;; a sequence of whitespace characters.
		     (when (eq state :space)
		       (princ #\Space stream))
		     (princ char stream)
		     (setf state :parse))
		    ))))

(in-package :rs-restless)

(defun wilbur-add-namespaces (namespaces)
  "Announce namespaces to Wilbur.

Argument NAMESPACES is an alist with cons cells of the
form ‘(PREFIX . URI)’."
  (dolist (namespace namespaces)
    (destructuring-bind (prefix . uri)
	namespace
      (when (and (stringp prefix) (stringp uri))
	(wilbur:del-namespace prefix)
	(wilbur:add-namespace prefix uri)))))

(defun wilbur-parse-rdf/xml (source source-uri &key destination (parser :cl-rdfxml))
  "Parse an RDF/XML document into a Wilbur database.

First argument SOURCE is the RDF/XML document; value has to be
 a stream, a pathname, or a string.
Second argument SOURCE-URI is the URI of the RDF/XML document.
 The source URI is only required if the RDF description element
 has no ‘about’ attribute (TBC).
Keyword argument DESTINATION is the Wilbur database; ‘nil’ means
 to create a new database.
Keyword argument PARSER is the RDF/XML parser.  Value is either
 ‘:wilbur’ (the built-in Wilbur RDF/XML parser) or ‘:cl-rdfxml’.

Return value is the Wilbur database."
  (check-type destination (or null wilbur:db))
  (check-type source (or stream pathname string))
  (when (not (null source-uri))
    (setf source-uri (string-from-uri source-uri)))
  (ecase parser
    (:wilbur
     (labels ((parse (stream)
		(let ((database (wilbur:parse-db-from-stream stream source-uri)))
		  (if destination
		      (wilbur:db-merge destination database source-uri)
		    (setf destination database))
		  destination)))
       (typecase source
	 (pathname
	  (with-open-file (stream source)
	    (parse stream)))
	 (string
	  (with-input-from-string (stream source)
	    (parse stream)))
	 (t
	  (parse source)))))
    (:cl-rdfxml
     ;; The ‘cl-rdfxml:parse-document’ function provides a
     ;; ‘:coerce-datatyped-empty-properties’ option to handle
     ;; non-compliant RDF/XML documents.  Problem are empty typed
     ;; literals, e.g. literals of the form ‘""^^xsd:dateTime’.
     (let ((source-node (when source-uri (wilbur:node (string-from-uri source-uri)))))
       (labels ((thing (thing)
		  (etypecase thing
		    (null
		     (wilbur:node nil))
		    (puri:uri
		     (wilbur:node (string-from-uri thing)))
		    (cl-rdfxml:plain-literal
		     (let ((language (cl-rdfxml:literal-language thing)))
		       (wilbur:literal (cl-rdfxml:literal-string thing)
				       :language language)))
		    (cl-rdfxml:typed-literal
		     (let ((datatype (wilbur:node
				      (string-from-uri
				       (cl-rdfxml:literal-datatype thing)))))
		       (wilbur:literal (cl-rdfxml:literal-string thing)
				       :datatype datatype)))
		    (cl-rdfxml:blank-node
		     (wilbur:node nil))))
		(emit (subject predicate object)
		  (wilbur:add-triple (wilbur:triple
				      (thing subject)
				      (thing predicate)
				      (thing object))))
		(emit* (subject predicate object)
		  (wilbur:add-triple (wilbur:triple
				      (thing subject)
				      (thing predicate)
				      (thing object)
				      source-node))))
	 (let ((wilbur:*db* (or destination (make-instance 'wilbur:db))))
	   (cl-rdfxml:parse-document (if source-node #'emit* #'emit) source
				     :coerce-datatyped-empty-properties t)
	   wilbur:*db*))))
    ))

;;; wilbur.lisp ends here
