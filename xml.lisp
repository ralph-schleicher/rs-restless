;;; xml.lisp --- XML utilities

;; Copyright (C) 2023 Ralph Schleicher

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

;;; Commentary:

;;; Code:

(in-package :rs-restless)

(defvar *xml-document* nil
  "Convenience variable for binding an XML document.")

(defvar *xml-namespaces*
  '(("xml" . "http://www.w3.org/XML/1998/namespace")
    ("xs" . "http://www.w3.org/2001/XMLSchema")
    ("xsd" . "http://www.w3.org/2001/XMLSchema")
    ("xsi" . "http://www.w3.org/2001/XMLSchema-instance")
    ("xsl" . "http://www.w3.org/1999/XSL/Transform")
    ("xslt" . "http://www.w3.org/1999/XSL/Transform"))
  "Alist of well known XML namespaces.
List elements are cons cells of the form ‘(PREFIX . NAME)’ where
PREFIX is the common namespace prefix and NAME is the standardized
namespace name.

The predefined namespace prefixes are ‘xml’ for the XML namespace,
‘xs’ and ‘xsd’ for the XML schema namespace, ‘xsi’ for the XML schema
instance namespace, and ‘xsl’ and ‘xslt’ for the XSL transformation
namespace.")

(defgeneric xmlns-get-prefix (object)
  (:documentation
   "Return the namespace prefix of an XML namespace object."))

(defgeneric xmlns-get-name (object)
  (:documentation
   "Return the namespace name of an XML namespace object."))

(defmethod xmlns-get-prefix ((object cons))
  (car object))

(defmethod xmlns-get-name ((object cons))
  (if (consp (cdr object))
      (cadr object)
    (cdr object)))

(defun xmlns-prefix (object)
  "Return the namespace prefix of an XML namespace object.

Argument OBJECT is the XML namespace object.

Return value is a string.

Exceptional situations:

   * Signals a type error if the value returned by the
     ‘xmlns-get-prefix’ function is not a string."
  (let ((prefix (xmlns-get-prefix object)))
    (when (not (stringp prefix))
      (error 'type-error :datum prefix :expected-type 'string))
    prefix))

(defun xmlns-name (object)
  "Return the namespace name of an XML namespace object.

Argument OBJECT is the XML namespace object.

Return value is a string.

Exceptional situations:

   * Signals a type error if the value returned by the
     ‘xmlns-get-name’ function is not a string."
  (let ((name (xmlns-get-name object)))
    (when (not (stringp name))
      (error 'type-error :datum name :expected-type 'string))
    name))

(defun xml-parse (source &rest options &key (result-type 'dom:document) &allow-other-keys)
  "Parse an XML document.

First argument SOURCE is the input object.  Value is either a stream,
 a string, or a pathname.  The symbol ‘t’ is equal to the value of the
 ‘*standard-input*’ special variable.
Keyword argument RESULT-TYPE specifies the Lisp representation of the
 XML document.  Value is either ‘:dom’ for a ‘dom:document’ or ‘:stp’
 for a ‘stp:document’.  The default is to create a ‘dom:document’.
Remaining keyword arguments are forwarded to the underlying
 ‘cxml:parse’ function.

Return value is the Lisp representation of the XML document."
  ;; Remove known keys.
  (iter (while (remf options :result-type)))
  (apply #'cxml:parse
         (if (eq source t) *standard-input* source)
         (ecase result-type
           ((:dom dom:document)
            (cxml-dom:make-dom-builder))
           ((:stp stp:document)
            (stp:make-builder)))
         options))

(defun xml-serialize (destination document &rest options &key encoding canonical (declaration t) indentation &allow-other-keys)
  "Serialize the Lisp representation of an XML document.

First argument DESTINATION is the output object.  Value is either
 a stream, a string, or a pathname.  The symbol ‘t’ is equal to
 ‘*standard-output*’ and ‘nil’ means to return a string.
Second argument DOCUMENT is the Lisp representation of the XML
 document to be serialized.  Value is either a ‘dom:document’ or
 a ‘stp:document’ object.
Keyword arguments ENCODING, CANONICAL, DECLARATION, and INDENTATION
 are used to initialize a CXML sink object.
Remaining keyword arguments are forwarded to the underlying DOM
 mapping function.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a string containing the
XML document."
  ;; Remove known keys.
  (iter (for key :in '(:encoding :canonical :declaration :indentation))
        (iter (while (remf options key))))
  (flet ((%print (stream)
           (let* ((sink-options (nconc (when encoding
                                         (list :encoding encoding))
                                       (when canonical
                                         (list :canonical canonical))
                                       (when (null declaration)
                                         (list :omit-xml-declaration-p t))
                                       (when indentation
                                         (list :indentation indentation))))
                  (sink (if (null stream)
                            (apply #'cxml:make-string-sink sink-options)
                          (let ((element-type (stream-element-type stream)))
                            (cond ((subtypep element-type 'character)
                                   (apply #'cxml:make-character-stream-sink stream sink-options))
                                  ((subtypep element-type '(unsigned-byte 8))
                                   (apply #'cxml:make-octet-stream-sink stream sink-options))
                                  ((error 'type-error :datum element-type :expected-type '(or character (unsigned-byte 8)))))))))
             (etypecase document
               (dom:document
                (apply #'dom:map-document sink document
                       (nconc (when canonical
                                (list :include-doctype :canonical-notations))
                              options)))
               (stp:document
                (stp:serialize document sink))))))
    (etypecase destination
      (stream
       (%print destination) nil)
      (string
       (with-output-to-string (stream destination)
         (%print stream) nil))
      (pathname
       (with-open-file (stream destination :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede :if-does-not-exist :create)
         (%print stream) nil))
      ((member t)
       (%print *standard-output*) nil)
      (null
       (%print nil)))))

(defun xpath-ensure-node (object)
  "Ensure that OBJECT is an XML node."
  (or (typecase object
        (xpath:node-set
         (xpath:first-node object))
        (dom:node
         object))
      (error "Object ‘~A’ is not a node." object)))

(defun xpath-required-node (xpath context &optional (value #'identity))
  "Evaluate an XPath expression and return the first node."
  (funcall value (xpath-ensure-node (xpath:evaluate xpath context))))

(defun xpath-required-string (xpath context)
  "Like ‘xpath-required-node’ but return the node's text value."
  (xpath-required-node xpath context #'xpath:string-value))

(defun xpath-optional-node (xpath context &optional (value #'identity))
  "Evaluate an XPath expression and return the first node or ‘nil’."
  (when-let ((node (xpath:first-node (xpath:evaluate xpath context))))
    (funcall value node)))

(defun xpath-optional-string (xpath context)
  "Like ‘xpath-optional-node’ but return the node's text value."
  (xpath-optional-node xpath context #'xpath:string-value))

;;; xml.lisp ends here
