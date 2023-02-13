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
namespace name.")

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

;;; xml.lisp ends here
