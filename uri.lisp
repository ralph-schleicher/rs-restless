;;; uri.lisp --- URI utilities

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

(in-package :rs-restless)

;; Make Clozure CL happy, see ‹https://github.com/fukamachi/quri/issues/82›.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (ignore-errors (find-method #'make-load-form () (mapcar #'find-class '(quri:uri))))
    (defmethod make-load-form ((object quri:uri) &optional environment)
      (make-load-form-saving-slots object :environment environment))))

(defconst root-puri (puri:uri "/")
  "Root path URL.")

(defconst root-quri (quri:uri "/")
  "Root path URL.")

(defgeneric string-from-uri (uri)
  (:documentation
   "Return the string representation of an URI."))

(defmethod string-from-uri ((uri puri:uri))
  (with-output-to-string (stream)
    (puri:render-uri uri stream)))

(defmethod string-from-uri ((uri quri:uri))
  (with-output-to-string (stream)
    (quri:render-uri uri stream)))

(defmethod string-from-uri ((uri string))
  uri)

(defun puri-designator (uri)
  "Return either an object of type ‘puri:uri’ or a string."
  (if (puri:uri-p uri) uri (string-from-uri uri)))

(defun quri-designator (uri)
  "Return either an object of type ‘quri:uri’ or a string."
  (if (quri:uri-p uri) uri (string-from-uri uri)))

(defun ensure-puri (uri)
  "Return an object of type ‘puri:uri’."
  (if (puri:uri-p uri) uri (puri:uri (string-from-uri uri))))

(defun ensure-quri (uri)
  "Return an object of type ‘quri:uri’."
  (if (quri:uri-p uri) uri (quri:uri (string-from-uri uri))))

(defgeneric merge-uri (reference-uri base-uri)
  (:documentation
   "Merge a reference URI into the base URI."))

(defmethod merge-uri ((reference-uri puri:uri) base-uri)
  (puri:merge-uris reference-uri (ensure-puri base-uri)))

(defmethod merge-uri ((reference-uri quri:uri) base-uri)
  (quri:merge-uris reference-uri (ensure-quri base-uri)))

(defmethod merge-uri ((reference-uri string) (base-uri puri:uri))
  (puri:merge-uris (puri:uri reference-uri) base-uri))

(defmethod merge-uri ((reference-uri string) (base-uri quri:uri))
  (quri:merge-uris (quri:uri reference-uri) base-uri))

;;; uri.lisp ends here
