;;; yason.lisp --- YASON hacks, until upstream is fixed

;; Copyright (C) 2022 Ralph Schleicher

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

(in-package :yason)

(export '*nil-encoder*)
(defvar *nil-encoder* #'encode-null
  "The actual function used to encode ‘nil’.
Default is ‘encode-null’, but ‘encode-false’ or ‘encode-list’ may be
appropriate, too.")

(export 'encode-true)
(defun encode-true (object &optional (stream *json-output*))
  "Constantly encode OBJECT as true."
  (write-string "true" stream)
  object)

(export 'encode-false)
(defun encode-false (object &optional (stream *json-output*))
  "Constantly encode OBJECT as false."
  (write-string "false" stream)
  object)

(export 'encode-null)
(defun encode-null (object &optional (stream *json-output*))
  "Constantly encode OBJECT as null."
  (write-string "null" stream)
  object)

(export 'encode-list)
(defun encode-list (object &optional (stream *json-output*))
  "Encode OBJECT as a list by calling ‘*list-encoder*’."
  (funcall *list-encoder* object stream))

(defmethod encode ((object (eql :true)) &optional (stream *standard-output*))
  (encode-true object stream))

(defmethod encode ((object (eql :false)) &optional (stream *standard-output*))
  (encode-false object stream))

(defmethod encode ((object (eql nil)) &optional (stream *json-output*))
  (funcall *nil-encoder* object stream))

(defun encode-alist (object &optional (stream *json-output*))
  "Encode OBJECT (an alist) as a JSON object."
  (with-aggregate/object (stream #\{ #\})
    (loop :for (key . value) :in object
          :do (with-element-output ()
		(encode-assoc-key/value key value stream)))
    object))

;;; yason.lisp ends here
