;;; json.lisp --- JSON utilities

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

;;; Commentary:

;; https://www.json.org/json-en.html
;; https://phmarek.github.io/yason/
;;
;; JSON Values
;;
;;    * Use keywords ‘:true’, ‘:false’, and ‘:null’ for the JSON
;;      values ‘true’, ‘false’, and ‘null’ respectively.
;;
;;    * A JSON object is an alist with elements of the form
;;      ‘(KEY . VALUE)’.
;;
;;    * A JSON array is a vector.

;;; Code:

(in-package :yason)

(defmethod encode ((object (eql :true)) &optional (stream *standard-output*))
  (write-string "true" stream)
  object)

(defmethod encode ((object (eql :false)) &optional (stream *standard-output*))
  (write-string "false" stream)
  object)

(in-package :rs-restless)

(declaim (type (or null package) *json-keyword-package*))
(defvar *json-keyword-package* nil
  "Home package for the keys of JSON object members.
A value of ‘nil’ means that keys are strings.")

(defun json-object-key (name)
  "Return the JSON object key for the string NAME.
If the ‘*json-keyword-package*’ parameter is not null, keys for
JSON object members are symbols in that package.  Otherwise, keys
are strings."
  (check-type name string)
  (if (packagep *json-keyword-package*)
      (intern name *json-keyword-package*)
    name))

(defun json-decode (&optional (stream *standard-input*))
  "Read a JSON value and return it as Lisp data."
  (let ((data (yason:parse stream
			   :object-as :alist
			   :object-key-fn #'json-object-key
			   :json-arrays-as-vectors t
			   :json-booleans-as-symbols t
			   :json-nulls-as-keyword t)))
    (setf data (nsubst :true 'yason:true data :test #'eq))
    (setf data (nsubst :false 'yason:false data :test #'eq))
    data))

(defun json-decode-from-string (string)
  "Like ‘json-decode’ but read the JSON value from STRING."
  (with-input-from-string (stream string)
    (json-decode stream)))

(defun json-encode (data &optional (stream *standard-output*))
  "Print Lisp data as a JSON value.
This is the inverse of the ‘json-decode’ function."
  (let ((yason:*list-encoder* #'yason:encode-alist)
        (yason:*symbol-key-encoder* #'symbol-name))
    ;; FIXME: Indentation is only enabled for debugging purpose.
    ;; Disable it when going to production for the sake of speed.
    (with-open-stream (output (yason:make-json-output-stream stream :indent nil))
      (yason:encode data output))))

(defun json-encode-to-string (data)
  "Like ‘json-encode’ but return the output as a string."
  (with-output-to-string (stream)
    (json-encode data stream)))

;;; json.lisp ends here
