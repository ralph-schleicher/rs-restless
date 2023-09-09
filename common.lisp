;;; common.lisp --- common definitions

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

(defmacro defconst (name value &optional doc)
  "Define a constant variable.

This is like ‘defconstant’ except that the initially set value
is reused when the ‘defconst’ form is evaluated again."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro defsubst (name arg-list &body body)
  "Define an inline function.

This is like ‘defun’ except that the function is globally marked
for inline expansion by the compiler."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,arg-list
       ,@body)))

(defun should-not-happen ()
  "Signal an impossible condition."
  (alexandria:simple-program-error "Should not happen."))

(defun missing-initialization-argument ()
  "Signal a missing initialization argument program error.
Example usage:

     (defclass foo ()
       ((bar
         :initarg :bar
         :initform (missing-initialization-argument)
         :documentation \"A required object initialization argument.\")))"
  (alexandria:simple-program-error "Missing initialization argument."))

(defun whitespace-char-p (char)
  "Return true if CHAR is a whitespace character.
Argument CHAR has to be a character object."
  (declare (type character char))
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Linefeed)
      (char= char #\Return)
      (char= char #\Newline)))

(defun bounding-indices-if-not (predicate seq &key (start 0) end key)
  "Return start index of first element in SEQ and end index of last element
in SEQ not matching PREDICATE.

Keywords START and END are bounding index designators.
Keyword KEY is a function designator of one argument.

If all elements match PREDICATE, return the start index positions.
Likewise if SEQ is empty."
  (when (null end)
    (setf end (length seq)))
  (let* ((left (or (position-if-not predicate seq :start start :end end :key key) end))
	 (right (position-if-not predicate seq :from-end t :start left :end end :key key)))
    (setf right (if right (1+ right) start))
    (if (< left right)
	(values left right)
      (values start start))))

(defun make-keyword (name)
  "Create a symbol in the keyword package.
Argument NAME must be a string designator."
  (intern (string-upcase (string name)) (find-package :keyword)))

(defun ~ (&rest strings)
  "Concatenate all arguments as strings.
Arguments must be string designators.
Null arguments are ignored."
  (apply #'concatenate 'string (mapcar #'string (delete nil strings))))

(defun P (&rest strings)
  "Like ‘~’ but return a pathname."
  (parse-namestring (apply #'~ strings)))

;; Iterate driver for XPath node sets.
(defmacro-driver (for var in-node-set node-set)
  "Elements of a ‘xpath:node-set’."
  (let ((node-set-iter (gensym "NODE-SET-ITERATOR-")))
    `(progn
       (with ,node-set-iter = (xpath:make-node-set-iterator ,node-set))
       (,(if generate 'generate 'for) ,var
        :next (progn
                (when (xpath:node-set-iterator-end-p ,node-set-iter)
                  (terminate))
                (prog1
                    (xpath:node-set-iterator-current ,node-set-iter)
                  (xpath:node-set-iterator-next ,node-set-iter)))))))

;;; common.lisp ends here
