;;; drakma.lisp --- Drakma extensions

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

(eval-when (:load-toplevel :execute)
  (setf drakma:*drakma-default-external-format* :utf-8))

(defun around-drakma-http-request (next-function request-uri &rest arguments)
  (multiple-value-bind (body status-code headers effective-uri stream closep reason-phrase)
      (apply next-function request-uri arguments)
    (let ((method (or (getf arguments :method) :get))
	  (parameters (getf arguments :parameters))
	  (headers* (getf arguments :additional-headers))
	  (cookie-jar (getf arguments :cookie-jar)))
      (iter (for key :in '(:method :parameters :additional-headers :cookie-jar))
	    (iter (while (remf arguments key))))
      (format *trace-output* "== REQUEST =============================~%")
      (format *trace-output* (~ "Method: ~S~%"
				"URI: ~S~%"
				"Parameters: ~S~%"
				"Headers: ~S~%"
				"Cookies: ~S~%"
				"Other: ~S~%")
	      method request-uri parameters headers* cookie-jar arguments)
      (format *trace-output* "== RESPONSE ============================~%")
      (format *trace-output* (~ "URI: ~S~%"
				"Status: ~S, ~A~%"
				"Headers: ~S~%")
	      effective-uri status-code reason-phrase headers)
      (format *trace-output* "== BODY ================================~%")
      (format *trace-output* "~S~%" body)
      (format *trace-output* "========================================~%"))
    ;; Return values.
    (values body status-code headers effective-uri stream closep reason-phrase)))

(defun trace-drakma-requests (&optional (enable t))
  "Enable or disable tracing of Drakma HTTP requests."
  (cond (enable
	 (unless (cl-advice:advisable-function-p #'drakma:http-request)
	   (cl-advice:make-advisable 'drakma:http-request))
	 (cl-advice:add-advice :around #'drakma:http-request
			       'around-drakma-http-request))
	(t
	 (when (cl-advice:advisable-function-p #'drakma:http-request)
	   (cl-advice:remove-advice :around #'drakma:http-request
				    'around-drakma-http-request)))))

(defmacro with-drakma-response ((&optional body-var status-code-var headers-var url-var reason-phrase-var) request &body forms)
  "Wrapper for a Drakma HTTP request.

Optional arguments BODY-VAR, STATUS-CODE-VAR, HEADERS-VAR, URL-VAR,
 and REASON-PHRASE-VAR are symbols for binding the corresponding
 return value of the ‘drakma:http-request’ function; ‘nil’ means to
 ignore the value.
Second argument REQUEST is any form returning the same values as a
 ‘drakma:http-request’ function call.
Remaining arguments FORMS is the macro body.

If the body, i.e. the primary value, of the HTTP request is a stream,
the ‘with-drakma-response’ macro ensures that it will be closed."
  (let ((body (gensym "BODY"))
        (status (gensym "STATUS"))
        (headers (gensym "HEADERS"))
        (location (gensym "LOCATION"))
        (stream (gensym "STREAM"))
        (closep (gensym "CLOSEP"))
        (reason (gensym "REASON")))
    `(multiple-value-bind (,body ,status ,headers ,location ,stream ,closep ,reason)
         ,request
       (declare (ignorable ,status ,headers ,location ,reason) (ignore ,stream))
       (unwind-protect
            (let (,@(when body-var (list (list body-var body)))
                  ,@(when status-code-var (list (list status-code-var status)))
                  ,@(when headers-var (list (list headers-var headers)))
                  ,@(when url-var (list (list url-var location)))
                  ,@(when reason-phrase-var (list (list reason-phrase-var reason))))
              ,@forms)
	 (cleanup-drakma-response ,body ,closep)))))

(defun cleanup-drakma-response (body closep)
  "Cleanup code for a Drakma HTTP request.
If the body, i.e. the primary value of a Drakma HTTP request is a
stream, ensure that it is closed unless the second argument CLOSEP
is false."
  (when (and (streamp body) (open-stream-p body) closep)
    (close body))
  (values))

;;; drakma.lisp ends here
