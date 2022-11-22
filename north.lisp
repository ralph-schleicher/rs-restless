;;; north.lisp --- North hacks

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

(in-package #:org.shirakumo.north)

(defmethod call ((request request) &rest drakma-args)
  (let ((drakma:*text-content-types*
          (list* '("application" . "x-www-form-urlencoded")
                 drakma:*text-content-types*)))
    (multiple-value-bind (body status-code headers effective-uri stream closep reason-phrase)
        (apply #'drakma:http-request
               (url request)
               :method (http-method request)
               :parameters (parameters request)
               :additional-headers (headers request)
               :url-encoder #'url-encode
               :external-format-in *external-format*
               :external-format-out *external-format*
               drakma-args)
      (declare (ignorable stream closep))
      #-(and)
      (progn
	(format *trace-output* "== REQUEST (OAUTH) =====================~%")
	(format *trace-output* (rs-restless::~
				"Method: ~S~%"
				"URI: ~S~%"
				"Parameters: ~S~%"
				"Headers: ~S~%")
		(http-method request)
		(url request)
		(parameters request)
		(headers request))
	(format *trace-output* "== RESPONSE (OAUTH) ====================~%")
	(format *trace-output* (rs-restless::~
				"URI: ~S~%"
				"Status: ~S, ~S~%"
				"Headers: ~S~%")
		effective-uri status-code reason-phrase headers)
	(format *trace-output* "== BODY (OAUTH) ========================~%")
	(format *trace-output* "~S~%" body)
	(format *trace-output* "========================================~%"))
      (unless (<= 200 status-code 299)
        (error 'request-failed :request request :body body :status-code status-code :headers headers))
      body)))

;;; north.lisp ends here
