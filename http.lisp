;;; http.lisp --- HTTP utilities

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

;;; Code:

(in-package :rs-restless)

;; See ‹https://www.iana.org/assignments/http-status-codes›.
(defconst http-status-code-alist
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (102 . "Processing")
    (103 . "Early Hints")
    (200 . "OK")
    (201 . "Created")
    (202 . "Accepted")
    (203 . "Non-Authoritative Information")
    (204 . "No Content")
    (205 . "Reset Content")
    (206 . "Partial Content")
    (207 . "Multi-Status")
    (208 . "Already Reported")
    (226 . "IM Used")
    (300 . "Multiple Choices")
    (301 . "Moved Permanently")
    (302 . "Found")
    (303 . "See Other")
    (304 . "Not Modified")
    (305 . "Use Proxy")
    (306 . "(Unused)")
    (307 . "Temporary Redirect")
    (308 . "Permanent Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (402 . "Payment Required")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (406 . "Not Acceptable")
    (407 . "Proxy Authentication Required")
    (408 . "Request Timeout")
    (409 . "Conflict")
    (410 . "Gone")
    (411 . "Length Required")
    (412 . "Precondition Failed")
    (413 . "Content Too Large")
    (414 . "URI Too Long")
    (415 . "Unsupported Media Type")
    (416 . "Range Not Satisfiable")
    (417 . "Expectation Failed")
    (418 . "(Unused)")
    (421 . "Misdirected Request")
    (422 . "Unprocessable Content")
    (423 . "Locked")
    (424 . "Failed Dependency")
    (425 . "Too Early")
    (426 . "Upgrade Required")
    (428 . "Precondition Required")
    (429 . "Too Many Requests")
    (431 . "Request Header Fields Too Large")
    (451 . "Unavailable For Legal Reasons")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout")
    (505 . "HTTP Version Not Supported")
    (506 . "Variant Also Negotiates")
    (507 . "Insufficient Storage")
    (508 . "Loop Detected")
    (510 . "Not Extended")
    (511 . "Network Authentication Required"))
  "Alist of HTTP status codes.
List elements are cons cells of the form ‘(CODE . REASON)’
where CODE is the HTTP status code (a non-negative integer)
and REASON is the associated reason phrase (a string).")

(define-condition http-status ()
  ((code
    :initarg :code
    :initform nil
    :type (or null (integer 0))
    :reader http-status-code
    :documentation "The HTTP status code.")
   (reason
    :initarg :reason
    :initform nil
    :type (or null string)
    :reader http-reason-phrase
    :documentation "The HTTP status reason phrase."))
  (:documentation "HTTP status information.")
  (:report (lambda (condition stream)
	     (let* ((code (http-status-code condition))
		    (reason (or (http-reason-phrase condition)
				(cdr (assoc code http-status-code-alist)))))
	       (format stream "HTTP status code ~A~@[ ‘~A’~]."
		       (or code "not available") reason)))))

(define-condition http-informational (http-status) ())
(define-condition http-successful (http-status) ())
(define-condition http-redirection (http-status) ())
(define-condition http-client-error (http-status) ())
(define-condition http-server-error (http-status) ())

(defun make-http-status (status-code &optional reason-phrase)
  "Create a ‘http-status’ condition."
  (check-type status-code (or null (integer 0)))
  (check-type reason-phrase (or null string))
  (make-condition (cond ((null status-code)
			 'http-status)
			((<= 100 status-code 199)
			 'http-informational)
			((<= 200 status-code 299)
			 'http-successful)
			((<= 300 status-code 399)
			 'http-redirection)
			((<= 400 status-code 499)
			 'http-client-error)
			((<= 500 status-code 599)
			 'http-server-error)
			('http-status))
		  :code status-code
		  :reason reason-phrase))

;;; http.lisp ends here
