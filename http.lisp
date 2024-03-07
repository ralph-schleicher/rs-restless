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

(define-condition http-status (condition)
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
    :reader http-status-reason
    :documentation "The HTTP status reason phrase."))
  (:documentation "HTTP status information.
The base class for all HTTP status conditions.

Class precedence list:

     ‘http-status’, ‘condition’, ...")
  (:report (lambda (condition stream)
             (let ((code (http-status-code condition))
                   (reason (http-status-reason condition)))
               (format stream "HTTP status code ~A~@[ (~A)~]."
                       (or code "not available") reason)))))

(define-condition http-informational (http-status)
  ()
  (:documentation "HTTP status for an informational response.
A ‘http-informational’ condition indicates an interim response
for communicating connection status or request progress prior to
completing the requested action and sending a final response.
The status code should be in the range from 100 to 199.

Class precedence list:

     ‘http-informational’, ‘http-status’, ..."))

(define-condition http-successful (http-status)
  ()
  (:documentation "HTTP status for a successful response.
A ‘http-successful’ condition indicates that the client's request
was successfully received, understood, and accepted.  The status
code should be in the range from 200 to 299.

Class precedence list:

     ‘http-successful’, ‘http-status’, ..."))

(define-condition http-redirection (http-status)
  ()
  (:documentation "HTTP status for a redirection response.
A ‘http-redirection’ condition indicates that further action needs
to be taken by the user agent in order to fulfill the request.
The status code should be in the range from 300 to 399.

Class precedence list:

     ‘http-redirection’, ‘http-status’, ..."))

(define-condition http-client-error (http-status error)
  ()
  (:documentation "HTTP status for a client error response.
A ‘http-client-error’ condition indicates that the client seems to
have erred.  The status code should be in the range from 400 to 499.

Class precedence list:

     ‘http-client-error’, ‘http-status’, ‘error’, ..."))

(define-condition http-server-error (http-status error)
  ()
  (:documentation "HTTP status for a server error response.
A ‘http-server-error’ condition indicates that the server failed to
fulfill an apparently valid request.  The status code should be in
the range from 500 to 599.

Class precedence list:

     ‘http-server-error’, ‘http-status’, ‘error’, ..."))

(defun make-http-status (code &optional reason)
  "Create a ‘http-status’ condition.

First argument CODE is the HTTP status code.  Value should
 be a non-negative integer in the range from 100 to 599.
Optional second argument REASON is the HTTP status reason
 phrase.  Value has to be a string.  If argument REASON is
 omitted or ‘nil’, attempt to determine the reason phrase
 from the status code.

The returned condition is a sub-type of ‘http-status’ if CODE is
in the range from 100 to 599.  Otherwise, the return value is a
condition of type ‘http-status’."
  (check-type code (or null (integer 0)))
  (check-type reason (or null string))
  (make-condition (cond ((null code)
                         'http-status)
                        ((<= 100 code 199)
                         'http-informational)
                        ((<= 200 code 299)
                         'http-successful)
                        ((<= 300 code 399)
                         'http-redirection)
                        ((<= 400 code 499)
                         'http-client-error)
                        ((<= 500 code 599)
                         'http-server-error)
                        ('http-status))
                  :code code :reason (or reason (cdr (assoc code http-status-code-alist)))))

(defun ensure-http-status (code &rest arguments)
  "Check for certain HTTP status codes.

First argument CODE is the HTTP status code to be checked.
The remaining arguments specify the set of valid HTTP status
 codes.  Each argument has to be either an integer, a cons cell
 of the form ‘(LOW . HIGH)’, or the symbol of a ‘http-status’
 condition.

Return CODE if it is a member of the set of valid HTTP status
codes.  Otherwise, signal an error with a condition created by
calling ‘(make-http-status CODE)’.

Exceptional situations:

   * Signals a type error if an argument is not of the correct
     type."
  (check-type code (integer 0))
  (or (iter (for datum :in arguments)
            (cond ((integerp datum)
                   (when (= datum code)
                     (return code)))
                  ((typep datum '(cons integer integer))
                   (when (<= (car datum) code (cdr datum))
                     (return code)))
                  ((eq datum 'http-status)
                   (when (<= 100 code 599)
                     (return code)))
                  ((eq datum 'http-informational)
                   (when (<= 100 code 199)
                     (return code)))
                  ((eq datum 'http-successful)
                   (when (<= 200 code 299)
                     (return code)))
                  ((eq datum 'http-redirection)
                   (when (<= 300 code 399)
                     (return code)))
                  ((eq datum 'http-client-error)
                   (when (<= 400 code 499)
                     (return code)))
                  ((eq datum 'http-server-error)
                   (when (<= 500 code 599)
                     (return code)))
                  ((error 'type-error
                          :datum datum
                          :expected-type '(or integer
                                              (cons integer integer)
                                              (member http-informational
                                                      http-successful
                                                      http-redirection
                                                      http-client-error
                                                      http-server-error))))))
      (error (make-http-status code))))

(defvar *http-cookies* (make-instance 'drakma:cookie-jar)
  "A collection of HTTP cookies.")

(defun clear-http-cookies ()
  "Unconditionally delete all stored HTTP cookies.

Return the empty cookie jar.

See also the ‘*http-cookies*’ special variable."
  (etypecase *http-cookies*
    (drakma:cookie-jar
     (setf (drakma:cookie-jar-cookies *http-cookies*) ()))
    (null))
  *http-cookies*)

;; https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
(defvar *basic-authentication* nil
  "The default HTTP basic authentication credentials.")

(defun http-request (request-uri
                     &rest
                       arguments
                     &key
                       authentication-scheme
                       (basic-authentication *basic-authentication*)
                       (kerberos-login-token *kerberos-login-token*)
                       (kerberos-spn *kerberos-spn*)
                       (cookie-jar *http-cookies*)
                       additional-headers
                     &allow-other-keys)
  "Issue an HTTP request.

First argument REQUEST-URI is the requested URI.
Keyword argument AUTHENTICATION-SCHEME defines the HTTP authentication
 scheme for the request.  Value is either ‘nil’ for no authentication,
 ‘:basic’ for basic authentication, or ‘:negotiate’ for integrated
 Windows authentication.  Default is ‘nil’, i.e. an unauthorized HTTP
 request.
Keyword argument BASIC-AUTHENTICATION are the basic authentication
 credentials.  Default is the value of the ‘*basic-authentication*’
 special variable.
Keyword argument KERBEROS-LOGIN-TOKEN is the Kerberos login token
 for integrated Windows authentication.  Default is the value of
 the ‘*kerberos-login-token*’ special variable.
Keyword argument KERBEROS-SPN is the service principal name (SPN),
 i.e. the unique name of the service instance.  Default is the value
 of the ‘*kerberos-spn*’ special variable.
Keyword argument ADDITIONAL-HEADERS is an alist of HTTP headers.
 This is used to merge the ‘Authorization’ header of the selected
 authentication scheme.
Remaining keyword arguments are forwarded to the underlying Drakma
 HTTP request.

Return the values of the HTTP request."
  (check-type authentication-scheme (member nil :basic :negotiate))
  ;; Remove known keys.
  (iter (for key :in '(:authentication-scheme
                       :basic-authentication
                       :basic-authorization ;not used
                       :kerberos-login-token
                       :kerberos-spn
                       :cookie-jar
                       :additional-headers))
        (iter (while (remf arguments key))))
  (let (;; For Kerberos authentication.
        gss-context
        gss-token
        ;; Pointer to the ‘Authorization’ header.
        (authorization (assoc "Authorization" additional-headers :test #'string-equal)))
    (cond ((null authorization))
          ((null authentication-scheme)
           ;; No authentication scheme defined.
           ;; Remove the ‘Authorization’ header.
           (setf additional-headers (remove "Authorization" additional-headers :key #'car :test #'string-equal))
           ;; Clear pointer.
           (setf authorization nil)))
    (let ((cerberus:*current-user* kerberos-login-token))
      (case authentication-scheme
        (:basic
         (check-type basic-authentication string))
        (:negotiate
         (check-type kerberos-login-token kerberos-login-token)
         (check-type kerberos-spn string)
         (multiple-value-setq (gss-context gss-token)
           (gss:initialize-security-context
            (gss:acquire-credentials :kerberos kerberos-spn)))))
      (labels ((%http-request ()
                 ;; Merge the authorization header.
                 (let ((credentials (case authentication-scheme
                                      (:basic
                                       (~ "Basic " basic-authentication))
                                      (:negotiate
                                       (~ "Negotiate " (cl-base64:usb8-array-to-base64-string gss-token))))))
                   (when credentials
                     (if authorization
                         ;; Update the ‘Authorization’ header.
                         (setf (cdr authorization) credentials)
                       ;; Add the ‘Authorization’ header.
                       (progn
                         (setf authorization (cons "Authorization" credentials))
                         (push authorization additional-headers)))))
                 ;; Issue the HTTP request.
                 (apply #'drakma:http-request request-uri
                        :additional-headers additional-headers
                        :cookie-jar cookie-jar
                        arguments)))
        ;; Initial HTTP request.
        (multiple-value-bind (body status-code response-headers effective-uri stream closep reason-phrase)
            (%http-request)
          ;; Handle authentication errors, e.g. due to an expired access token.
          (when (and (= status-code 401) authentication-scheme)
            (let (retryp)
              (case authentication-scheme
                (:basic
                 ;; Existing credentials do not work.  Give up.
                 ())
                (:negotiate
                 (let* ((auth-data (cdr (assoc "WWW-Authenticate" response-headers :test #'string-equal)))
                        (buffer (let ((start 10) (end (length auth-data)))
                                  (when (and (< start end) (string= auth-data "Negotiate " :end1 start))
                                    (iter (for pos :from start :below end)
                                          (unless (position (char auth-data pos) "+/0123456789=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
                                            (setf end pos)
                                            (finish)))
                                    (when (< start end)
                                      (cl-base64:base64-string-to-usb8-array (subseq auth-data start end)))))))
                   (multiple-value-setq (gss-context gss-token)
                     (gss:initialize-security-context gss-context :buffer buffer))
                   (setf retryp t))))
              ;; Retry the request.
              (when retryp
                (cleanup-drakma-response body closep)
                (multiple-value-setq (body status-code response-headers effective-uri stream closep reason-phrase)
                  (%http-request)))))
          ;; Return values.
          (values body status-code response-headers effective-uri stream closep reason-phrase))))))

;;; http.lisp ends here
