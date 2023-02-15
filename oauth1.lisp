;;; oauth1.lisp --- OAuth 1.0a user authorization

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

(defclass oauth1-client ()
  ((user-name
    :documentation "User name for the user authorization process."
    :accessor oauth1-user-name
    :initform nil)
   (password
    :documentation "Password for the user authorization process."
    :accessor oauth1-password
    :initform nil)
   (consumer-key
    :documentation "Consumer key with the service provider."
    :accessor oauth1-consumer-key
    :initform nil)
   (secret-key
    :documentation "Consumer secret with the service provider."
    :accessor oauth1-secret-key
    :initform nil)
   (request-token-url
    :documentation "URL to obtain an unauthorized request token."
    :accessor oauth1-request-token-url
    :initform nil)
   (user-authorization-url
    :documentation "URL to authorize the request token by the user."
    :accessor oauth1-user-authorization-url
    :initform nil)
   (access-token-url
    :documentation "URL to obtain an access token."
    :accessor oauth1-access-token-url
    :initform nil)
   (north-client
    :documentation "North client object."
    :accessor oauth1-north-client
    :initform nil)
   (cookie-jar
    :documentation "HTTP cookies; value is a Drakma ‘cookie-jar’ object."
    :accessor oauth1-cookie-jar
    :initform (make-instance 'drakma:cookie-jar)))
  (:documentation "Base class for OAuth 1.0a clients."))

(defun make-oauth1-client (&key (class 'oauth1-client) user-name password consumer-key secret-key)
  "Create an OAuth client."
  (unless (subtypep class 'oauth1-client)
    (error 'type-error :expected-type 'oauth1-client :datum class))
  (let ((client (make-instance class)))
    (when (or user-name password)
      (check-type user-name string)
      (check-type password string)
      (setf (oauth1-user-name client) user-name
	    (oauth1-password client) password))
    (when (or consumer-key secret-key)
      (check-type consumer-key string)
      (check-type secret-key string)
      (setf (oauth1-consumer-key client) consumer-key
	    (oauth1-secret-key client) secret-key))
    client))

(defgeneric oauth1-user-authorization-setup (client)
  (:documentation "Setup the OAuth user authorization process.")
  (:method ((client oauth1-client))
    (declare (ignore client))
    (values)))

(defgeneric oauth1-user-authorization-cleanup (client)
  (:documentation "Cleanup the OAuth user authorization process.")
  (:method ((client oauth1-client))
    (declare (ignore client))
    (values)))

(defgeneric oauth1-user-authorization-visit (client authorize-uri)
  (:documentation "Visit the OAuth user authorization URL.
Value is the verification code (a string).")
  (:method ((client oauth1-client) authorize-uri)
    (with-drakma-response (nil status-code nil effective-uri)
	(drakma:http-request authorize-uri
			     :cookie-jar (oauth1-cookie-jar client)
			     :want-stream t)
      (unless (= status-code 200)
	(error 'program-error))
      (flet ((extract (query)
	       ;; Extract the verification code from
	       ;; the query parameters of an URL.
	       (when (stringp query)
		 (cdr (assoc "oauth_verifier" (quri:url-decode-params query) :test #'string=)))))
	(etypecase effective-uri
	  (puri:uri
	   (extract (puri:uri-query effective-uri)))
	  (quri:uri
	   (extract (quri:uri-query effective-uri)))
	  (string
	   effective-uri))))))

(defmacro with-oauth1-user-authorization (client &body forms)
  "Execute the OAuth user authorization process.

First argument is an OAuth client object.
The body should call ‘oauth1-user-authorization-visit’."
  (once-only (client)
    `(progn
       (oauth1-user-authorization-setup ,client)
       (unwind-protect
	    (progn ,@forms)
	 (oauth1-user-authorization-cleanup ,client)))))

(defgeneric setup-oauth1-client (client)
  (:documentation "Setup the OAuth client for authentication.")
  (:method ((client oauth1-client))
    (declare (ignore client))
    (values)))

(defun verify-oauth1-client (client)
  ;; Perform the authentication.
  (with-oauth1-user-authorization client
    (let* ((north-client (oauth1-north-client client))
	   (authorize-uri (north:initiate-authentication north-client))
	   (oauth-verifier (oauth1-user-authorization-visit client authorize-uri)))
      (unless (stringp oauth-verifier)
	(error 'program-error))
      (north:complete-authentication north-client oauth-verifier)))
  (values))

(defun oauth1-http-request (request-uri
			    &rest arguments
			    &key client unsigned (method :get) parameters headers
			    &allow-other-keys)
  "Issue an OAuth 1.0a authorized HTTP request.

First argument REQUEST-URI is the requested URI.
Keyword argument CLIENT is the OAuth client object.
If keyword argument UNSIGNED is true, the HTTP request will not use
 the client's OAuth authentication credentials.  Otherwise, the HTTP
 request will be signed and authorized – including a silent refresh in
 case an authorization error occurs.
Keyword argument METHOD is the HTTP method, e.g. ‘:get’ or ‘:put’.
Keyword argument PARAMETERS is an alist of parameters.  The parameters
 are usually added to the query part of the request URI.  However, for
 a POST request, the parameters comprise the body of the request.
Keyword argument HEADERS is an alist of additional HTTP headers which
 should be sent with the request.
Remaining keyword arguments are forwarded to the underlying Drakma
 HTTP request.

Return the values of the Drakma HTTP request."
  (check-type client oauth1-client)
  (when (and (not unsigned) (null (oauth1-north-client client)))
    (setup-oauth1-client client)
    ;; Create the North OAuth client.
    (setf (oauth1-north-client client)
	  (make-instance 'north:client
	    :key (oauth1-consumer-key client)
	    :secret (oauth1-secret-key client)
	    :request-token-uri (oauth1-request-token-url client)
	    :authorize-uri (oauth1-user-authorization-url client)
	    :access-token-uri (oauth1-access-token-url client)))
    (verify-oauth1-client client))
  ;; Remove known keys – don't want to disable keyword argument
  ;; checking when calling ‘drakma:http-request’.
  (iter (for key :in '(:client :unsigned :method :parameters :headers))
	(iter (while (remf arguments key))))
  (labels ((http-request ()
	     ;; Like ‘north:make-signed-request’, but simply return
	     ;; all Drakma values instead of signaling an error.
	     (let* ((north-client (oauth1-north-client client))
		    (request (north:make-request
			      request-uri method
			      :headers headers
			      :oauth (when (not unsigned)
				       `((:oauth_consumer_key . ,(north:key north-client))
					 (:oauth_token . ,(north:token north-client)))))))
	       (unless (eq method :post)
		 ;; Parameters are part of the request URI.
		 (setf (north:parameters request) parameters))
	       (unless unsigned
		 (north:make-signed request (north:secret north-client) (north:token-secret north-client))
		 (north:make-authorized request))
	       (when (eq method :post)
		 ;; Parameters comprise the body of the request.
		 (setf (north:parameters request) parameters))
	       ;; Issue the HTTP request.
	       (apply #'drakma:http-request (north:url request)
		      :method method
		      :parameters (north:parameters request)
		      :url-encoder #'north:url-encode
		      :form-data (eq method :post)
		      :cookie-jar (oauth1-cookie-jar client)
		      :additional-headers (north:headers request)
		      arguments))))
    ;; Initial HTTP request.
    (multiple-value-bind (body status-code headers effective-uri stream closep reason-phrase)
	(http-request)
      ;; Handle authentication errors, e.g. due to an expired OAuth
      ;; access token.
      (when (and (= status-code 401) (not unsigned))
	;; Cleanup, see ‘with-drakma-response’.
	(when (and (streamp body) (open-stream-p body) closep)
	  (close body))
	;; Refresh OAuth authentication credentials – once only.
	(verify-oauth1-client client)
	;; Retry the request.
	(multiple-value-setq (body status-code headers effective-uri stream closep reason-phrase)
	  (http-request)))
      ;; Return values.
      (values body status-code headers effective-uri stream closep reason-phrase))))

;;; oauth1.lisp ends here
