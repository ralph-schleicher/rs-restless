;;; fritz.lisp --- TR-064 protocol for AVM devices

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

;; See ‹https://avm.de/service/schnittstellen/›.

;;; Code:

(in-package :rs-restless)

(defvar *fritz-url* "http://fritz.box:49000/tr64desc.xml"
  "The default TR-064 device description URL.

For FRITZ!Box devices, protocol is either HTTP on port 49000
or HTTPS on port 49443.")

(defvar *fritz-realm* "F!Box SOAP-Auth"
  "The realm for content level authentication.")

(defvar *fritz-user-name* nil
  "The login name of the TR-064 service user.")

(defvar *fritz-secret* nil
  "The secret of the TR-064 service user.

See the ‘fritz-secret’ function for how to create the content level
authentication secret.")

(defun fritz-secret (user-name password &optional (realm *fritz-realm*))
  "Create the content level authentication secret.
Arguments USER-NAME, PASSWORD, and REALM are combined and a MD5 hash
value is calculated for it.

Return value is the base 16 encoded string representation of the hash
value."
  (check-type user-name string)
  (check-type password string)
  (check-type realm string)
  #-(and)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence 'ironclad:md5
                             (ironclad:ascii-string-to-byte-array
                              (~ user-name ":" realm ":" password))))
  (with-output-to-string (stream)
    (iter (for code :in-vector (md5:md5sum-string (~ user-name ":" realm ":" password)))
          (multiple-value-bind (high low)
              (truncate code 16)
            (write-char (char-downcase (digit-char high 16)) stream)
            (write-char (char-downcase (digit-char low 16)) stream)))))

(defun fritz-find-action (service-name action-name &key (device *upnp-device*) all)
  "Find an action by its service and action name.

First argument SERVICE-NAME is the service type suffix.
Second argument ACTION-NAME is the action name.
Keyword argument DEVICE is the UPnP root device object; default
 is the value of the ‘*upnp-device*’ special variable.
If keyword argument ALL is true, return all matching UPnP action
 objects as a list.

Return the first matching UPnP action object or ‘nil’ of no matching
action can be found."
  (check-type service-name string)
  (check-type action-name string)
  (check-type device upnp-root-device)
  (let ((needle (~ ":service:" service-name ":")))
    (if (not all)
        (iter (for action :in (%upnp-get device :action-list))
              (for service = (upnp-service action))
              (for haystack = (%upnp-get service :service-type))
              (and (search needle haystack :test #'string=)
                   (string= action-name (%upnp-get action :name))
                   (leave action)))
      (iter (for action :in (%upnp-get device :action-list))
            (for service = (upnp-service action))
            (for haystack = (%upnp-get service :service-type))
            (and (search needle haystack :test #'string=)
                 (string= action-name (%upnp-get action :name))
                 (collect action))))))

(defun fritz-invoke (action arguments &key (user-name *fritz-user-name*) (secret *fritz-secret*))
  "Send a control action.

First argument ACTION is an UPnP action object.
Second argument ARGUMENTS are the input arguments.  Value is
 an alist with cons cells of the form ‘(ARGUMENT-NAME . VALUE)’
 where ARGUMENT-NAME is the name of the input argument (a string)
 and VALUE is the corresponding input value.
Keyword arguments USER-NAME and SECRET are the credentials of
 the TR-064 service user.  Default is to use the values of the
 ‘*fritz-user-name*’ and ‘*fritz-secret*’ special variables
 respectively.

Return value is the alist of output arguments with cons cells of
the form ‘(ARGUMENT-NAME . VALUE)’ where ARGUMENT-NAME is the name
of the output argument (a string) and VALUE is the corresponding
output value.

Examples:

     (fritz-invoke (fritz-find-action \"DeviceInfo\" \"GetSecurityPort\") ())
      ⇒ ((\"NewSecurityPort\" . 49443))"
  (check-type action upnp-action)
  (let* ((service (upnp-service action))
         (device (upnp-device service))
         (service-type (%upnp-get service :service-type))
         (action-name (%upnp-get action :name))
         (document (with-drakma-response (body)
                       (soap-http-request
                        ;; Request URI.
                        (string-from-uri (merge-uri (upnp-control-uri service) (upnp-base-uri device)))
                        ;; SOAP message.
                        (with-soap-envelope (:version :soap-1.1
                                             :xml-namespaces `(("serv" . ,service-type))
                                             :xml-declaration t)
                          (cxml:with-element* ("serv" action-name)
                            (iter (for argument :in (upnp-input-arguments action))
                                  (for argument-name = (%upnp-get argument :name))
                                  (for value = (let ((cell (assoc argument-name arguments :test #'string=)))
                                                 (when (null cell)
                                                   (alexandria:simple-program-error "Missing input argument ‘~A’." argument-name))
                                                 (cdr cell)))
                                  (for data-type = (%upnp-get* argument :state-variable :data-type))
                                  (for string = (upnp-print-to-string value data-type))
                                  (collect (cxml:with-element argument-name
                                             (cxml:text string))))))
                        :additional-headers `(("SoapAction" . ,(upnp-soap-action action))))
                     (xml-parse body))))
    ;; Return output arguments as an alist.
    (iter (for argument :in (upnp-output-arguments action))
          (for argument-name = (%upnp-get argument :name))
          (for string = (xpath-required-string (format nil "//~A[1]" argument-name) document))
          (for data-type = (%upnp-get* argument :state-variable :data-type))
          (for value = (upnp-read-from-string string data-type))
          (collect (cons argument-name value)))))

;;; fritz.lisp ends here
