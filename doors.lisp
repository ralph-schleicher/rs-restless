;;; doors.lisp --- DOORS Web Access client

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

(defclass doors-client (oslc-client)
  ()
  (:documentation "DOORS Web Access client.

DOORS Web Access provides an OSLC interface for the DOORS requirements
management system."))

(defmethod initialize-instance :after ((client doors-client) &rest arguments &key &allow-other-keys)
  (declare (ignore arguments))
  ;; DOORS uses non-standard XML tags in the root services document.
  (setf (oauth-request-token-tag client)
        "http://jazz.net/xmlns/prod/jazz/jfs/1.0/oauthRequestTokenUrl"
        (oauth-user-authorization-tag client)
        "http://jazz.net/xmlns/prod/jazz/jfs/1.0/oauthUserAuthorizationUrl"
        (oauth-access-token-tag client)
        "http://jazz.net/xmlns/prod/jazz/jfs/1.0/oauthAccessTokenUrl"))

(defmethod oauth1-user-authorization-setup ((client doors-client))
  ;; Non-interactive user sign on.  See the source code of the DOORS
  ;; Web Access login page.  This has to happen in the same session as
  ;; the rest of the OAuth user authorization.
  (let ((user-name (oauth1-user-name client))
        (password (oauth1-password client)))
    (when (and (stringp user-name)
               (stringp password))
      (with-drakma-response (nil status-code)
          (drakma:http-request (~ (base-url client) "/j_acegi_security_check")
                               :method :post
                               :parameters `(("j_username" . ,user-name)
                                             ("j_password" . ,password))
                               :cookie-jar (oauth1-cookie-jar client)
                               :want-stream t)
        (unless (= status-code 200)
          (error (make-http-status status-code)))))))

;;; doors.lisp ends here
