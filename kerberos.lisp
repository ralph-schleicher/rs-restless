;;; kerberos.lisp --- Kerberos authentication

;; Copyright (C) 2024 Ralph Schleicher

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

;;; Code:

(in-package :rs-restless)

(deftype kerberos-login-token ()
  "The type of a Kerberos login token."
  'cerberus::login-token)

(defvar *kerberos-enctypes* (list :aes256-cts-hmac-sha384-192
                                  :aes128-cts-hmac-sha256-128
                                  :aes256-cts-hmac-sha1-96
                                  :aes128-cts-hmac-sha1-96)
  "A list of encryption types in decreasing order of preference.
The encryption type aes256-cts-hmac-sha1-96 should be included in
this list since it is supported by all Kerberos implementations.
Call the ‘cerberus:list-all-profiles’ function to get a list of
supported encryption types.

See also ‘https://web.mit.edu/kerberos/krb5-current/doc/admin/enctypes.html’.")

(defvar *kerberos-kdc* nil
  "The Kerberos key distribution center.
Value is either the host name (a string) or the IP address (a vector
of integers) of the KDC.  On Windows, the KDC is an Active Directory
domain controller.

How to find out the host name or IP address of the KDC?

     C:> nslookup.exe -type=srv _kerberos._tcp.example.com
     Server:  ns1.example.com
     Address:  10.1.1.1

     _kerberos._tcp.example.com      SRV service location:
               priority       = 0
               weight         = 100
               port           = 88
               svr hostname   = dc1.example.com
     dc1.example.com                 internet address = 10.1.1.3")

(defvar *kerberos-spn* nil
  "The default service principal name.

How to find out the SPN for ‘https://app.example.com:8443/rest-api/’
on Windows?

     C:> setspn.exe -Q */app.example.com
     Checking domain DC=example,DC=com

     No such SPN found.

Hmm, examine the host name:

     C:> nslookup.exe app.example.com
     Server:  ns1.example.com
     Address:  10.1.1.1

     Name:    host.example.com
     Address:  10.1.3.23
     Aliases:  app.example.com

Try again with the canonical host name:

     C:> setspn.exe -Q */host.example.com
     Checking domain DC=example,DC=com
     CN=...,DC=example,DC=com
             RestrictedKrbHost/host.example.com
             HOST/host.example.com
             RestrictedKrbHost/host
             HOST/host

     Existing SPN found!

So, the SPN is, e.g. ‘HOST/host.example.com@EXAMPLE.COM’.
You have to check which SPN actually works.")

(defvar *kerberos-login-token* nil
  "The default Kerberos login token.")

(defun make-kerberos-user (principal password &key enctype)
  "Create a Kerberos login token for a user.

First argument PRINCIPAL is the principal name of the user.  Value is
 a string of the form USER@REALM where USER is the user's login name
 and REALM is the name of the Kerberos realm.  The later is usually
 equal to the domain name in uppercase letters.
Second argument PASSWORD is the user's login password.
Keyword argument ENCTYPE defines the encryption type.  Default is to
 try all encryption types listed in the ‘*kerberos-enctypes*’ special
 variable.

The value of the ‘*kerberos-kdc*’ special variable defines the key
distribution center.

Return value is the Kerberos login token.  Signal an error if the
request fails."
  (let ((kdc-address (labels ((string-from-ipv4 (addr)
                                (format nil "~D.~D.~D.~D" (aref addr 0) (aref addr 1) (aref addr 2) (aref addr 3)))
                              (string-from-addr (addr)
                                (ecase (length addr)
                                  (4
                                   (string-from-ipv4 addr)))))
                       (etypecase *kerberos-kdc*
                         (string
                          (string-from-addr (usocket:get-host-by-name *kerberos-kdc*)))
                         ((array (integer 0) (4))
                          (string-from-ipv4 *kerberos-kdc*)))))
        (cerberus:*current-user* nil))
    (iter (with profiles = (cerberus:list-all-profiles))
          (for profile :in (if enctype (list enctype) *kerberos-enctypes*))
          (for match = (member profile profiles :test #'string-equal))
          (when (null match)
            (next-iteration))
          (ignore-errors
           (cerberus:logon-user principal password :kdc-address kdc-address :etype (first match)))
          (until cerberus:*current-user*))
    (or cerberus:*current-user*
        (error "Failed to request a login token for ‘~A’." principal))))

;;; kerberos.lisp ends here
