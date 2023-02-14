;;; packages.lisp --- package definitions

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

(in-package :common-lisp-user)

(defpackage #:de.ralph-schleicher.restless
  (:nicknames :rs-restless)
  (:use :common-lisp :iterate)
  (:intern
   ;; common.lisp
   #:defconst
   #:defsubst
   #:~
   #:P)
  (:export
   ;; drakma.lisp
   #:trace-drakma-requests
   #:with-drakma-response
   ;; uri.lisp
   #:string-from-uri
   ;; json.lisp
   #:*json-keyword-package*
   #:json-object-key
   #:json-decode
   #:json-decode-from-string
   #:json-encode
   #:json-encode-to-string
   ;; xml.lisp
   #:*xml-document*
   #:*xml-namespaces*
   #:xmlns-prefix
   #:xmlns-name
   ;; rdf.lisp
   #:*rdf-triples*
   #:parse-rdf/xml
   ;; wilbur.lisp
   #:wilbur-add-namespaces
   #:wilbur-parse-rdf/xml
   ;; oauth1.lisp
   #:make-oauth1-client
   #:oauth1-client
   #:oauth1-user-name
   #:oauth1-password
   #:oauth1-consumer-key
   #:oauth1-secret-key
   #:oauth1-request-token-url
   #:oauth1-user-authorization-url
   #:oauth1-access-token-url
   #:oauth1-user-authorization-setup
   #:oauth1-user-authorization-cleanup
   #:oauth1-http-request
   ;; oslc.lisp
   #:*oslc-client*
   #:make-oslc-client
   #:oslc-client
   #:oslc-root-services-url
   #:oslc-invoke
   #:oslc-service-provider-catalogs
   #:browse-oslc-service-provider-catalog
   ;; doors.lisp
   #:doors-client)
  (:documentation
   "A Common Lisp library for communicating with web services.

The RESTless library provides convenience functions for HTTP
requests/responses, URI handling, JSON encoding/decoding, RDF/XML
parsing, OAuth 1.0a user authorization, OSLC clients, and more."))

(defpackage #:de.ralph-schleicher.restless-user
  (:nicknames :rs-restless-user)
  (:use :common-lisp :iterate
	:rs-restless)
  (:import-from :rs-restless
		#:defconst
		#:defsubst
		#:~
		#:P)
  (:documentation
   "User package with RESTless bindings."))

;;; packages.lisp ends here
