;;; oslc.lisp --- Open Services for Lifecycle Collaboration

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

(defparameter *oslc-namespaces*
  '(;; Dublin Core Metadata Initiative (DCMI) properties.
    ("dcterms" . "http://purl.org/dc/terms/")
    ;; FOAF (friend of a friend) namespace.
    ("foaf" . "http://xmlns.com/foaf/0.1/")
    ;; Jazz Discovery Services XML namespace.
    ("jd" . "http://jazz.net/xmlns/prod/jazz/discovery/1.0/")
    ;; Jazz Foundation Services XML namespace.
    ("jfs" . "http://jazz.net/xmlns/prod/jazz/jfs/1.0/")
    ;; OSLC 2.0 namespaces.
    ("oslc" . "http://open-services.net/ns/core#")
    ("oslc_rm" . "http://open-services.net/ns/rm#")
    ;; Web Ontology Language namespace.
    ("owl" . "http://www.w3.org/2002/07/owl#")
    ;; RDF/XML namespaces.
    ("rdf" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ("rdfs" . "http://www.w3.org/2000/01/rdf-schema#")
    ;; XSD namespace.
    ("xsd" . "http://www.w3.org/2001/XMLShema#"))
  "Well known OSLC namespaces.")

(eval-when (:load-toplevel :execute)
  (wilbur-add-namespaces *oslc-namespaces*))

(defparameter *oslc-media-types*
  '(("application" . "rdf+xml")
    ("application" . "xml")
    ("application" . "json")
    ;; OSLC RM 1.0 media types.
    ("application" . "x-oslc-rm-requirement-1.0+xml")
    ("application" . "x-oslc-rm-requirement-collection-1.0+xml")
    ("application" . "x-oslc-rm-service-description-1.0+xml")
    ("application" . "x-oslc-disc-service-provider-catalog+xml"))
  "Well known OSLC media types.")

(defparameter *oslc-text-content-types*
  (list* '("text" . nil)
	 ;; For OAuth.
	 '("application" . "x-www-form-urlencoded")
	 ;; For OSLC.
	 *oslc-media-types*)
  "Well known text content types.")

(defmacro with-oslc-defaults (&body forms)
  `(let ((north:*external-format* :utf-8)
	 (drakma:*drakma-default-external-format* :utf-8)
	 (drakma:*text-content-types* *oslc-text-content-types*))
     ,@forms))

(defvar *oslc-client* nil
  "The default OSLC client.")

(defclass oslc-client (oauth1-client)
  ((base-url
    :accessor base-url
    :initform nil
    :documentation "Base URL of the OSLC service provider.")
   (root-services
    :accessor root-services
    :initform "/public/rootservices"
    :documentation "End point of the OSLC root services document.")
   (oslc-core-version
    :accessor oslc-core-version
    :initform "2.0")
   (oauth-request-token-tag
    :accessor oauth-request-token-tag
    :initform "http://open-services.net/ns/core#oauthRequestTokenURI")
   (oauth-user-authorization-tag
    :accessor oauth-user-authorization-tag
    :initform "http://open-services.net/ns/core#authorizationURI")
   (oauth-access-token-tag
    :accessor oauth-access-token-tag
    :initform "http://open-services.net/ns/core#oauthAccessTokenURI")))

(defun make-oslc-client (base-url &rest arguments &key (class 'oslc-client) &allow-other-keys)
  "Create an OSLC client.

The base URL must not end with a slash."
  (check-type base-url string)
  (unless (subtypep class 'oslc-client)
    (error 'type-error :expected-type 'oslc-client :datum class))
  (let ((client (apply #'make-oauth1-client arguments)))
    (setf (base-url client) base-url)
    client))

(defun oslc-root-services-url (&optional (client *oslc-client*))
  "Return the URL of the OSLC root services document."
  (check-type client oslc-client)
  (with-slots (base-url root-services) client
    (~ base-url root-services)))

(defmethod setup-oauth1-client ((client oslc-client))
  (let* ((root-url (oslc-root-services-url client))
	 ;; Fetch the OSLC root services document and parse the RDF triples.
	 (wilbur:*db* (with-drakma-response (body status-code)
			  (drakma:http-request root-url :want-stream t)
			(unless (= status-code 200)
			  (error 'program-error))
			(wilbur-parse-rdf/xml body root-url)))
	 ;; Query the OAuth URLs.
	 (request-token-url (wilbur:node-uri
			     (wilbur:triple-object
			      (first (wilbur:query nil (wilbur:node (oauth-request-token-tag client)) nil)))))
	 (user-authorization-url (wilbur:node-uri
				  (wilbur:triple-object
				   (first (wilbur:query nil (wilbur:node (oauth-user-authorization-tag client)) nil)))))
	 (access-token-url (wilbur:node-uri
			    (wilbur:triple-object
			     (first (wilbur:query nil (wilbur:node (oauth-access-token-tag client)) nil))))))
    ;; Configure the OAuth client.
    (setf (oauth1-request-token-url client) request-token-url
	  (oauth1-user-authorization-url client) user-authorization-url
	  (oauth1-access-token-url client) access-token-url)))

(defun oslc-invoke (method request-uri
		    &rest arguments
		    &key (client *oslc-client*) parameters headers (accept "application/rdf+xml") (oslc-core-version "2.0")
		    &allow-other-keys)
  "Invoke an OSLC REST method.

First argument METHOD is the HTTP method, e.g. ‘:get’ or ‘:put’.
Second argument REQUEST-URI is the REST API end point.
Keyword argument CLIENT is the OSLC client object.
If keyword argument UNSIGNED is true, the HTTP request will not use
 the client's OAuth authentication credentials.  Otherwise, the HTTP
 request will be signed and authorized – including a silent refresh in
 case an authorization error occurs.
Keyword argument PARAMETERS is an alist of parameters.  The parameters
 are usually added to the query part of the request URI.  However, for
 a POST request, the parameters comprise the body of the request.
Keyword argument HEADERS is an alist of additional HTTP headers which
 should be sent with the request.
If keyword argument ACCEPT is non-null, it specifies the contents of
 the ‘Accept’ header.  Default is ‘application/rdf+xml’.
If keyword argument OSLC-CORE-VERSION is non-null, it specifies the
 contents of the ‘OSLC-Core-Version’ header sent.  Default is ‘2.0’.
Remaining keyword arguments are forwarded to the underlying Drakma
 HTTP request.

Return the values of the Drakma HTTP request."
  (check-type client oslc-client)
  ;; Add ‘OSLC-Core-Version’ header.
  (when oslc-core-version
    (setf headers (list* (cons "OSLC-Core-Version" oslc-core-version) headers)))
  ;; Remove known keys – don't want to disable keyword argument
  ;; checking when calling ‘oauth1-http-request’.
  (iter (for key :in '(:client :parameters :headers :accept :oslc-core-version))
	(iter (while (remf arguments key))))
  (with-oslc-defaults
    (apply #'oauth1-http-request request-uri
	   :client client
	   :method method
	   :parameters parameters
	   :headers headers
	   :accept accept
	   arguments)))

(defgeneric oslc-service-provider-catalogs (source domain)
  (:documentation "Return all service provider catalogs for a domain.

First argument SOURCE is the RDF database.
Second argument DOMAIN is the OSLC domain (a keyword).

Value is a list of URLs."))

(defmethod oslc-service-provider-catalogs ((source oslc-client) domain)
  ;; Query the root services document of the OSLC client.
  (oslc-service-provider-catalogs
   (let ((url (oslc-root-services-url source)))
     (with-drakma-response (stream)
	 (oslc-invoke :get url :client source :unsigned t :want-stream t)
       (wilbur-parse-rdf/xml stream url)))
   domain))

(defmethod oslc-service-provider-catalogs ((source wilbur:db) (domain (eql :rm)))
  "Return all service provider catalogs in the requirements management domain."
  (let ((wilbur:*db* source))
    (nunion
     ;; OSLC Core 3.0 recommends <https://jazz.net/wiki/bin/view/Main/RootServicesSpec>.
     ;; - An oslc_rm:rmServiceProviders element (optional) locates a
     ;;   Requirements Management Service Provider Catalog Resource.
     ;;   See the OSLC Service Provider Catalog 1.0 specification and
     ;;   the OSLC Requirements Management Specification for details.
     (iter (for triple :in (wilbur:query nil (wilbur:node "http://open-services.net/xmlns/rm/1.0/rmServiceProviders") nil))
	   (collect (wilbur:node-uri (wilbur:triple-object triple))))
     ;; As an alternative, find all service provider catalogs in the
     ;; requirements management domain.  This is the OSLC 2.0 way, I
     ;; suppose.
     (iter (for triple :in (wilbur:query nil (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (wilbur:node "http://open-services.net/ns/core#ServiceProviderCatalog")))
	   (for subject = (wilbur:triple-subject triple))
	   (when (wilbur:query subject (wilbur:node "http://open-services.net/ns/core#domain") (wilbur:node "http://open-services.net/ns/rm#"))
	     (collect (wilbur:node-uri subject))))
     :test #'string=)))

(defun browse-oslc-service-provider-catalog (url &optional (client *oslc-client*))
  "Browse an OSLC service provider catalog.

Value is a tree of service provider catalogs of the form

     ((URL . TITLE) LEAFS)

The LEAFS is a list of subordinate service provider catalogs or
service providers.  A service provider has the form ‘(URL . TITLE)’.
Thus, service provider catalogs are lists and service providers are
cons cells."
  (let (haystack)
    (labels ((make-leaf (url title)
	       (cons url title))
	     (make-branch (url title leafs)
	       (list (cons url title) leafs))
	     (node-title (subject)
	       (let ((title (wilbur:query subject (wilbur:node "http://purl.org/dc/terms/title") nil)))
		 (when (= (length title) 1)
		   ;; Value should be a string literal.
		   (wilbur:triple-object (first title)))))
	     (browse-catalog (url)
	       (let ((wilbur:*db* (with-drakma-response (stream)
				      (oslc-invoke :get url :client client :want-stream t)
				    (wilbur-parse-rdf/xml stream url))))
		 ;; Mark catalog as visited.
		 (push url haystack)
		 (make-branch
		  url (node-title (wilbur:node url))
		  (nconc
		   (iter (for triple :in (wilbur:query nil (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (wilbur:node "http://open-services.net/ns/core#ServiceProviderCatalog")))
			 (for subject = (wilbur:triple-subject triple))
			 (for needle = (wilbur:node-uri subject))
			 ;; Avoid infinite recursion.
			 (unless (find needle haystack :test #'string=)
			   (collect (browse-catalog needle))))
		   (iter (for triple :in (wilbur:query nil (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (wilbur:node "http://open-services.net/ns/core#ServiceProvider")))
			 (for subject = (wilbur:triple-subject triple))
			 (collect (make-leaf
				   (wilbur:node-uri subject)
				   (node-title subject)))))))))
      (browse-catalog url))))

;;; oslc.lisp ends here
