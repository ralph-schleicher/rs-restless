;;; soap.lisp --- simple object access protocol

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

(deftype soap-version ()
  "The data type of a SOAP standard identifier."
  '(member :soap-1.2 :soap-1.1))

(defvar *soap-version* :soap-1.2
  "The SOAP standard identifier.
Possible values together with their meaning are listed in the
following table.

     :soap-1.2
          SOAP version 1.2, see ‹https://www.w3.org/TR/soap12/›.

     :soap-1.1
          SOAP version 1.1 (this is no standard, just a note),
          see ‹https://www.w3.org/TR/2000/NOTE-SOAP-20000508/›.

Default is ‘:soap-1.2’.")
(declaim (type soap-version *soap-version*))

(defvar *soap-namespace-prefix* "env"
  "The SOAP namespace prefix.
Value has to be a string.  Default is ‘env’.")
(declaim (type string *soap-namespace-prefix*))

(defconst soap-namespace-alist
  '((:soap-1.2 . "http://www.w3.org/2003/05/soap-envelope")
    (:soap-1.1 . "http://schemas.xmlsoap.org/soap/envelope/"))
  "Alist of SOAP namespace names.
List elements are cons cells of the form ‘(VERSION . NAME)’ where
VERSION is the SOAP standard identifier (see the ‘*soap-version*’
special variable) and NAME is the corresponding SOAP namespace
name (a string).")

(defmacro with-soap-envelope ((&rest options &key version namespace-prefix xml-declaration xml-namespaces header &allow-other-keys) &body body)
  "Create a SOAP message, i.e. an XML document.

Keyword argument VERSION is the SOAP standard identifier.
 The resulting SOAP message will conform to this SOAP version.
 Default is the value of the ‘*soap-version*’ special variable.
Keyword argument NAMESPACE-PREFIX is the SOAP namespace prefix
 for the SOAP message.  Value has to be a string.  Default is
 the value of the ‘*soap-namespace-prefix*’ special variable.
If keyword argument XML-DECLARATION is true, add an XML declaration
 to the SOAP message.  Default is to omit the XML declaration.
Keyword argument XML-NAMESPACES is a list of additional XML namespaces
 for the XML document.  List elements are either XML namespace objects
 or strings.  For XML namespace objects, the namespace prefix and name
 is evaluated by calling the ‘xmlns-prefix’ and ‘xmlns-name’ function
 respectively.  Strings are interpreted as namespace prefixes and the
 corresponding namespace names are looked up in the ‘*xml-namespaces*’
 special variable.
Keyword argument HEADER is the contents of the SOAP header.
The body (and the header) are CXML serialization forms.

Return value is the SOAP message as a string.

For example, the form

     (with-soap-envelope (:xml-namespaces '((\"ser\" . \"http://example.com/service/1.0/\")))
       (cxml:with-element \"ser:fubar\"
         (cxml:with-element \"baz\" (cxml:text \"hack\"))))

creates this SOAP message:

     <env:Envelope xmlns:env=\"http://www.w3.org/2003/05/soap-envelope\"
                   xmlns:ser=\"http://example.com/service/1.0/\">
      <env:Header/>
      <env:Body>
       <ser:fubar>
        <baz>hack</baz>
       </ser:fubar>
      </env:Body>
     </env:Envelope>

The indentation is only added for visualization effects.  The actual
SOAP message does not contain any superfluous whitespace characters."
  (declare (ignore version namespace-prefix xml-declaration xml-namespaces))
  `(invoke-soap-envelope (lambda () ,@header) (lambda () ,@body) ,@options))

(defun invoke-soap-envelope (header body
			     &key
			       (version *soap-version*)
			       (namespace-prefix *soap-namespace-prefix*)
			       xml-declaration
			       xml-namespaces
			       xml-indentation
			     &allow-other-keys)
  "Helper function for the ‘with-soap-envelope’ macro."
  (check-type version soap-version)
  (check-type namespace-prefix string)
  (let* ((env namespace-prefix)
	 (form `(cxml:with-element* (,env "Envelope")
		  (cxml:with-element* (,env "Header") (funcall ,header))
		  (cxml:with-element* (,env "Body") (funcall ,body)))))
    ;; Add XML namespaces; preserving the given order.
    (dolist (namespace (reverse xml-namespaces))
      ;; Resolve a namespace prefix via ‘*xml-namespaces*’.
      (when (stringp namespace)
	(let ((tem (assoc namespace *xml-namespaces* :test #'string=)))
	  (when (null tem)
	    (error "Unknown namespace prefix ‘~A’." namespace))
	  (setf namespace tem)))
      (setf form `(cxml:with-namespace (,(xmlns-prefix namespace) ,(xmlns-name namespace))
		    ,form)))
    ;; Finish the XML document.
    (setf form `(cxml:with-xml-output (cxml:make-rod-sink :omit-xml-declaration-p (not ,xml-declaration) :indentation ,xml-indentation)
		  (cxml:with-namespace (,env (xmlns-name (assoc ,version soap-namespace-alist)))
		    ,form)))
    ;; Create it.
    (eval form)))

;;; soap.lisp ends here
