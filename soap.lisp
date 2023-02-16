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

(define-condition soap-fault (error)
  ((message
    :initform nil
    :type (or null string dom:document)
    :accessor soap-fault-message
    :documentation "The SOAP message describing the fault.")
   (version
    :initform nil
    :type (or null soap-version)
    :accessor soap-fault-version
    :documentation "The SOAP version of the fault message.")
   (code
    :initform nil
    :type (or null string (cons string))
    :accessor soap-fault-code
    :documentation "The fault code and optional sub-codes.")
   (reason
    :initform nil
    :type (or null string)
    :accessor soap-fault-reason
    :documentation "The human-readable explanation of the fault.")
   (node
    :initform nil
    :type (or null string)
    :accessor soap-fault-node
    :documentation "The SOAP node on the SOAP message path issuing the fault.")
   (role
    :initform nil
    :type (or null string)
    :accessor soap-fault-role
    :documentation "The role the SOAP node was operating in at the point the fault occurred.")
   (detail
    :initform nil
    :type boolean
    :accessor soap-fault-detail
    :documentation "Whether or not the fault message has a detail entry."))
  (:documentation "Condition type for a SOAP fault.")
  (:report (lambda (condition stream)
	     (let ((code (soap-fault-code condition))
		   (reason (soap-fault-reason condition))
		   (node (soap-fault-node condition))
		   (role (soap-fault-role condition))
		   (detail (soap-fault-detail condition))
		   (message (soap-fault-message condition)))
	       (format stream "SOAP fault")
	       (when code
		 (if (consp code)
		     (format stream " ‘~{~A~^.~}’" code)
		   (format stream " ‘~A’" code)))
	       (when detail
		 (format stream " in detail"))
	       (when node
		 (format stream " on node ‘~A’" node)
		 (when role
		   (format stream " as ‘~A’" role)))
	       (format stream ".")
	       (when reason
		 (terpri stream)
		 (format stream "Reason: ~A" reason))
	       (when message
		 (terpri stream)
		 (format stream "Message:~%~A"
			 (typecase message
			   (dom:document
			    (dom:map-document (cxml:make-rod-sink :omit-xml-declaration-p t :indentation 1) message))
			   (t
			    message))))
	       ()))))

(defun make-soap-fault (datum)
  "Create a ‘soap-fault’ condition.

Argument DATUM is a SOAP message designator.  Value is either
 a stream, a string, a pathname, or a DOM node.

If the SOAP message contains a ‘Fault’ element, return value is
a ‘soap-fault’ condition.  Otherwise, return ‘nil’.

Exceptional situations:

   * Signals an error if DATUM does not designate a SOAP message."
  (when-let* ((document (typecase datum
			  (dom:document
			   datum)
			  (dom:node
			   (dom:owner-document datum))
			  (t
			   (cxml:parse datum (cxml-dom:make-dom-builder)))))
	      ;; Lookup the SOAP version from the namespace
	      ;; name of the document element.
	      (name (dom:namespace-uri (dom:document-element document)))
	      (version (let ((tem (rassoc name soap-namespace-alist :test #'string=)))
			 (when (null tem)
			   (error "Unknown SOAP namespace name ‘~A’." name))
			 (car tem)))
	      ;; Lookup the ‘Fault’ element.
	      (fault (xpath:with-namespaces (("env" name))
		       (xpath:first-node (xpath:evaluate "/env:Envelope/env:Body/env:Fault" document))))
	      ;; Looks good, create the condition.
	      (condition (make-instance 'soap-fault)))
    ;; Common slots.
    (setf (soap-fault-message condition) document
	  (soap-fault-version condition) version)
    ;; Decompose the fault.
    (ecase version
      (:soap-1.2
       (xpath:with-namespaces (("env" name))
	 (let ((codes (iter (with node = (xpath:first-node (xpath:evaluate "env:Code" fault)))
			    (while node)
			    (for value = (xpath:first-node (xpath:evaluate "env:Value" node)))
			    (while value)
			    ;; Get the local name of the code.
			    (let* ((code (xpath:string-value value))
				   (pos (position #\: code)))
			      (when pos
				(setf code (subseq code (1+ pos))))
			      (collecting code))
			    ;; Recurse on the sub-codes.
			    (setf node (xpath:first-node (xpath:evaluate "env:Subcode" node))))))
	   (setf (soap-fault-code condition) codes))
	 ;; Prefer reason phrase in English.
	 (when-let ((node (or (xpath:first-node (xpath:evaluate "env:Reason/env:Text[@xml:lang=\"en-US\"]" fault))
			      (xpath:first-node (xpath:evaluate "env:Reason/env:Text[@xml:lang=\"en\"]" fault))
			      (xpath:first-node (xpath:evaluate "env:Reason/env:Text[@xml:lang=\"\"]" fault))
			      (xpath:first-node (xpath:evaluate "env:Reason/env:Text" fault)))))
	   (let ((reason (xpath:string-value node)))
	     (when (plusp (length reason))
	       (setf (soap-fault-reason condition) reason))))
	 (when-let ((node (xpath:first-node (xpath:evaluate "env:Node" fault))))
	   (setf (soap-fault-node condition) (xpath:string-value node)))
	 (when-let ((node (xpath:first-node (xpath:evaluate "env:Role" fault))))
	   (setf (soap-fault-role condition) (xpath:string-value node)))
	 (when-let ((node (xpath:first-node (xpath:evaluate "env:Detail" fault))))
	   (setf (soap-fault-detail condition) t))))
      (:soap-1.1
       (when-let ((node (xpath:first-node (xpath:evaluate "faultcode" fault))))
	 (let* ((code (xpath:string-value node))
		(pos (position #\: code)))
	   (when pos
	     (setf code (subseq code (1+ pos))))
	   (setf (soap-fault-code condition) code)))
       (when-let ((node (xpath:first-node (xpath:evaluate "faultstring" fault))))
	 (let ((reason (xpath:string-value node)))
	   (when (plusp (length reason))
	     (setf (soap-fault-reason condition) reason))))
       (when-let ((node (xpath:first-node (xpath:evaluate "faultactor" fault))))
	 (let ((actor (xpath:string-value node)))
	   (when (plusp (length actor))
	     (setf (soap-fault-node condition) actor))))
       (when-let ((node (xpath:first-node (xpath:evaluate "detail" fault))))
	 (setf (soap-fault-detail condition) t))))
    ;; Return value.
    condition))

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

(defun soap-http-request (request-uri message
			  &rest options
			  &key
			    (method :post)
			    (fault-error-p t)
			    (if-not-successful :error)
			  &allow-other-keys)
  "Send a SOAP message via HTTP.

First argument REQUEST-URI is the SOAP end point.
Second argument MESSAGE is the SOAP message (a string).  See the
 ‘with-soap-envelope’ macro for how to serialize a SOAP message.
Keyword argument METHOD is the HTTP method.  Default is ‘:post’.
If keyword argument FAULT-ERROR-P is true, signal an error of type
 ‘soap-fault’ if the HTTP status code is 500.  Enabled by default.
Keyword argument IF-NOT-SUCCESSFUL specifies the action to be taken
 if the HTTP status code is not in the range from 200 to 299.  A value
 of ‘:error’ means to signal an error of type ‘http-status’ and ‘nil’
 means to do nothing.  Default is ‘:error’.

Remaining keyword arguments are forwarded to the underlying Drakma
 HTTP request.

Return the values of the Drakma HTTP request."
  (check-type if-not-successful (member nil :error))
  ;; Remove known keys.
  (iter (for key :in '(:method :fault-error-p :if-not-successful))
	(iter (while (remf options key))))
  (multiple-value-bind (body status-code headers effective-uri stream closep reason-phrase)
      (apply #'drakma:http-request request-uri
	     :method method
	     :content message
	     :content-type "text/xml" ;or "application/soap+xml"
	     options)
    (cond ((and fault-error-p (= status-code 500))
	   (let ((fault (or (make-soap-fault body)
			    (make-http-status status-code reason-phrase))))
	     (cleanup-drakma-response body closep)
	     (error fault)))
	  ((and (eq if-not-successful :error)
		(not (<= 200 status-code 299)))
	   (cleanup-drakma-response body closep)
	   (error (make-http-status status-code reason-phrase))))
    ;; Return values.
    (values body status-code headers effective-uri stream closep reason-phrase)))

;;; soap.lisp ends here
