;;; upnp.lisp --- universal plug and pray

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

(defconst upnp-nil 'upnp-nil
  "Not in list, for use with ‘getf’.")

(defvar *upnp-device* nil
  "The default UPnP device.")

;;;; Types

(defclass upnp-object ()
  ((property-list
    :initarg :property-list
    :initform ()
    :type list
    :accessor upnp-property-list
    :documentation "Object properties."))
  (:documentation "Base class for all UPnP objects."))

(defclass upnp-device (upnp-object)
  ((parent
    :initarg :parent
    :initform nil
    :type (or null upnp-device)
    :reader upnp-parent
    :documentation "Parent device in the device tree.")
   (device-node
    :initarg :device-node
    :initform (missing-initialization-argument)
    :type dom:element
    :reader upnp-node
    :documentation "XML element of the device description."))
  (:documentation "UPnP device object."))

(defclass upnp-root-device (upnp-device)
  ((base-uri
    :initarg :base-uri
    :initform (missing-initialization-argument)
    :type quri:uri
    :accessor upnp-base-uri
    :documentation "Base URL of the device."))
  (:documentation "UPnP root device object."))

(defclass upnp-service (upnp-object)
  ((device
    :initarg :device
    :initform (missing-initialization-argument)
    :type upnp-device
    :reader upnp-device
    :documentation "Device of the service.")
   (service-node
    :initarg :service-node
    :initform (missing-initialization-argument)
    :type dom:element
    :reader upnp-node
    :documentation "XML element of the service description.")
   (control-uri
    :initarg :control-uri
    :initform root-quri
    :type quri:uri
    :accessor upnp-control-uri
    :documentation "Relative URL for control.")
   (event-uri
    :initarg :event-uri
    :initform root-quri
    :type quri:uri
    :accessor upnp-event-uri
    :documentation "Relative URL for eventing."))
  (:documentation "UPnP service object."))

(defclass upnp-action (upnp-object)
  ((service
    :initarg :service
    :initform (missing-initialization-argument)
    :type upnp-service
    :reader upnp-service
    :documentation "Service of the action.")
   (scpd-node
    :initarg :scpd-node
    :initform (missing-initialization-argument)
    :type dom:element
    :reader upnp-node
    :documentation "XML element of the action in the SCPD.")
   (soap-action
    :initarg :soap-action
    :initform nil
    :type (or null string)
    :accessor upnp-soap-action
    :documentation "SOAP action tag.")
   (input-arguments
    :initarg :input-arguments
    :initform ()
    :type list
    :accessor upnp-input-arguments
    :documentation "Alist of input arguments for invoking the action.
List elements are cons cells of the form ‘(ARGUMENT-NAME . STATE-VARIABLE)’.")
   (output-arguments
    :initarg :output-arguments
    :initform ()
    :type list
    :accessor upnp-output-arguments
    :documentation "Alist of output arguments, i.e. return values.
List elements are cons cells of the form ‘(ARGUMENT-NAME . STATE-VARIABLE)’."))
  (:documentation "UPnP action object."))

(defclass upnp-argument (upnp-object)
  ((action
    :initarg :action
    :initform (missing-initialization-argument)
    :type upnp-action
    :reader upnp-action
    :documentation "Action of the argument.")
   (scpd-node
    :initarg :scpd-node
    :initform (missing-initialization-argument)
    :type dom:element
    :reader upnp-node
    :documentation "XML element of the action argument in the SCPD."))
  (:documentation "UPnP action argument object."))

(defclass upnp-state-variable (upnp-object)
  ((service
    :initarg :service
    :initform (missing-initialization-argument)
    :type upnp-service
    :reader upnp-service
    :documentation "Service of the state variable.")
   (scpd-node
    :initarg :scpd-node
    :initform (missing-initialization-argument)
    :type dom:element
    :reader upnp-node
    :documentation "XML element of the state variable in the SCPD."))
  (:documentation "UPnP state variable object."))

(defgeneric %upnp-put (object indicator value)
  (:documentation "Update an UPnP object property.
Lookup a property in the object's property list and set its
corresponding property value.  If there is no property with
that property indicator, add it.

First argument OBJECT is the UPnP object.
Second argument INDICATOR is the property indicator (a keyword).
Third argument VALUE is the property value.

Return the new property value."))

(defmethod %upnp-put ((object upnp-object) indicator value)
  (setf (getf (upnp-property-list object) indicator) value))

(defgeneric %upnp-get (object indicator &optional default)
  (:documentation "Query an UPnP object property.
Lookup a property in the object's property list and return its
corresponding property value.  If there is no property with that
property indicator, return a default value.

First argument OBJECT is the UPnP object.
Second argument INDICATOR is the property indicator (a keyword).
Optional third argument DEFAULT is the value to be returned if
 the property does not exist.

Return either the property value or the default value."))

(defmethod %upnp-get ((device upnp-device) indicator &optional default)
  (let ((value (getf (upnp-property-list device) indicator upnp-nil)))
    (when (eq value upnp-nil)
      ;; Try to fetch the property value from the device description.
      (setf value (let ((device-node (upnp-node device)))
                    (xpath:with-namespaces (("" (dom:namespace-uri device-node)))
                      (case indicator
                        (:device-type
                         (xpath-required-string "./deviceType[1]" device-node))
                        (:friendly-name
                         (xpath-required-string "./friendlyName[1]" device-node))
                        (:manufacturer
                         (xpath-required-string "./manufacturer[1]" device-node))
                        (:manufacturer-url
                         (xpath-optional-string "./manufacturerURL[1]" device-node))
                        (:model-description
                         (xpath-optional-string "./modelDescription[1]" device-node))
                        (:model-name
                         (xpath-required-string "./modelName[1]" device-node))
                        (:model-number
                         (xpath-optional-string "./modelNumber[1]" device-node))
                        (:model-url
                         (xpath-optional-string "./modelURL[1]" device-node))
                        (:serial-number
                         (xpath-optional-string "./serialNumber[1]" device-node))
                        (:unique-device-name
                         (xpath-required-string "./UDN[1]" device-node))
                        (:universal-product-code
                         (xpath-optional-string "./UPC[1]" device-node))
                        (:icon-list
                         ())
                        (:service-list
                         (should-not-happen)) ;see ‘%upnp-describe-device’
                        (:device-list
                         (should-not-happen))
                        (:presentation-url
                         (xpath-optional-string "./presentationURL[1]" device-node))
                        ;; Other properties.
                        (:parent-device
                         (upnp-parent device))
                        (:root-device
                         (let ((root device))
                           (iter (for parent = (upnp-parent root))
                                 (while (not (null parent)))
                                 (setf root parent))
                           root))
                        (t upnp-nil)))))
      (unless (eq value upnp-nil)
        (%upnp-put device indicator value)))
    ;; The actual return value.
    (if (eq value upnp-nil) default value)))

(defmethod %upnp-get ((service upnp-service) indicator &optional default)
  (let ((value (getf (upnp-property-list service) indicator upnp-nil)))
    (when (eq value upnp-nil)
      (setf value (let ((service-node (upnp-node service)))
                    (xpath:with-namespaces (("" (dom:namespace-uri service-node)))
                      (case indicator
                        (:service-type
                         (xpath-required-string "./serviceType[1]" service-node))
                        (:service-identifier
                         (xpath-required-string "./serviceId[1]" service-node))
                        (:control-url
                         (xpath-required-string "./controlURL[1]" service-node))
                        (:event-subscription-url
                         (xpath-required-string "./eventSubURL[1]" service-node))
                        (:scpd-url
                         (xpath-required-string "./SCPDURL[1]" service-node))
                        (t upnp-nil)))))
      (unless (eq value upnp-nil)
        (%upnp-put service indicator value)))
    (if (eq value upnp-nil) default value)))

(defmethod %upnp-get ((action upnp-action) indicator &optional default)
  (let ((value (getf (upnp-property-list action) indicator upnp-nil)))
    (when (eq value upnp-nil)
      (setf value (let ((action-node (upnp-node action)))
                    (xpath:with-namespaces (("" (dom:namespace-uri action-node)))
                      (case indicator
                        (:name
                         (xpath-required-string "./name[1]" action-node))
                        (:argument-list
                         (should-not-happen))
                        (t upnp-nil)))))
      (unless (eq value upnp-nil)
        (%upnp-put action indicator value)))
    (if (eq value upnp-nil) default value)))

(defmethod %upnp-get ((argument upnp-argument) indicator &optional default)
  (let ((value (getf (upnp-property-list argument) indicator upnp-nil)))
    (when (eq value upnp-nil)
      (setf value (let ((argument-node (upnp-node argument)))
                    (xpath:with-namespaces (("" (dom:namespace-uri argument-node)))
                      (case indicator
                        (:name
                         (xpath-required-string "./name[1]" argument-node))
                        (:direction
                         (make-keyword
                          (xpath-required-string "./direction[1]" argument-node)))
                        (:retval
                         (not (null (xpath-optional-node "./retval[1]" argument-node))))
                        (:related-state-variable
                         (xpath-required-string "./relatedStateVariable[1]" argument-node))
                        (t upnp-nil)))))
      (unless (eq value upnp-nil)
        (%upnp-put argument indicator value)))
    (if (eq value upnp-nil) default value)))

(defmethod %upnp-get ((state upnp-state-variable) indicator &optional default)
  (let ((value (getf (upnp-property-list state) indicator upnp-nil)))
    (when (eq value upnp-nil)
      (setf value (let ((state-node (upnp-node state)))
                    (xpath:with-namespaces (("" (dom:namespace-uri state-node)))
                      (case indicator
                        (:send-events
                         (if (dom:has-attribute state-node "sendEvents")
                             (string= (dom:get-attribute state-node "sendEvents") "yes")
                           t)) ;default is ‘yes’
                        (:multicast
                         (if (dom:has-attribute state-node "multicast")
                             (string= (dom:get-attribute state-node "multicast") "yes")
                           nil)) ;default is ‘no’
                        (:name
                         (xpath-required-string "./name[1]" state-node))
                        (:data-type
                         (make-keyword
                          (xpath-required-string "./dataType[1]" state-node)))
                        (:default-value
                         (xpath-optional-string "./defaultValue[1]" state-node))
                        (:allowed-value-list
                         (iter (for node :in-node-set (xpath:evaluate "./allowedValueList/allowedValue" state-node))
                               (collecting (xpath:string-value node))))
                        (:allowed-value-range
                         (when-let ((range-node (xpath-optional-node "./allowedValueRange[1]" state-node)))
                           (let ((minimum (xpath-required-string "./minimum[1]" range-node))
                                 (maximum (xpath-required-string "./maximum[1]" range-node))
                                 (step (xpath-optional-string "./step[1]" range-node)))
                             (nconc (list :minimum minimum :maximum maximum) (when step (list :step step))))))
                        (t upnp-nil)))))
      (unless (eq value upnp-nil)
        (%upnp-put state indicator value)))
    (if (eq value upnp-nil) default value)))

(defmethod print-object ((object upnp-device) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (%upnp-get object :model-name) stream)))

(defmethod print-object ((object upnp-service) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (%upnp-get object :service-type) stream)))

(defmethod print-object ((object upnp-action) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (upnp-soap-action object) stream)))

(defmethod print-object ((object upnp-argument) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~(~A~)" (%upnp-get object :name) (%upnp-get object :direction))))

(defmethod print-object ((object upnp-state-variable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~(~A~)" (%upnp-get object :name) (%upnp-get object :data-type))))

(defgeneric upnp-name (object)
  (:documentation "Unique name to identify an object."))

(defmethod upnp-name ((device upnp-device))
  (%upnp-get device :unique-device-name))

(defmethod upnp-name ((service upnp-service))
  (%upnp-get service :service-identifier))

(defmethod upnp-name ((action upnp-action))
  (%upnp-get action :name))

(defmethod upnp-name ((argument upnp-argument))
  (%upnp-get argument :name))

(defmethod upnp-name ((state upnp-state-variable))
  (%upnp-get state :name))

(defmethod upnp-base-uri ((device upnp-device))
  (upnp-base-uri (%upnp-get device :root-device)))

;;;; Description

(defun upnp-device-description (object)
  "Retrieve a description of a device and its capabilities.
Description is step two in UPnP networking.

Argument OBJECT identifies the device.  Value is the URL to the
 device's UPnP description.

Return value is a UPnP device object."
  (let* ((root-url (or (ignore-errors
                        (string-from-uri object))
                       (error "Bad device description.")))
         (root-uri (quri:uri root-url))
         ;; Get the root document.
         (root-document (with-drakma-response (body status)
                            (drakma:http-request root-url :verify nil)
                          (ensure-http-status status 200)
                          (xml-parse body)))
         (root-element (dom:document-element root-document))
         (device-node (xpath:with-namespaces (("" (dom:namespace-uri root-element)))
                        (xpath-required-node "./device[1]" root-element)))
         ;; Create the root device object.
         (device (make-instance 'upnp-root-device
                                :parent nil
                                :device-node device-node
                                ;; Start with the root URL.  The base
                                ;; URL may change when switching from
                                ;; HTTP to HTTPS.
                                :base-uri root-uri
                                :property-list (list :root-url root-url
                                                     :root-document root-document
                                                     :root-element root-element))))
    (xpath:with-namespaces (("" (dom:namespace-uri root-element)))
      (%upnp-describe-device device))))

(defun %upnp-describe-device (device)
  "Recursively describe services and embedded devices of DEVICE."
  (let ((device-node (upnp-node device)))
    (%upnp-put device :service-list
               (iter (for service-node :in-node-set (xpath:evaluate "./serviceList/service" device-node))
                     (collecting (%upnp-make-service service-node device))))
    (%upnp-put device :device-list
               (iter (for child-node :in-node-set (xpath:evaluate "./deviceList/device" device-node))
                     (collecting (%upnp-make-device child-node device)))))
  ;; Return value.
  device)

(defun %upnp-make-device (device-node parent)
  "Create a new device based on the DEVICE-NODE description."
  (%upnp-describe-device (make-instance 'upnp-device
                                        :parent parent
                                        :device-node device-node)))

(defun %upnp-make-service (service-node device)
  "Create a new service based on the SERVICE-NODE description."
  (let* ((service (make-instance 'upnp-service
                                 :device device
                                 :service-node service-node))
         (service-type (%upnp-get service :service-type))
         (control-uri (quri:uri (%upnp-get service :control-url)))
         (event-uri (quri:uri (%upnp-get service :event-subscription-url)))
         (scpd-uri (merge-uri (quri:uri (%upnp-get service :scpd-url)) (upnp-base-uri device)))
         (scpd-document (with-drakma-response (body status)
                            (drakma:http-request (string-from-uri scpd-uri) :verify nil)
                          (ensure-http-status status 200)
                          (xml-parse body)))
         (scpd-element (dom:document-element scpd-document)))
    ;; Update the service object.
    (setf (upnp-control-uri service) control-uri
          (upnp-event-uri service) event-uri)
    (%upnp-put service :scpd-document scpd-document)
    (%upnp-put service :scpd-element scpd-element)
    ;; Examine the service control protocol description.
    (xpath:with-namespaces (("" (dom:namespace-uri scpd-element)))
      ;; Collect the actions provided by this service.  First collect
      ;; the state variables since the action arguments refer to them.
      (let* ((state-variables (iter (for state-node :in-node-set (xpath:evaluate "./serviceStateTable/stateVariable" scpd-element))
                                    (collecting (make-instance 'upnp-state-variable :service service :scpd-node state-node))))
             (action-list (iter (for action-node :in-node-set (xpath:evaluate "./actionList/action" scpd-element))
                                (for action = (make-instance 'upnp-action :service service :scpd-node action-node))
                                (for action-name = (%upnp-get action :name))
                                (for input-arguments = ())
                                (for output-arguments = ())
                                (for argument-list = ())
                                (iter (for argument-node :in-node-set (xpath:evaluate "./argumentList/argument" action-node))
                                      (for argument = (make-instance 'upnp-argument :action action :scpd-node argument-node))
                                      (for related-state-variable = (%upnp-get argument :related-state-variable))
                                      (for state-variable = (or (find related-state-variable state-variables :key #'upnp-name :test #'string=)
                                                                (error "Unknown state variable ‘~A’." related-state-variable)))
                                      (%upnp-put argument :state-variable state-variable)
                                      (for direction = (%upnp-get argument :direction))
                                      (if (string= direction "in")
                                          (push argument input-arguments)
                                        (push argument output-arguments))
                                      (push argument argument-list))
                                ;; Update the action object.
                                (setf (upnp-soap-action action) (~ service-type "#" action-name)
                                      (upnp-input-arguments action) (nreverse input-arguments)
                                      (upnp-output-arguments action) (nreverse output-arguments))
                                (%upnp-put action :argument-list (nreverse argument-list))
                                (collecting action))))
        ;; Save actions and state variables.
        (%upnp-put service :action-list action-list)
        (%upnp-put service :state-variables state-variables)
        ;; Append the actions to the root device's action list.
        (let ((root (%upnp-get device :root-device)))
          (%upnp-put root :action-list (nconc (%upnp-get root :action-list ()) (copy-list action-list))))))
    ;; Return value.
    service))

(defun upnp-find-soap-action (soap-action &optional (device *upnp-device*))
  "Find an action by its SOAP action tag.

First argument SOAP-ACTION is the SOAP action tag (a string).  The
 SOAP action tag consists of the service type and the action name
 separated by a ‘#’ character.
Optional second argument DEVICE is the UPnP root device.

Return the first matching UPnP action object or ‘nil’ of no matching
action can be found."
  (check-type soap-action string)
  (check-type device upnp-root-device)
  (find soap-action (%upnp-get device :action-list)
        :key #'upnp-soap-action
        :test #'string=))

;;; upnp.lisp ends here
