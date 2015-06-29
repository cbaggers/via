(in-package #:via)

;; helpers
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;; a proxy is defined as a list of functions the object must adhere to
;; a proxy is then a struct that has a slot for the object and then one
;; slot for each function

(defstruct proxy
  (target (error "All proxies must be initialized with a target") :type t))

(defmacro defproxy (name (&key (strict t)) &body func-descriptions)
  (declare (ignore strict))
  (let ((func-descriptions
         (mapcar (lambda (fd)
                   (cons (first fd)
                         (mapcar (lambda (s)
                                   (if (listp s) s (list s t)))
                                 (rest fd))))
                 func-descriptions)))
    `(progn
       (defstruct (,(symb name '-proxy) (:include proxy) (:conc-name %prxy-))
         ,@(loop :for (func-name . args) :in func-descriptions
              :collect
              `(,func-name (error ,(format nil "Method ~a for Proxy ~a must be provided"
                                            func-name name))
                           :type function)))
       ,@(loop :for (func-name . args) :in func-descriptions
            :for arg-names = (mapcar #'first args)
            :for arg-types = (mapcar #'second args)
            :collect
            (let ((slot-name (symb '%prxy- func-name)))
              `(defun ,func-name (prxy ,@arg-names)
                 (let* ((target (proxy-target prxy))
                        (func (,slot-name prxy)))
                   (funcall func target ,@arg-names))))))))

;; def-proxy-impl is a macro that lets you define a implementation for a proxy
;; for a given type
;; you can then use (make-*-proxy x) where * is the type (proxy *) which is
;; generic and specialized on the type.
(defmacro def-proxy-impl ((proxy-type target-type) &body funcs)
  `(progn
     (defun ,(symb 'make- target-type '-proxy) (x)
       (,(symb 'make- proxy-type '-proxy)
         :target x
         ,@funcs))
     (defmethod proxify ((x ,target-type) (p (eql ',proxy-type)))
       (declare (ignore p))
       (,(symb 'make- target-type '-proxy) x))))

;; ;; define the proxy
;; (defproxy printable ()
;;   (print-it))
;;
;; ;; define some example type
;; (defclass thingy () ())
;;
;; ;; define an implementation of the proxy for the type
;; (def-proxy-impl (printable thingy) :print-it #'print)
;;
;; ;; make an instance of the proxy
;; (defvar test (proxify th 'printable))
;;
;; ;; print the printable
;; (print-it test)
