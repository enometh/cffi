;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Mar 22 19:53:30 2020 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2020 Madhu.  All Rights Reserved.
;;;
(defpackage "CFFI-C2FFI-SPEC-HELPER"
  (:use "CL")
  (:export "DUMP-H-FILE" "INITIALIZE-SPEC-PACKAGE"
	   "MAKE-MK-DEFSYSTEM-C2FFI-SPEC-TEMPLATE"
	   "USE-SPEC-PACKAGE"))
(in-package "CFFI-C2FFI-SPEC-HELPER")

(defun dump-h-file (includes-string h-file-name spec-dir &key force)
  (ensure-directories-exist spec-dir :verbose t)
  (let ((target (concatenate 'string spec-dir h-file-name)))
    (when (or force (not (probe-file target)))
      (cl-user::string->file includes-string target))))

(defun initialize-spec-package (target-package &optional (package *package*))
  (when (find-package  target-package)
    (unuse-package target-package package)
    (delete-package target-package))
  (make-package target-package :use nil))

(defun make-mk-defsystem-c2ffi-spec-template (system-name system-directory
					      h-file-name spec-package-name)
  `(mk:defsystem ,system-name
     :source-pathname ,system-directory
     :binary-pathname ,system-directory
     :source-extension "lisp"
     :depends-on (:cffi :cffi-c2ffi :cffi-c2ffi-generator :cffi-libffi)
     :components ((:module ,(concatenate 'string
					 (string-upcase (string system-name))
					 "-HEADERS")
		   :source-pathname ""
		   :language :c2ffi-file
		   :source-extension "h"
		   :compiler-options (:c2ffi-spec-package ,spec-package-name
				      :exclude-definitions nil
				      :include-paths nil
				      :sys-include-paths ())
		   :components ((:file ,h-file-name
				 :source-extension :empty))))))

(defvar *c2ffi-defined-names* '("+FUNCTION-NAMES+"
				"+STRUCT-NAMES+"
				"+UNION-NAMES+"
				"+VARIABLE-NAMES+"
				"+TYPE-NAMES+"
				"+CONSTANT-NAMES+"
				;; "+ARGUMENT-NAMES+"
				"+FIELD-NAMES+"))

(defun get-exported-names (spec-package)
  (setq spec-package (find-package spec-package))
  (loop for name in *c2ffi-defined-names*
	for var = (find-symbol name spec-package)
	for val = (symbol-value var)
	when (eql (symbol-package var) spec-package)
	append (loop for (nam . _sym) in val
		     collect nam)))

(defun get-conflict-names (spec-package &optional (package *package*))
  (setq package (find-package package))
  (setq spec-package (find-package spec-package))
  (loop for nam in (get-exported-names spec-package)
	for (sym status) = (multiple-value-list
			    (find-symbol nam package))
	for (ext-sym ext-status) = (multiple-value-list
				    (find-symbol nam spec-package))
	if (and status ext-status #+nil (eql status :external)
		#+nil (eql (symbol-package sym) package)
		(not (eql sym ext-sym)))
	  collect nam))

(defun use-spec-package (spec-package &optional (package *package*) &key (dry-run-p))
  "Use all symbols in SPEC-PACKAGE which are \"exported\" via the
variables in *C2FFI-DEFINED-NAMES*.  Conflicts are avoided by via
SHADOWING-IMPORT of any symbols which may conflict, before calling
use-package. Returns a list of symbols which have to be accessed
explictly."
  (setq spec-package (find-package spec-package))
  (setq package (find-package package))
  (unless dry-run-p
    (unuse-package spec-package package)
    (export (mapcar (lambda (x) (find-symbol x spec-package))
		    (get-exported-names spec-package))
	    spec-package))
  (let* ((conflicting-names (get-conflict-names spec-package package))
	 (conflicts (mapcar
		     (lambda (name)
		       (multiple-value-bind (sym stat)
			   (find-symbol name spec-package)
			 (assert stat)
			 sym))
		     conflicting-names)))
    (unless dry-run-p
      (shadowing-import conflicts package)
      (use-package spec-package package))
    ;; return a list of symbols that should be explicitly qualified.
    (mapcar (lambda (name)
	      (multiple-value-bind (sym stat)
		  (find-symbol name package)
		(assert stat)
		sym))
	    conflicting-names)))



#||
(defun get-conflict-alist (spec-package &optional (package *package*))
  (mapcar (lambda (pkg)
	    (cons pkg (get-conflict-names spec-package pkg)))
	  (adjoin package (package-use-list package))))

#+nil
(defun all-conflicting-syms (spec-package &optional (package *package*))
  (loop for (_pkg . conflicts) in (get-conflict-alist spec-package package)
	nconc (mapcar (lambda (x)
			(find-symbol x spec-package))
		      conflicts)))

;; for testing a general package
(defun get-exported-names (spec-package)
  (setq spec-package (find-package spec-package))
  (loop for sym being each external-symbol of spec-package
	for nam = (symbol-name sym)
	for (var stat) = (multiple-value-list
			  (find-symbol nam spec-package))
	do (assert (eq var sym))
	(assert (eq stat :external))
	when (eql (symbol-package sym) spec-package)
	collect nam))
||#
