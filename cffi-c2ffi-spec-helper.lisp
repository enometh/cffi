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
  (loop for name in *c2ffi-defined-names*
	for var = (find-symbol name spec-package)
	for val = (symbol-value var)
	append (loop for (nam . _sym) in val
		     collect nam)))

(defun get-conflict-names (spec-package &optional (package *package*))
  (loop for nam in (get-exported-names spec-package)
	for (sym status) = (multiple-value-list
			    (find-symbol nam package))
	if (and sym (eql status :external))
	  collect nam))

(defun use-spec-package (spec-package &optional (package *package*))
  "Use all symbols in SPEC-PACKAGE which are \"exported\" via the
variables in *C2FFI-DEFINED-NAMES*.  Conflicts are avoided by via
SHADOWING-IMPORT of any symbols which may conflict. Returns a list of
symbols which have to be accessed explictly"
  (unuse-package spec-package package)
  (export (mapcar (lambda (x) (find-symbol x spec-package))
		  (get-exported-names spec-package))
	  spec-package)
  (let ((conflicts-alist
	  (mapcar (lambda (pkg)
		    (cons pkg (get-conflict-names spec-package pkg)))
		  (adjoin package (package-use-list package)))))
    (shadowing-import (loop for (_pkg . conflicts) in conflicts-alist
			    nconc (mapcar (lambda (x)
					    (find-symbol x spec-package))
					  conflicts))
		      package)
    (use-package spec-package package)
    ;; return a list of symbols that should be explicitly qualified.
    (loop for (pkg . conflicts) in conflicts-alist
	  nconc (mapcar (lambda (x) (find-symbol x pkg)) conflicts))))
