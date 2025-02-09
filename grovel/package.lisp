;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Groveler DEFPACKAGE.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

#+(and asdf (not mk-defsystem))
(uiop:define-package #:cffi-grovel
  (:mix #:cffi-toolchain #:asdf #:uiop #:alexandria #:common-lisp))

(defpackage #:cffi-grovel
  (:use "CL" #+nil "ALEXANDRIA" "CFFI-TOOLCHAIN")
  (:import-from "UIOP" "STRCAT" "NEST" "RUN-PROGRAM")
  (:import-from "ALEXANDRIA" "WITH-UNIQUE-NAMES" "SYMBOLICATE" "APPENDF"
   "FORMAT-SYMBOL" "ENSURE-LIST")
  #+(and asdf (not mk-defsystem))
  (:import-from "ASDF" "COMPILE-OP" "LOAD-SOURCE-OP"  "LINK-OP" "LOAD-OP" "SYSTEM")
  (:export
   ;; Class name
   #:grovel-file
   #:process-grovel-file
   #:wrapper-file
   #:process-wrapper-file
   ;; Error conditions
   #:grovel-error
   #:missing-definition))
