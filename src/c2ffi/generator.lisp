;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; generator.lisp --- Generate CFFI bindings for a c2ffi output.
;;;
;;; Copyright (C) 2015, Attila Lendvai <attila@lendvai.name>
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

(in-package #:cffi/c2ffi)

;;; Output generation happens in one phase, straight into the output
;;; stream. There's minimal look-ahead (for source-location and name)
;;; which is needed to apply user specified filters in time.
;;;
;;; Each CFFI form is also EVAL'd during generation because the CFFI
;;; type lookup/parsing mechanism is used while generating the output.
;;;
;;; Nomenclature:
;;;
;;;  - variable names in this file are to be interpreted in the
;;;    C,c2ffi,json context, and 'cffi' is added to names that denote
;;;    the cffi name.
;;;
;;; Possible improvments:
;;;
;;;  - generate an additional grovel file for C inline function
;;;    declarations found in header files
;;;
;;;  - generate struct-by-value DEFCFUN's into a separate file so that
;;;    users can decide whether to depend on libffi, or they can make do
;;;    without those definitions

(defvar *allow-pointer-type-simplification* t)
(defvar *allow-skipping-struct-fields* t)
(defvar *assume-struct-by-value-support* t)
;; Called on the json name and may return a symbol to be used, or a string.
(defvar *ffi-name-transformer* 'default-ffi-name-transformer)
;; Called on the already transformed name to decide whether to export it
(defvar *ffi-name-export-predicate* 'default-ffi-name-export-predicate)
;; Called on the CFFI type, e.g. to turn (:pointer :char) into a :string.
(defvar *ffi-type-transformer* 'default-ffi-type-transformer)
;; May return up to two closures using VALUES. The first one will be called
;; with each emitted form, and the second one once, at the end. They both may
;; return a list of forms that will be emitted using OUTPUT/CODE.
(defvar *callback-factory* 'default-callback-factory)

(alexandria:define-constant +generated-file-header+
    ";;; -*- Mode: lisp -*-~%~
     ;;;~%~
     ;;; This file has been automatically generated by cffi/c2ffi. Editing it by hand is not wise.~%~
     ;;;~%~%"
  :test 'equal)

(defvar *c2ffi-output-stream*)

(defun output/export (names package)
  (let ((names (alexandria:ensure-list names)))
    ;; Make sure we have something PRINT-READABLY as a package name,
    ;; i.e. not a SIMPLE-BASE-STRING on SBCL.
    (output/code `(export ',names ',(make-symbol (package-name package))))))

(defun output/code (form)
  (check-type form cons)
  (format *c2ffi-output-stream* "~&")
  (write form
         :stream *c2ffi-output-stream*
         :circle t
         :pretty t
         :escape t
         :readably t)
  (format *c2ffi-output-stream* "~%~%")
  (unless (member (first form) '(cffi:defcfun alexandria:define-constant) :test 'eq)
    (eval form)))

(defun output/string (message-control &rest message-arguments)
  (apply 'format *c2ffi-output-stream* message-control message-arguments))

;; NOTE: as per c2ffi json output. A notable difference to
;; CFFI::*BUILT-IN-FOREIGN-TYPES* is the presence of :SIGNED-CHAR.
(alexandria:define-constant +c-builtin-types+ '(":void" ":_Bool" ":char" ":signed-char" ":unsigned-char" ":short"
                                     ":unsigned-short" ":int" ":unsigned-int" ":long" ":unsigned-long"
                                     ":long-long" ":unsigned-long-long" ":float" ":double" ":long-double")
  :test 'equal)

(define-condition unsupported-type (cffi::foreign-type-error)
  ((json-definition :initarg :json-definition
                    :accessor json-definition-of)))

(defun unsupported-type (json-entry)
  (error 'unsupported-type :type-name nil :json-definition json-entry))

;;;;;;
;;; Utilities

(defun compile-rules (rules)
  (case rules
    (:all rules)
    (t (mapcar (lambda (pattern)
                 (check-type pattern string "Patterns in the inclusion/exclusion rules must be strings.")
                 (let ((scanner (cl-ppcre:create-scanner pattern)))
                   (alexandria:named-lambda cffi/c2ffi/cl-ppcre-rule-matcher
                       (string)
                     (funcall scanner string 0 (length string)))))
               rules))))

(defun include-definition? (name source-location
                            include-definitions exclude-definitions
                            include-sources exclude-sources)
  (labels
      ((covered-by-a-rule? (name rules)
         (or (eq rules :all)
             (not (null (some (alexandria:rcurry #'funcall name) rules)))))
       (weak? (rules)
         (eq :all rules))
       (strong? (name rules)
         (and name
              (not (weak? rules))
              (covered-by-a-rule? name rules))))
    (let* ((excl-def/weak   (weak? exclude-definitions))
           (excl-def/strong (strong? name exclude-definitions))
           (incl-def/weak   (weak? include-definitions))
           (incl-def/strong (strong? name include-definitions))
           (excl-src/weak   (weak? exclude-sources))
           (excl-src/strong (strong? source-location exclude-sources))
           (incl-src/weak   (weak? include-sources))
           (incl-src/strong (strong? source-location include-sources))
           (incl/strong     (or incl-def/strong
                                incl-src/strong))
           (excl/strong     (or excl-def/strong
                                excl-src/strong))
           (incl/weak       (or incl-def/weak
                                incl-src/weak))
           (excl/weak       (or excl-def/weak
                                excl-src/weak)))
      (or incl-def/strong
          (and (not excl/strong)
               (or incl/strong
                   (and incl/weak
                        ;; we want src exclude rules to be stronger
                        (not excl-src/weak))
                   (not excl/weak)))))))

(defun coerce-to-byte-size (bit-size)
  (let ((byte-size (/ bit-size 8)))
    (unless (integerp byte-size)
      (error "Non-byte size encountered where it wasn't expected (~A bits)" bit-size))
    byte-size))

(defmacro assume (condition &optional format-control &rest format-arguments)
  "Similar to ASSERT, but WARN's only."
  `(unless ,condition
     ,(if format-control
          `(warn ,format-control ,@format-arguments)
          `(warn "ASSUME failed: ~S" ',condition))))

(defun canonicalize-transformer-hook (hook)
  (etypecase hook
    ((and (or function symbol)
          (not null))
     hook)
    (string
     (the symbol (uiop/stream:safe-read-from-string hook)))))

;;;;;;
;;; Json access

(defun json-value (alist key &key (otherwise nil otherwise?))
  (check-type alist list)
  (check-type key (and symbol (not null)))
  (let* ((entry (assoc key alist))
         (result (cond
                   (entry
                    (cdr entry))
                   (otherwise?
                    otherwise)
                   (t (error "Key ~S not found in json entry ~S." key alist)))))
    (if (equal result "")
        nil
        result)))

(defmacro with-json-values ((json-entry &rest args) &body body)
  (if (null args)
      `(progn
         ,@body)
      (alexandria:once-only (json-entry)
        `(let (,@(loop
                   :for entry :in args
                   :collect (let* ((args (alexandria:ensure-list entry))
                                   (name (pop args))
                                   (key (or (pop args)
                                            (alexandria:make-keyword (symbol-name name)))))
                              (destructuring-bind
                                    ;; using &optional would trigger a warning (on SBCL)
                                    (&key (otherwise nil otherwise?))
                                  args
                                `(,name
                                  (json-value ,json-entry ,key ,@(when otherwise?
                                                                       `(:otherwise ,otherwise))))))))
           ,@body))))

(defun expected-json-keys (alist &rest keys)
  (let* ((keys (list* :location keys))
         (outliers (remove-if (lambda (el)
                                (member (car el) keys :test 'eq))
                              alist)))
    (when outliers
      (warn "Unexpected key(s) in json entry ~S: ~S" alist outliers))))

;;;;;;
;;; Namespaces, names and conversions

;; an alist of (name . hashtable)
(defvar *generated-names*)
(defvar *anon-name-counter*)
(defvar *anon-entities*)

(defun register-anon-entity (id name)
  (check-type id integer)
  (check-type name string)
  (assert (not (zerop (length name))))
  (setf (gethash id *anon-entities*) name)
  name)

(defun lookup-anon-entity (id)
  (or (gethash id *anon-entities*)
      (error "Could not find anonymous entity with id ~S." id)))

(defun generate-anon-name (base-name)
  (format nil "~A"
          (uiop/utility:strcat (symbol-name base-name)
                  (princ-to-string (incf *anon-name-counter*)))))

(defun valid-name-or-die (name)
  ;; checks for valid json names (*not* CFFI names)
  (etypecase name
    (string
     (assert (not (zerop (length name)))))
    (cons
     (assert (= 2 (length name)))
     (assert (member (first name) '(:struct :union :enum)))
     (valid-name-or-die (second name)))))

(defun call-hook (hook &rest args)
  (apply hook
         ;; indiscriminately add one keyword arg entry to warn
         (append args '(just-a-warning "Make sure your transformer hook has &key &allow-other-keys for future extendability."))))

(defun find-cffi-type-or-die (type-name &optional (namespace :default))
  (when (eq namespace :enum)
    ;; TODO FIXME this should be cleaned up in CFFI. more about namespace confusion at:
    ;; https://github.com/cffi/cffi/issues/266
    (setf namespace :default))
  (cffi::find-type-parser type-name namespace))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (alexandria:define-constant +name-kinds+ '(:struct :union :function :variable :type
                                  :constant :field :argument :enum :member)
    :test 'equal))

(deftype ffi-name-kind ()
  '#.(list* 'member +name-kinds+))

(defun json-name-to-cffi-name (name kind &optional anonymous)
  (check-type name string)
  (check-type kind ffi-name-kind)
  (when *ffi-name-transformer*
    (setf name (call-hook *ffi-name-transformer* name kind))
    (unless (or (and (symbolp name)
                     (not (null name)))
                (stringp name))
      (error "The FFI-NAME-TRANSFORMER ~S returned with ~S which is not a valid name."
             *ffi-name-transformer* name)))
  (let ((cffi-name (if (symbolp name)
                       name
                       (intern name))))
    (when (and (not anonymous)
               (boundp '*generated-names*))
      ;; TODO FIXME this function also gets called for e.g. argument types of a function. and
      ;; if the function ends up *not* getting emitted, e.g. because of a missing type, then
      ;; we wrongly record here the missing type in the *generated-names* registry.
      (setf (gethash name (cdr (assoc kind *generated-names*)))
            cffi-name))
    cffi-name))

(defun default-callback-factory (&key &allow-other-keys)
  (values))

(defun default-ffi-name-transformer (name kind &key &allow-other-keys)
  (check-type name string)
  (case kind
    #+nil
    ((:constant :member)
     (assert (not (symbolp name)))
     (format nil "+~A+" name))
    (t name)))

(defun change-case-to-readtable-case (name &optional (reatable *readtable*))
  (ecase (readtable-case reatable)
    (:upcase (string-upcase name))
    (:downcase (string-downcase name))
    (:preserve name)
    ;; (:invert no, you don't)
    ))

(defun camelcased? (name)
  (and (>= (length name) 3)
       (let ((lower 0)
             (upper 0))
         (loop
           :for char :across name
           :do (cond
                 ((upper-case-p char)
                  (incf upper))
                 ((lower-case-p char)
                  (incf lower))))
         (unless (or (zerop lower)
                     (zerop upper))
           (let ((ratio (/ upper lower)))
             (and (<= 0.05 ratio 0.5)))))))

(defun camelcase-to-dash-separated (name)
  (coerce (loop
            :for char :across name
            :for index :from 0
            :when (and (upper-case-p char)
                       (not (zerop index)))
              :collect #\-
            :collect (char-downcase char))
          'string))

(defun maybe-camelcase-to-dash-separated (name)
  (if (camelcased? name)
      (camelcase-to-dash-separated name)
      name))

(defun default-ffi-name-export-predicate (symbol &key &allow-other-keys)
  (declare (ignore symbol))
  nil)

(defun default-ffi-type-transformer (type context &key &allow-other-keys)
  (declare (ignore context))
  (cond
    ((and (consp type)
          (eq :pointer (first type)))
     (let ((pointed-to-type (second type)))
       (if (eq pointed-to-type :char)
           :string
           type)))
    (t
     type)))

(defun function-pointer-type-name ()
  (alexandria:symbolicate '#:function-pointer))

(defmacro with-allowed-foreign-type-errors ((on-failure-form &key (enabled t)) &body body)
  (alexandria:with-unique-names (type-block)
    `(block ,type-block
      (handler-bind
          ((cffi::foreign-type-error
            (lambda (_)
              (declare (ignore _))
              (when ,enabled
                (return-from ,type-block ,on-failure-form)))))
        ,@body))))

(defun %json-type-to-cffi-type (json-entry)
  (with-json-values (json-entry tag)
    (let ((cffi-type
           (cond
             ((alexandria:switch (tag :test 'equal)
                (":void"               :void)
                (":_Bool"              :bool)
                ;; regarding :signed-char see https://stackoverflow.com/questions/436513/char-signed-char-char-unsigned-char
                (":char"               :char)
                (":signed-char"        :char)
                (":unsigned-char"      :unsigned-char)
                (":short"              :short)
                (":unsigned-short"     :unsigned-short)
                (":int"                :int)
                (":unsigned-int"       :unsigned-int)
                (":long"               :long)
                (":unsigned-long"      :unsigned-long)
                (":long-long"          :long-long)
                (":unsigned-long-long" :unsigned-long-long)
                (":float"              :float)
                (":double"             :double)
                ;; TODO FIXME
                ;;(":long-double"        :long-double)
                )
              ;; return the result of the condition expression
              )
             ((or (progn
                    (assert (not (member tag +c-builtin-types+ :test 'equal)) ()
                            "Not all C basic types are covered! The outlier is: ~S" tag)
                    nil)
                  (equal tag ":struct")
                  (equal tag ":union"))
              ;; ":struct" is a "struct foo-struct var" kind of reference
              (expected-json-keys json-entry :name :tag :id)
              (with-json-values (json-entry name id)
                (let* ((kind (if (equal tag ":struct")
                                 :struct
                                 :union))
                       (cffi-name (if name
                                      (json-name-to-cffi-name name kind)
                                      (lookup-anon-entity id))))
                  (find-cffi-type-or-die cffi-name kind)
                  `(,kind ,cffi-name))))
             ((or (equal tag "struct")
                  (equal tag "union"))
              ;; "struct" denotes a "struct {} var", or "typedef struct {} my_type"
              ;; kind of inline anonymous declaration. Let's call PROCESS-C2FFI-ENTRY
              ;; to emit it for us, and return with the generated name (first value)
              ;; as if it was a standalone toplevel struct definition.
              ;; TODO is it a problem that we don't invoke the CALLBACK-FACTORY stuff here?
              (let ((form (process-c2ffi-entry json-entry))
                    (kind (if (equal tag "struct")
                              :struct
                              :union)))
                (assert (and (consp form)
                             (member (first form) '(cffi:defcstruct cffi:defcunion))))
                `(,kind ,(first (alexandria:ensure-list (second form))))))
             ((equal tag ":enum")
              ;; ":enum" is an "enum foo var" kind of reference
              (expected-json-keys json-entry :name :tag :id)
              (with-json-values (json-entry name id)
                (let ((cffi-name (json-name-to-cffi-name (or name
                                                             (lookup-anon-entity id))
                                                         :enum)))
                  (find-cffi-type-or-die cffi-name :enum)
                  ;; TODO FIXME this would be the proper one, but CFFI is broken: `(:enum ,cffi-name)
                  cffi-name)))
             ((equal tag "enum")
              ;; "enum" is an inline "typedef enum {m1, m2} var" kind of inline declaration
              (expected-json-keys json-entry :name :tag :id)
              ;; TODO FIXME similarly to struct, but it would be nice to see an example
              (error "not yet implemented"))
             ((equal tag ":array")
              (expected-json-keys json-entry :tag :type :size)
              (with-json-values (json-entry type size)
                (check-type size integer)
                `(:array ,(json-type-to-cffi-type type) ,size)))
             ((equal tag ":pointer")
              (expected-json-keys json-entry :tag :type :id)
              (with-json-values (json-entry type)
                `(:pointer ,(with-allowed-foreign-type-errors
                                (:void :enabled *allow-pointer-type-simplification*)
                              (json-type-to-cffi-type type)))))
             ((equal tag ":function-pointer")
              (expected-json-keys json-entry :tag)
              (function-pointer-type-name))
             ((equal tag ":function")
              (unsupported-type json-entry))
             (t
              (assert (not (alexandria:starts-with #\: tag)))
              (let ((cffi-name (json-name-to-cffi-name tag :type)))
                ;; TODO FIXME json-name-to-cffi-name collects the mentioned
                ;; types to later emit +TYPE-NAMES+, but if this next
                ;; find-cffi-type-or-die dies then the entire function is
                ;; skipped.
                (find-cffi-type-or-die cffi-name)
                cffi-name)))))
      (assert cffi-type () "Failed to map ~S to a cffi type" json-entry)
      cffi-type)))

(defun should-export-p (symbol)
  (and symbol
       (symbolp symbol)
       (not (keywordp symbol))
       *ffi-name-export-predicate*
       (call-hook *ffi-name-export-predicate* symbol)))

(defun json-type-to-cffi-type (json-entry &optional (context nil context?))
  (let ((cffi-type (%json-type-to-cffi-type json-entry)))
    (if context?
        (call-hook *ffi-type-transformer* cffi-type context)
        cffi-type)))

;;;;;;
;;; Entry point, the "API"

(defun process-c2ffi-spec-file (c2ffi-spec-file package-name
                                &key
                                  (allow-pointer-type-simplification *allow-pointer-type-simplification*)
                                  (allow-skipping-struct-fields *allow-skipping-struct-fields*)
                                  (assume-struct-by-value-support *assume-struct-by-value-support*)
                                  ;; either a pathname or a string (will be copied as is),
                                  ;; or a function that will be funcall'd with one argument
                                  ;; to emit a form (i.e. OUTPUT/CODE).
                                  prelude
                                  (output (make-pathname :name (uiop/utility:strcat (pathname-name c2ffi-spec-file) ".cffi-tmp")
                                                         :type "lisp" :defaults c2ffi-spec-file))
                                  (output-encoding UIOP/STREAM:*DEFAULT-ENCODING*)
                                  ;; The args following this point are mirrored in the ASDF
                                  ;; component on the same name.
                                  (ffi-name-transformer *ffi-name-transformer*)
                                  (ffi-name-export-predicate *ffi-name-export-predicate*)
                                  ;; as per CFFI:DEFINE-FOREIGN-LIBRARY and CFFI:LOAD-FOREIGN-LIBRARY
                                  (ffi-type-transformer *ffi-type-transformer*)
                                  (callback-factory *callback-factory*)
                                  foreign-library-name
                                  foreign-library-spec
                                  (emit-generated-name-mappings t)
                                  (include-sources :all)
                                  exclude-sources
                                  (include-definitions :all)
                                  exclude-definitions)
  "Generates a lisp file with CFFI definitions from C2FFI-SPEC-FILE.
PACKAGE-NAME will be overwritten, it assumes full control over the
target package."
  (check-type c2ffi-spec-file (or pathname string))
  (macrolet ((@ (var)
                 `(setf ,var (compile-rules ,var))))
    (@ include-sources)
    (@ exclude-sources)
    (@ include-definitions)
    (@ exclude-definitions))
  (with-standard-io-syntax
    (alexandria:with-input-from-file (in c2ffi-spec-file :external-format (uiop:encoding-external-format :utf-8))
      (alexandria:with-output-to-file (*c2ffi-output-stream* output :if-exists :supersede
                            :external-format (uiop:encoding-external-format output-encoding))
        (let* ((*package* (or (find-package package-name)
                              (make-package package-name)))
               ;; Make sure we use an uninterned symbol, so that it's neutral to READTABLE-CASE.
               (package-name (make-symbol (package-name *package*)))
               ;; Let's rebind a copy, so that when we are done with
               ;; the generation (which also EVAL's the forms) then
               ;; the CFFI type repository is also reverted back to
               ;; the previous state. This avoids redefinition warning
               ;; when the generated file gets compiled and loaded
               ;; later.
               (cffi::*default-type-parsers* (alexandria:copy-hash-table cffi::*default-type-parsers*))
               (cffi::*struct-type-parsers* (alexandria:copy-hash-table cffi::*struct-type-parsers*))
               (cffi::*union-type-parsers* (alexandria:copy-hash-table cffi::*union-type-parsers*))
               (*anon-name-counter* 0)
               (*anon-entities* (make-hash-table))
               (*generated-names* (mapcar (lambda (key)
                                            `(,key . ,(make-hash-table :test 'equal)))
                                          +name-kinds+))
               (*allow-pointer-type-simplification* allow-pointer-type-simplification)
               (*allow-skipping-struct-fields* allow-skipping-struct-fields)
               (*assume-struct-by-value-support* assume-struct-by-value-support)
               (*ffi-name-transformer* (canonicalize-transformer-hook ffi-name-transformer))
               (*ffi-name-export-predicate* (canonicalize-transformer-hook ffi-name-export-predicate))
               (*ffi-type-transformer* (canonicalize-transformer-hook ffi-type-transformer))
               (*callback-factory* (canonicalize-transformer-hook callback-factory))
               (*read-default-float-format* 'double-float)
               (json (json:decode-json in)))
          (output/string +generated-file-header+)
          ;; some forms that are always emitted
          (mapc 'output/code
                ;; Make sure the package exists. We don't even want to :use COMMON-LISP here,
                ;; to avoid any possible name clashes.
                `((uiop:define-package ,package-name (:use))
                  (in-package ,package-name)
                  (cffi:defctype ,(function-pointer-type-name) :pointer)))
          (when (and foreign-library-name
                     foreign-library-spec)
            (when (stringp foreign-library-name)
              (setf foreign-library-name (uiop/stream:safe-read-from-string foreign-library-name)))
            (output/code `(cffi:define-foreign-library ,foreign-library-name
                            ,@foreign-library-spec))
            ;; TODO: Unconditionally emitting a USE-FOREIGN-LIBRARY may not be smart.
            ;; For details see: https://github.com/cffi/cffi/issues/272
            (output/code `(cffi:use-foreign-library ,foreign-library-name)))
          (etypecase prelude
            (null)
            (string
             (output/string prelude))
            (pathname
             (alexandria:with-input-from-file (prelude-stream prelude)
               (alexandria:copy-stream prelude-stream *c2ffi-output-stream*
                                       :element-type 'character)))
            ((or symbol function)
             (funcall prelude 'output/code)))
          ;;
          ;; Let's enumerate the entries
          (multiple-value-bind (form-callback epilogue-callback)
              (funcall *callback-factory*)
            (dolist (json-entry json)
              (with-json-values (json-entry name location)
                (let ((source-location-file (subseq location
                                                    0
                                                    (or (position #\: location)
                                                        0))))
                  (if (include-definition?
                       name source-location-file
                       include-definitions exclude-definitions
                       include-sources exclude-sources)
                      (progn
                        (output/string "~&~%;; ~S" location)
                        (let ((emitted-definition (process-c2ffi-entry json-entry)))
                          ;;
                          ;; Call the plugin to let the user emit a form after the given
                          ;; definition
                          (when (and emitted-definition
                                     form-callback)
                            (map nil 'output/code (call-hook form-callback emitted-definition)))))
                      (output/string "~&;; Skipped ~S due to filters" name)))))
            ;;
            ;; Call the plugin to let the user append multiple forms after the
            ;; emitted definitions
            (when epilogue-callback
              (map nil 'output/code (call-hook epilogue-callback))))
          ;;
          ;; emit optional exports
          (maphash
           (lambda (package-name symbols)
             (output/export (sort (remove-if-not #'should-export-p symbols) #'string<)
                            package-name))
           (get-all-names-by-package *generated-names*))

          ;;
          ;; emit optional mappings
          (when emit-generated-name-mappings
            (mapcar (lambda (entry)
                      (destructuring-bind (kind variable-name) entry
                        (output/code `(defparameter
                                          ,(intern (symbol-name variable-name))
                                        ',(alexandria:hash-table-alist (cdr (assoc kind *generated-names*)))))))
                    `((:function #:+function-names+)
                      (:struct   #:+struct-names+)
                      (:union    #:+union-names+)
                      (:variable #:+variable-names+)
                      (:type     #:+type-names+)
                      (:constant #:+constant-names+)
                      (:argument #:+argument-names+)
                      (:field    #:+field-names+))))))))
  output)

(defun get-all-names-by-package (name-collection)
  (let ((tables (mapcar #'cdr name-collection))
        all
        (grouped (make-hash-table)))
    (loop :for table :in tables :do
         (loop :for s :being :the :hash-values :of table :do
            (push s all)))
    (remove-duplicates all :test #'eq)
    (loop :for name :in all
       :for package-name := (package-name (symbol-package name))
       :do (setf (gethash package-name grouped)
                 (cons name (gethash package-name grouped))))
    grouped))

;;;;;;
;;; Processors for various definitions

(defvar *c2ffi-entry-processors* (make-hash-table :test 'equal))

(defun process-c2ffi-entry (json-entry)
  (let* ((kind (json-value json-entry :tag))
         (processor (gethash kind *c2ffi-entry-processors*)))
    (if processor
        (let ((definition-form
               (handler-bind
                   ((unsupported-type
                     (lambda (e)
                       (warn "Skip definition because cannot map ~S to any CFFI type. The definition is ~S"
                             (json-definition-of e) json-entry)
                       (return-from process-c2ffi-entry (values))))
                    (cffi::undefined-foreign-type-error
                     (lambda (e)
                       (output/string "~&;; Skipping definition ~S because of missing type ~S"
                                      json-entry (cffi::foreign-type-error/compound-name e))
                       (return-from process-c2ffi-entry (values)))))
                 (funcall processor json-entry))))
          (when definition-form
            (output/code definition-form)
            definition-form))
        (progn
          (warn "No cffi/c2ffi processor defined for ~A" json-entry)
          (values)))))

(defmacro define-processor (kind args &body body)
  `(setf (gethash ,(string-downcase kind) *c2ffi-entry-processors*)
         (alexandria:named-lambda ,(alexandria:symbolicate 'c2ffi-processor/ kind) (-json-entry-)
           (with-json-values (-json-entry- ,@args)
             ,@body))))

(defun %process-struct-like (json-entry kind definer anon-base-name)
  (expected-json-keys json-entry :tag :ns :name :id :bit-size :bit-alignment :fields)
  (with-json-values (json-entry tag (struct-name :name) fields bit-size id)
    (assert (member tag '(":struct" "struct" ":union" "union") :test 'equal))
    (flet ((process-field (json-entry)
             (with-json-values (json-entry (field-name :name) bit-offset type)
               (let ((cffi-type (with-allowed-foreign-type-errors
                                    ('failed :enabled *allow-skipping-struct-fields*)
                                  (json-type-to-cffi-type type `(,kind ,struct-name ,field-name)))))
                 (if (eq cffi-type 'failed)
                     (output/string "~&;; skipping field due to missing type ~S, full json entry: ~S" type json-entry)
                     `(,(json-name-to-cffi-name field-name :field)
                        ,cffi-type
                       ,@(unless (eq kind :union)
                                 `(:offset ,(coerce-to-byte-size bit-offset)))))))))
      `(,definer (,(json-name-to-cffi-name (or struct-name
                                               (register-anon-entity
                                                id
                                                (generate-anon-name anon-base-name)))
                                           kind
                                           (null struct-name))
                   :size ,(coerce-to-byte-size bit-size))
           ,@(remove nil (mapcar #'process-field fields))))))

(define-processor struct ()
  (%process-struct-like -json-entry- :struct 'cffi:defcstruct '#:anon-struct-))

(define-processor union ()
  (%process-struct-like -json-entry- :union 'cffi:defcunion '#:anon-union-))

(define-processor typedef (name type)
  (expected-json-keys -json-entry- :tag :name :ns :type)
  `(cffi:defctype ,(json-name-to-cffi-name name :type)
       ,(json-type-to-cffi-type type `(:typedef ,name))))

(define-processor function (return-type (function-name :name) parameters inline variadic storage-class)
  (declare (ignore storage-class))
  ;; TODO does storage-class matter for FFI accessibility?
  #+nil
  (assume (equal "extern" storage-class)
          "Unexpected function STORAGE-CLASS: ~S for function ~S" storage-class function-name)
  (expected-json-keys -json-entry- :tag :name :return-type :parameters :variadic :inline :storage-class :ns)
  (let ((uses-struct-by-value? nil))
    (flet ((process-arg (json-entry index)
             (expected-json-keys json-entry :tag :name :type)
             (with-json-values (json-entry tag (argument-name :name) type)
               (assert (equal tag "parameter"))
               (let* ((cffi-type (json-type-to-cffi-type type `(:function ,function-name ,argument-name)))
                      (canonicalized-type (cffi::canonicalize-foreign-type cffi-type)))
                 (when (and (consp canonicalized-type)
                            (member (first canonicalized-type) '(:struct :union)))
                   (setf uses-struct-by-value? t))
                 `(,(if argument-name
                        (json-name-to-cffi-name argument-name :argument)
                        (alexandria:symbolicate '#:arg (princ-to-string index)))
                    ,cffi-type)))))
      (let ((cffi-args (loop
                         :for arg :in parameters
                         :for index :upfrom 1
                         :collect (process-arg arg index))))
        (cond
          ((and uses-struct-by-value?
                (not *assume-struct-by-value-support*))
           (values))
          (inline
           ;; TODO inline functions should go into a separate grovel file?
           (output/string "~&;; Skipping inline function ~S" function-name)
           (values))
          (t `(cffi:defcfun (,function-name ,(json-name-to-cffi-name function-name :function))
                  ,(json-type-to-cffi-type return-type `(:function ,function-name :return-type))
              ,@(append cffi-args
                        (when variadic
                          '(&rest))))))))))

(define-processor extern (name type)
  (expected-json-keys -json-entry- :tag :name :type)
  `(cffi:defcvar (,name ,(json-name-to-cffi-name name :variable))
       ,(json-type-to-cffi-type type `(:variable ,name))))

;; ((TAG . enum) (NS . 0) (NAME . ) (ID . 3) (LOCATION . /usr/include/bits/confname.h:24:1) (FIELDS ((TAG . field) (NAME . _PC_LINK_MAX) (VALUE . 0)) ((TAG . field) (NAME . _PC_MAX_CANON) (VALUE . 1)) ((TAG . field) (NAME . _PC_MAX_INPUT) (VALUE . 2)) ((TAG . field) (NAME . _PC_NAME_MAX) (VALUE . 3)) ((TAG . field) (NAME . _PC_PATH_MAX) (VALUE . 4)) ((TAG . field) (NAME . _PC_PIPE_BUF) (VALUE . 5)) ((TAG . field) (NAME . _PC_CHOWN_RESTRICTED) (VALUE . 6)) ((TAG . field) (NAME . _PC_NO_TRUNC) (VALUE . 7)) ((TAG . field) (NAME . _PC_VDISABLE) (VALUE . 8)) ((TAG . field) (NAME . _PC_SYNC_IO) (VALUE . 9)) ((TAG . field) (NAME . _PC_ASYNC_IO) (VALUE . 10)) ((TAG . field) (NAME . _PC_PRIO_IO) (VALUE . 11)) ((TAG . field) (NAME . _PC_SOCK_MAXBUF) (VALUE . 12)) ((TAG . field) (NAME . _PC_FILESIZEBITS) (VALUE . 13)) ((TAG . field) (NAME . _PC_REC_INCR_XFER_SIZE) (VALUE . 14)) ((TAG . field) (NAME . _PC_REC_MAX_XFER_SIZE) (VALUE . 15)) ((TAG . field) (NAME . _PC_REC_MIN_XFER_SIZE) (VALUE . 16)) ((TAG . field) (NAME . _PC_REC_XFER_ALIGN) (VALUE . 17)) ((TAG . field) (NAME . _PC_ALLOC_SIZE_MIN) (VALUE . 18)) ((TAG . field) (NAME . _PC_SYMLINK_MAX) (VALUE . 19)) ((TAG . field) (NAME . _PC_2_SYMLINKS) (VALUE . 20))))
(define-processor enum (name fields id)
  (let ((bitmasks 0)
        (non-bitmasks 0))
    (labels
        ((for-bitmask-statistics (name value)
           (declare (ignore name))
           (if (cffi::single-bit-p value)
               (incf bitmasks)
               (incf non-bitmasks)))
         (for-enum-body (name value)
           `(,(json-name-to-cffi-name name :member)
              ,value))
         (process-fields (visitor)
           (loop
             :for json-entry :in fields
             :do (expected-json-keys json-entry :tag :name :value)
             :collect
             (with-json-values (json-entry tag name value)
               (assert (equal tag "field"))
               (check-type value integer)
               (funcall visitor name value)))))
      (process-fields #'for-bitmask-statistics)
      `(,(if (> (/ bitmasks
                   (+ non-bitmasks bitmasks))
                0.8)
             'cffi:defbitfield
             'cffi:defcenum)
           ,(json-name-to-cffi-name (or name
                                        (register-anon-entity
                                         id
                                         (generate-anon-name '#:anon-enum-)))
                                    :enum
                                    (null name))
         ,@(process-fields #'for-enum-body)))))

(defun make-define-constant-form (name value)
  (valid-name-or-die name)
  (let ((test-fn (typecase value
                   (number)
                   (t 'equal))))
    `(alexandria:define-constant ,(json-name-to-cffi-name name :constant)
         ,value ,@(when test-fn `(:test ',test-fn)))))

(define-processor const (name type (value :value :otherwise nil))
  (expected-json-keys -json-entry- :tag :name :type :value :ns)
  (let ((cffi-type (json-type-to-cffi-type type `(:contant ,name))))
    (cond
      ((not value)
       ;; #define __FOO_H and friends... just ignore them.
       (values))
      ((and (member cffi-type '(:int :unsigned-int
                                :long :unsigned-long
                                :long-long :unsigned-long-long))
            (integerp value))
       (make-define-constant-form name value))
      ((and (member cffi-type '(:float :double))
            (floatp value))
       (make-define-constant-form name value))
      ((member cffi-type '(:string (:pointer :char)) :test 'equal)
       (make-define-constant-form name value))
      (t
       (warn "Don't know how to emit a constant of CFFI type ~S, with value ~S (json type is ~S)." cffi-type value type)
       (values)))))
