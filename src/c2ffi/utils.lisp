(in-package "CFFI/C2FFI")

;;; read a c2ffi tmp file to get struct definitions

(defun find-matching-forms (car-form cadr-form file &key (package *package*))
  (unless (packagep package) (setq package (find-package package)))
  (assert (packagep package))
  (let (ret *read-eval* (*package* package))
    (with-open-file (stream file)
      (loop (let ((form (read stream nil 'eof)))
	      (if (eql form 'eof) (return ret))
	      (assert (symbolp (car form)))
	      (when (or (cl:null car-form)
			(string-equal car-form (car form))
			(and (user::prefixp "DEF" (symbol-name (car form)))
			     (string-equal
			      car-form
			      (subseq (symbol-name (car form)) 3))))
		(when (or (and (cl:null cadr-form) (not (cl:null car-form)))
			  (etypecase (cadr form)
			    (atom (string-equal cadr-form (cadr form)))
			    (cons (string-equal cadr-form (car (cadr form))))))
		  (push form ret))))))))

(defun get-cstruct-slot-names (name file)
  (let ((forms (or (find-matching-forms "DEFCSTRUCT" name file)
		   (find-matching-forms "DEFCUNION" name file))))
    (cond ((not forms)
	   (setq forms
		 (find-matching-forms "DEFCTYPE" name file))
	   (when forms
	     (assert (endp (cdr forms)))
	     (let ((target (third (car forms))))
	       (when (eql (first target) :struct)
		 (get-cstruct-slot-names (second target) file)))))
	  (t
	   (assert (endp (cdr forms)))
	   (mapcar #'car (cddr (car forms)))))))

(defun print-cstruct-slots (struct-ptr struct-type file)
  (let ((struct-name (etypecase struct-type
		       (atom struct-type)
		       (cons (assert (eql :struct (car struct-type)))
			     (assert (symbolp (cadr struct-type)))
			     (assert (endp (cdddr struct-type)))
			     (car struct-type)))))
    (loop for slot-name in (get-cstruct-slot-names struct-name file)
	  collect (cons slot-name
			(cffi:foreign-slot-value struct-ptr
						 struct-type slot-name)))))

#||
(setq $f "/dev/shm/xcb-c2ffi/xcb-includes.x86_64-pc-linux-gnu.cffi-tmp.lisp")
(find-matching-forms "DEFCTYPE" '|xcb_generic_error_t| $f)
(get-cstruct-slot-names '|xcb_generic_error_t| $f)
||#
