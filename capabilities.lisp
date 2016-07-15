(in-package #:terminfo)

(defvar *terminfo* nil
  "The global terminfo structure used as a default variable
for all commands with an optional terminfo parameter.  This
is set any time set-terminal is called.")

(defvar *capabilities* (make-hash-table :size 494)
  "")

(flet ((required-argument ()
	 (error "A required argument was not supplied.")))
  (defstruct (terminfo
	       (:print-function
		(lambda (object stream depth)
		  (declare (ignore depth))
		  (print-unreadable-object (object stream :type t :identity t)
		    (format stream "~A" (first (terminfo-names object)))))))
    (names (required-argument) :type list :read-only t)
    (booleans (required-argument) :type (simple-array (member t nil) (*)))
    (numbers (required-argument) :type (simple-array (signed-byte 16) (*)))
    (strings (required-argument) :type (simple-array t (*)))))

(defun %capability (name terminfo)
  (let ((whatsit (gethash name *capabilities*)))
    (when (null whatsit)
      (error "Terminfo capability ~S doesn't exist." name))
    (if (or (null terminfo) (>= (cdr whatsit)
				(length (funcall (car whatsit) terminfo))))
	nil #| default |#
	(let ((value (aref (funcall (car whatsit) terminfo) (cdr whatsit))))
	  (if (and (numberp value) (minusp value))
	      nil
	      value)))))

(declaim (inline capability))
(defun capability (name &optional (terminfo *terminfo*))
  "Return the contents of the terminfo database for the given parameter.
Returns nil for undefined capabilities."
  (%capability name terminfo))

(define-compiler-macro capability (&whole form
				   name &optional (terminfo '*terminfo*))
  (if (not (keywordp name))
      form
      (let ((value (gensym))
	    (tmp (gensym)))
	(unless (gethash name *capabilities*)
	  (warn "Terminfo capability ~S doesn't exist." name))
	`(let ((,value (load-time-value (cons nil nil)))
	       (,tmp ,terminfo))
	   (if (eq (car ,value) ,tmp)
	       (cdr ,value)
	       (setf (car ,value) ,tmp
		     (cdr ,value) (%capability ,name ,tmp)))))))

(defun capabilities (&optional (terminfo *terminfo*))
  "Return a list of capabilities for terminfo that are not nil."
  (let (result)
    (maphash
     (lambda (key value)
       (declare (ignore value))
       (when (capability key terminfo)
         (push key result)))
     *capabilities*)
    result))

(defmacro defcap (name type index &optional docstring)
  "Along with defining the capability information:
name
type: boolean integer or string
index
defcap automaticlly defines and exports a symbol-macro
that calls the capability from *terminfo*."
  (let ((thing (ecase type              ; indicates the accessor into the terminfo structure
		 (boolean 'terminfo-booleans)
		 (integer 'terminfo-numbers)
		 (string 'terminfo-strings)))
	(symbol (intern (string name) "KEYWORD")))
    `(progn
       (eval-when (:compile-toplevel)
	 ;; Mark capability as valid for the compiler-macro; needed when
	 ;; compiling TPUTS.  If there's already a value present, leave
	 ;; it alone, else just put any non-NIL value there; it'll get
	 ;; fixed up when the file is loaded.
	 (setf (gethash ,symbol *capabilities*)
	       (gethash ,symbol *capabilities* t)))
       (setf (gethash ,symbol *capabilities*) (cons #',thing ,index))
       (define-symbol-macro ,name (capability ,symbol *terminfo*))
       ,(when docstring
	  `(setf (documentation ',name 'variable) ,docstring))
       (export ',name "TERMINFO"))))
