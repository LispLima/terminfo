;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: TERMINFO -*-

;;; Copyright ,A)(B 2001 Paul Foley (mycroft@actrix.gen.nz)
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this Software to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; provided that the above copyright notice and this permission notice
;;; are included in all copies or substantial portions of the Software.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.

(in-package "TERMINFO")

(defvar *terminfo-directories* '("/etc/terminfo/"
				 "/lib/terminfo/"
				 "/usr/share/terminfo/"
				 "/usr/share/misc/terminfo/")
  "Known locations of terminfo databases.")



(defun load-terminfo (name)
  (let ((name (concatenate 'string #-darwin (string (char name 0))
                           #+darwin (format nil "~X" (char-code (char name 0)))
			   "/" name)))
    (dolist (path (list* (merge-pathnames
                          (make-pathname :directory '(:relative ".terminfo"))
                          (user-homedir-pathname))
			 *terminfo-directories*))
      (with-open-file (stream (merge-pathnames name path)
                              :direction :input
                              :element-type '(unsigned-byte 8)
                              :if-does-not-exist nil)
        (when stream
          (flet ((read-short (stream)
                             (let ((n (+ (read-byte stream) (* 256 (read-byte stream)))))
                               (if (> n 32767)
                                   (- n 65536)
                                 n)))
                 (read-string (stream)
                              (do ((c (read-byte stream) (read-byte stream))
                                   (s '()))
                                  ((zerop c) (coerce (nreverse s) 'string))
                                (push (code-char c) s))))
            (let* ((magic (let ((whosit (read-short stream)))
                            (if (= whosit #o432)
                                whosit
                              (error "Invalid file format"))))
                   (sznames (read-short stream))
                   (szbooleans (read-short stream))
                   (sznumbers (read-short stream))
                   (szstrings (read-short stream))
                   (szstringtable (read-short stream))
                   (names (let ((string (read-string stream)))
                            (loop for i = 0 then (1+ j)
                                  as j = (position #\| string :start i)
                                  collect (subseq string i j) while j)))
                   (booleans (make-array szbooleans
                                         :element-type '(or t nil)
                                         :initial-element nil))
                   (numbers (make-array sznumbers
                                        :element-type '(signed-byte 16)
                                        :initial-element -1))
                   (strings (make-array szstrings
                                        :element-type '(signed-byte 16)
                                        :initial-element -1))
                   (stringtable (make-string szstringtable))
                   (count 0))
              (dotimes (i szbooleans)
                (setf (aref booleans i) (not (zerop (read-byte stream)))))
              (when (oddp (+ sznames szbooleans))
                (read-byte stream))
              (dotimes (i sznumbers)
                (setf (aref numbers i) (read-short stream)))
              (dotimes (i szstrings)
                (unless (minusp (setf (aref strings i) (read-short stream)))
                  (incf count)))
              (dotimes (i szstringtable)
                (setf (char stringtable i) (code-char (read-byte stream))))
              (let ((xtrings (make-array szstrings :initial-element nil)))
                (dotimes (i szstrings)
                  (unless (minusp (aref strings i))
                    (setf (aref xtrings i)
                          (subseq stringtable (aref strings i)
                                  (position #\Null stringtable
                                            :start (aref strings i))))))
                (setq strings xtrings))
              (return (make-terminfo :names names :booleans booleans
                                     :numbers numbers :strings strings)))))))))

(defun xform (value format flags width precision)
  (let ((temp (make-array 8 :element-type 'character :fill-pointer 0
			  :adjustable t)))
    (flet ((shift (n c sign)
	     (let ((len (length temp)) (s 0))
	       (when (and sign (> len 0) (char= (char temp 0) #\-))
		 (setq len (1- len) s 1))
	       (when (> (+ len n s) (array-dimension temp 0))
		 (adjust-array temp (+ len n 5)))
	       (incf (fill-pointer temp) n)
	       (replace temp temp
			:start1 (+ n s) :start2 s :end2 (+ s len))
	       (fill temp c :start s :end (+ n s)))))
      (format temp (ecase format
		     (#\d "~D") (#\o "~O") (#\x "~(~X~)") (#\X "~:@(~X~)")
		     (#\s "~A"))
	      value)
      (when (position format "doxX")
	(let ((len (length temp)))
	  (when (minusp value) (decf len))
	  (when (< len precision) (shift (- precision len) #\0 t)))
	(when (logbitp 0 flags)
	  (case format
	    (#\o (unless (char= (char temp (if (minusp value) 1 0)) #\0)
		   (shift 1 #\0 t)))
	    (#\x (shift 1 #\x t) (shift 1 #\0 t))
	    (#\X (shift 1 #\X t) (shift 1 #\0 t))))
	(unless (minusp value)
	  (cond ((logbitp 1 flags) (shift 1 #\+ nil))
		((logbitp 2 flags) (shift 1 #\Space nil)))))
      (when (and (eql format #\s) (> precision 0) (> (length temp) precision))
	(setf (fill-pointer temp) precision))
      (when (< (length temp) width)
	(if (logbitp 3 flags)
	    (shift (- width (length temp)) #\Space nil)
	    (dotimes (i (- width (length temp)))
	      (vector-push-extend #\Space temp))))
      temp)))

(defun skip-forward (stream flag)
  (do ((level 0) (c (read-char stream nil) (read-char stream nil)))
      ((null c))
    (when (char= c #\%)
      (setq c (read-char stream))
      (cond ((char= c #\?) (incf level))
	    ((char= c #\;) (when (minusp (decf level)) (return t)))
	    ((and flag (char= c #\e) (= level 0)) (return t))))))

(defun tparm (string &rest args)
  "Return the string representing the command and arguments."
  (when (null string) (return-from tparm ""))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (do ((stack '()) (flags 0) (width 0) (precision 0) (number 0)
	   (dvars (make-array 26 :element-type '(unsigned-byte 8)
			      :initial-element 0))
	   (svars (load-time-value
		   (make-array 26 :element-type '(unsigned-byte 8)
			       :initial-element 0)))
	   (c (read-char in nil) (read-char in nil)))
	  ((null c))
	(cond ((char= c #\%)
	       (setq c (read-char in) flags 0 width 0 precision 0)
	       (tagbody
		state0
		  (case c
		    (#\% (princ c out) (go terminal))
		    (#\: (setq c (read-char in)) (go state2))
		    (#\+ (go state1))
		    (#\- (go state1))
		    (#\# (go state2))
		    (#\Space (go state2))
		    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (go state3))
		    (#\d (go state5))
		    (#\o (go state6))
		    ((#\X #\x) (go state7))
		    (#\s (go state8))
		    (#\c (princ (code-char (pop stack)) out) (go terminal))
		    (#\p (go state9))
		    (#\P (go state10))
		    (#\g (go state11))
		    (#\' (go state12))
		    (#\{ (go state13))
		    (#\l (push (length (pop stack)) stack) (go terminal))
		    (#\* (push (* (pop stack) (pop stack)) stack)
			 (go terminal))
		    (#\/ (push (let ((n (pop stack))) (/ (pop stack) n)) stack)
			 (go terminal))
		    (#\m (push (let ((n (pop stack))) (mod (pop stack) n))
			       stack)
			 (go terminal))
		    (#\& (push (logand (pop stack) (pop stack)) stack)
			 (go terminal))
		    (#\| (push (logior (pop stack) (pop stack)) stack)
			 (go terminal))
		    (#\^ (push (logxor (pop stack) (pop stack)) stack)
			 (go terminal))
		    (#\= (push (if (= (pop stack) (pop stack)) 1 0) stack)
			 (go terminal))
		    (#\> (push (if (<= (pop stack) (pop stack)) 1 0) stack)
			 (go terminal))
		    (#\< (push (if (>= (pop stack) (pop stack)) 1 0) stack)
			 (go terminal))
		    (#\A (push (if (or (zerop (pop stack))
				       (zerop (pop stack)))
				   0
				   1)
			       stack)
			 (go terminal))
		    (#\O (push (if (and (zerop (pop stack))
					(zerop (pop stack)))
				   0
				   1)
			       stack)
			 (go terminal))
		    (#\! (push (if (zerop (pop stack)) 1 0) stack)
			 (go terminal))
		    (#\~ (push (logand #xFF (lognot (pop stack))) stack)
			 (go terminal))
		    (#\i (when args
			   (incf (first args))
			   (when (cdr args)
			     (incf (second args))))
			 (go terminal))
		    (#\? (go state14))
		    (#\t (go state15))
		    (#\e (go state16))
		    (#\; (go state17))
		    (otherwise (error "Unknown %-control character: ~C" c)))
		state1
		  (let ((next (peek-char nil in nil)))
		    (when (position next "0123456789# +-doXxs")
		      (go state2)))
		  (if (char= c #\+)
		      (push (+ (pop stack) (pop stack)) stack)
		      (push (let ((n (pop stack))) (- (pop stack) n)) stack))
		  (go terminal)
		state2
		  (case c
		    (#\# (setf flags (logior flags 1)))
		    (#\+ (setf flags (logior flags 2)))
		    (#\Space (setf flags (logior flags 4)))
		    (#\- (setf flags (logior flags 8)))
		    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		     (go state3))
		    (t (go blah)))
		  (setf c (read-char in))
		  (go state2)
		state3
		  (setf width (digit-char-p c))
		state3-loop
		  (setf c (read-char in))
		  (case c
		    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		     (setf width (+ (* width 10) (digit-char-p c)))
		     (go state3-loop))
		    (#\. (setf c (read-char in)) (go state4)))
		  (go blah)
		state4
		  (setf precision (digit-char-p c))
		state4-loop
		  (setf c (read-char in))
		  (case c
		    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		     (setf precision (+ (* precision 10) (digit-char-p c)))
		     (go state4-loop)))
		  (go blah)
		blah
		  (case c
		    (#\d (go state5))
		    (#\o (go state6))
		    ((#\X #\x) (go state7))
		    (#\s (go state8))
		    (otherwise (error "Unknown %-control character: ~C" c)))
		state5
		state6
		state7
		state8
		  (princ (xform (pop stack) c flags width precision) out)
		  (go terminal)
		state9
		  (let* ((i (digit-char-p (read-char in)))
			 (a (nth (1- i) args)))
		    (etypecase a
		      (character (push (char-code a) stack))
		      (integer (push a stack))
		      (string (push a stack))))
		  (go terminal)
		state10
		  (let ((var (read-char in)))
		    (cond ((char<= #\a var #\z)
			   (setf (aref dvars (- (char-code var)
						(char-code #\a)))
				 (pop stack)))
			  ((char<= #\A var #\Z)
			   (setf (aref svars (- (char-code var)
						(char-code #\A)))
				 (pop stack)))
			  (t (error "Illegal variable name: ~C" var))))
		  (go terminal)
		state11
		  (let ((var (read-char in)))
		    (cond ((char<= #\a var #\z)
			   (push (aref dvars (- (char-code var)
						(char-code #\a)))
				 stack))
			  ((char<= #\A var #\Z)
			   (push (aref svars (- (char-code var)
						(char-code #\A)))
				 stack))
			  (t (error "Illegal variable name: ~C" var))))
		  (go terminal)
		state12
		  (push (char-code (read-char in)) stack)
		  (unless (char= (read-char in) #\')
		    (error "Invalid character constant"))
		  (go terminal)
		state13
		  (setq number 0)
		state13-loop
		  (setq c (read-char in))
		  (let ((n (digit-char-p c)))
		    (cond (n (setq number (+ (* 10 number) n))
			     (go state13-loop))
			  ((char= c #\})
			   (push number stack)
			   (go terminal))))
		  (error "Invalid integer constant")
		state14
		  (go terminal)
		state15
		  (when (/= (pop stack) 0)
		    (go terminal))
		  (skip-forward in t)
		  (go terminal)
		state16
		  (skip-forward in nil)
		state17
		terminal
		  #| that's all, folks |#))
	      (t (princ c out)))))))

(defgeneric stream-fileno (stream)
  (:method ((stream stream))
    nil)
  #+CMU
  (:method ((stream sys:fd-stream))
    (sys:fd-stream-fd stream))
  (:method ((stream two-way-stream))
    (stream-fileno (two-way-stream-output-stream stream)))
  (:method ((stream synonym-stream))
    (stream-fileno (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream echo-stream))
    (stream-fileno (echo-stream-output-stream stream)))
  (:method ((stream broadcast-stream))
    (stream-fileno (first (broadcast-stream-streams stream))))
  #+(and CMU simple-streams)
  (:method ((stream stream:simple-stream))
    (let ((fd (stream:stream-output-handle stream)))
      (if (or (null fd) (integerp fd)) fd (stream-fileno fd)))))

(defun stream-baud-rate (stream)
  (declare (type stream stream)
	   (values (or null (integer 0 4000000))))
  #+CMU
  (alien:with-alien ((termios (alien:struct unix:termios)))
    (declare (optimize (ext:inhibit-warnings 3)))
    (when (unix:unix-tcgetattr (stream-fileno stream) termios)
      (let ((baud (logand unix:tty-cbaud
			  (alien:slot termios 'unix:c-cflag))))
	(if (< baud unix::tty-cbaudex)
	  (aref #(0 50 75 110 134 150 200 300 600 1200
		  1800 2400 4800 9600 19200 38400)
		baud)
	  (aref #(57600 115200 230400 460800 500000 576000
		  921600 1000000 1152000 1500000 2000000
		  2500000 3000000 3500000 4000000)
		(logxor baud unix::tty-cbaudex)))))))

(defun terminal-size (&optional (stream *terminal-io*))
  (declare (type stream stream))
  #+CMU
  (alien:with-alien ((winsz (alien:struct unix:winsize)))
    (declare (optimize (ext:inhibit-warnings 3)))
    (if (unix:unix-ioctl (stream-fileno stream) unix:TIOCGWINSZ winsz)
	(values (alien:slot winsz 'unix:ws-row)
		(alien:slot winsz 'unix:ws-col))
	(values nil nil))))

(defstruct padding time force line-multiplier)

(defun decode-padding (string &optional (junk-allowed nil))
  "Decode padding from string.

Return the values of
- padding time in milliseconds (could have a tenth)
- whether or not to force the padding
- whether or not to multiply by the lines affected
- end of padding #\> index into string
e.g. \"<10.5*/>\"   =>   (values (make-padding 10.5 T T) 7)

Setting junk-allowed to t will decode a padding string
that does not start with #\<.
e.g. \"<10.5/>\"   => (values (make-padding 10.5 T NIL) 7)"
  (declare (type string string))
  (unless (find #\> string)
    (error "Invalid padding specification"))
  (do ((start (if junk-allowed
                  (1+ (position #\< string))
                  1))
       (time 0)
       (pad-end)
       (force)
       (line-multiplier))
      (pad-end
       (values (make-padding :time time :force force :line-multiplier line-multiplier) pad-end))
    (let ((c (elt string start)))
      (cond
        ((and (char= c #\*)
              (not line-multiplier))
         (setf line-multiplier t
               start (1+ start)))
        ((and (char= c #\/)
              (not force))
         (setf force t
               start (1+ start)))
        ((char= c #\>)
         (setf pad-end (1+ start)))
        ((and (digit-char-p (elt string start))
              (zerop time))
         (let* ((end (position-if-not #'digit-char-p string :start start))
                (ms (parse-integer string :start start :end end)))
           (if (char= (elt string end) #\.)
               (setf time (+ ms (* 0.1 (digit-char-p
                                        (elt string (1+ end)))))
                     start (+ end 2))
               (setf time ms
                     start end))))
        (t (error "Invalid padding specification"))))))

(defun strings-and-delays (string)
  "Decompose the command string and delays into a list of
strings and delay time padding structs.

The delay time entries are lists of the delay in milliseconds,
t or nil for / which indicates a delay time that needs to be forced,
and t or nil for * which indicates a multiplier for lines affected."
  (declare (type string string))
  (do ((strings-and-delays ())
       (start 0)
       (length (length string)))
      ((>= start length) (nreverse strings-and-delays))
    (let ((found (search "$<" string :start2 start)))
      (if found
          (progn
            (when (/= found start)
              (push (subseq string start found)
                    strings-and-delays)
              (setf start found))
            (multiple-value-bind
                  (padding end)
                (decode-padding (subseq string (1+ found)))
              (push padding strings-and-delays)
              (incf start (1+ end))))
          (return (append (nreverse strings-and-delays)
                          (list (subseq string start))))))))
;; TI> (ti::strings-and-delays "A{3}$<10.5*>B{2}")
;; ("A{3}" #S(PADDING :TIME 10.5 :FORCE NIL :LINE-MULTIPLIER T) "B{2}")
;; TI> (ti::strings-and-delays "A{3}$<10.5*>B{2}$<5>c")
;; ("A{3}" #S(PADDING :TIME 10.5 :FORCE NIL :LINE-MULTIPLIER T) "B{2}"
;;  #S(PADDING :TIME 5 :FORCE NIL :LINE-MULTIPLIER NIL) "c")
;; TI> (ti::strings-and-delays "A{3}$<10.5*>B{2}$<5/>c")
;; ("A{3}" #S(PADDING :TIME 10.5 :FORCE NIL :LINE-MULTIPLIER T) "B{2}"
;;  #S(PADDING :TIME 5 :FORCE T :LINE-MULTIPLIER NIL) "c")
;; TI> (ti::strings-and-delays "A{3}")
;; ("A{3}")

(defun print-padding (padding &key
                                stream
                                baud-rate (affected-lines 1)
                                (terminfo *terminfo*))
  "Print a padding definition to the stream depending
on the capability of the terminfo data. 

If stream is nil, the padding characters or delay time
in ms will be returned.  If a stream is provided, the
padding characters will be written, or the function will
sleep for the specified time."
  (declare (type padding padding))
  ;; Decide whether to apply padding:
  (when (or (padding-force padding)
            ;; TODO: capability doesn't indicate activation...
            (not (capability :xon-xoff terminfo)))
    (when (let ((pb (capability :padding-baud-rate terminfo)))
            (and baud-rate (or (null pb) (> baud-rate pb))))
      (cond ((capability :no-pad-char terminfo)
             (if stream
                 (progn (finish-output stream)
                        (sleep (* (padding-time padding) 0.001 affected-lines)))
                 (* (padding-time padding) affected-lines)))
            (t
             (let ((tmp (capability :pad-char terminfo))
                   (null-count (ceiling (* baud-rate (padding-time padding) 1000 affected-lines) 100000)))
               (let ((pad (or (and tmp (schar tmp 0))
                              #\Null)))
                 (if stream
                     (dotimes (i null-count)
                       (princ pad stream))
                     (make-string null-count :initial-element pad)))))))))

(defmacro tputs (string &rest args)
  "Given a string and its arguments, compose the appropriate command 
for the terminfo terminal and put it into the stream, or return
a list of strings and delay times when stream is nil.
Keyword arguments are passed on to the executing function, and include:
 (terminfo *terminfo*)
 (stream *terminal-io*)
 baud-rate
 (affected-lines 1))
"
    ;; There's got to be a better way...
  (let ((args (subseq args 0 (position-if #'keywordp args)))
        (keywords (member-if #'keywordp args)))
    `(%tputs ,(if args `(tparm ,string ,@args)
                  string)
             ,@keywords)))

(defun %tputs (string &key
                        (terminfo *terminfo*)
                        (stream *terminal-io*)
                        baud-rate
                        (affected-lines 1))
  "Print the control string to an output stream.  If stream is nil,
a list of strings and delay times is returned.
String must already have been operated upon by tparm if necessary."
  (declare (type fixnum affected-lines))
  (when string
    (let ((strings-and-delays (strings-and-delays string))
          (result ()))
      (dolist (item strings-and-delays (and (not stream) (nreverse result)))
        (let ((printed
               (typecase item
                 (padding (print-padding item :baud-rate baud-rate :stream stream :terminfo terminfo :affected-lines affected-lines))
                 (string (if stream (princ item stream) item)))))
          (unless stream
            (push printed result)))))))

(defun set-terminal (&optional name)
  "Load the terminfo database specified, or defined per 
the TERM environment variable."
  (setf *terminfo* (load-terminfo (or name
				      #+ccl
				      (ccl:getenv "TERM")
				      #+CMU
				      (cdr (assoc "TERM" ext:*environment-list*
						  :test #'string=))
				      #+Allegro
				      (sys:getenv "TERM")
				      #+SBCL
				      (sb-ext:posix-getenv "TERM")
                                      #+Lispworks
                                      (lispworks:environment-variable "TERM")
				      #| if all else fails |#
				      "dumb"))))

(provide :terminfo)
