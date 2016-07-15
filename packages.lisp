(defpackage "TERMINFO"
  (:use "CL")
  (:nicknames "TI")
  (:export #:*terminfo-directories*
           #:*terminfo*
           #:capability
           #:tparm
           #:tputs
           #:decode-padding
           #:set-terminal
           #:capabilities))
