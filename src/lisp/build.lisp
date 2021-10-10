(load "main.lisp")

(sb-ext:save-lisp-and-die "lisp-phone-encoder"
                          :toplevel 'main-with-options
                          :executable t)
