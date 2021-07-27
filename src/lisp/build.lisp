(load "src/phone-encoder.lisp")

(sb-ext:save-lisp-and-die "phone-encoder"
                          :toplevel 'phone-encoder:main-with-options
                          :executable t)