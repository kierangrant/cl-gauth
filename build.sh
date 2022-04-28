#!/bin/sh
sbcl --noinform --lose-on-corruption --no-userinit --no-sysinit --noprint --disable-debugger \
     --eval '(load "~/quicklisp/setup")' \
     --eval '(ql:quickload "cl-gauth")' \
     --eval "(save-lisp-and-die \"gauth\" :executable t :save-runtime-options t #+sb-core-compression :compression #+sb-core-compression t :toplevel #'cl-gauth::entrypoint)"
