;;;; package.lisp

(defpackage #:cl-data-gen
  (:use #:cl #:alexandria)
  (:export :defgenerator
           :gen-bool
           :gen-pos-int
           :gen-neg-int
           :gen-choose
           :gen-while
           :gen-const
           :gen-n-elems
   ))
