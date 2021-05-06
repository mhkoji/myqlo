(defpackage :myqlo-test.docker
  (:use :cl)
  (:export :main))
(in-package :myqlo-test.docker)
(ql:quickload :myqlo-test)

(setq myqlo-test:*host* "mysql")

(defun main ()
  (asdf:test-system :myqlo-test))
