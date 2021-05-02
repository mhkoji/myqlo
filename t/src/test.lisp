(defpackage :myqlo-test
  (:use :cl)
  (:export :with-connection))
(in-package :myqlo-test)

(defun ensure-db-exists-and-use (conn)
  (myqlo:query conn "CREATE DATABASE IF NOT EXISTS myqlo_test")
  (myqlo:query conn "USE myqlo_test"))

(defmacro with-connection ((var) &body body)
  `(let ((,var (myqlo:connect "127.0.0.1" "root" nil nil 3306 nil)))
     (unwind-protect (progn
                       (ensure-db-exists-and-use ,var)
                       ,@body)
       (myqlo:disconnect ,var))))

(fiveam:def-suite :myqlo)
