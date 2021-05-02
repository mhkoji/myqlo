(defpackage :myqlo-test.decimal
  (:use :cl :myqlo-test :fiveam))
(in-package :myqlo-test.decimal)
(in-suite :myqlo)

(defun setup-table (conn)
  (myqlo:query conn   "DROP TABLE IF EXISTS t_decimal")
  (myqlo:query conn   "CREATE TABLE t_decimal (d decimal)")
  (myqlo:query conn   "INSERT INTO t_decimal (d) VALUES (NULL)")
  (myqlo:execute conn "INSERT INTO t_decimal (d) VALUES (?),(?)"
   (list (myqlo:make-param :sql-type :long :value 100)
         (myqlo:make-param :sql-type :long :value 200))))

(test decimal
  (with-connection (conn)
    (setup-table conn)
    (is (equal
         (myqlo:query conn
          "SELECT * from t_decimal")
         '((nil) ("100") ("200"))))
    (is (equal
         (myqlo:execute conn
          "SELECT * from t_decimal where d = ?"
          (list (myqlo:make-param :sql-type :long :value 100)))
         '(("100"))))))
