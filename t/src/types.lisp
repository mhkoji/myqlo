(defpackage :myqlo-test.types
  (:use :cl :myqlo-test :fiveam))
(in-package :myqlo-test.types)
(in-suite :myqlo)

(test decimal
  (with-connection (conn)
    (myqlo:query conn "DROP TABLE IF EXISTS t_decimal")
    (myqlo:query conn "CREATE TABLE t_decimal (d decimal)")
    (myqlo:execute conn "INSERT INTO t_decimal (d) VALUES (?),(?)"
     (list (myqlo:make-param :sql-type :long :value 100)
           (myqlo:make-param :sql-type :long :value 200)))

    (is (equal
         (myqlo:query conn
          "SELECT * from t_decimal")
         '(("100") ("200"))))
    (is (equal
         (myqlo:execute conn
          "SELECT * from t_decimal where d = ?"
          (list (myqlo:make-param :sql-type :long :value 100)))
         '(("100"))))))

(test null
  (with-connection (conn)
    (myqlo:query conn "DROP TABLE IF EXISTS t_null")
    (myqlo:query conn "CREATE TABLE t_null (d decimal, t text)")
    (myqlo:query conn "INSERT INTO t_null (d, t) VALUES (NULL, NULL)")
    (is (equal
         (myqlo:query conn "SELECT * from t_null")
         '((nil nil))))
    (is (equal
         (myqlo:execute conn "SELECT * from t_null" nil)
         '((nil nil))))))

(test double
  (with-connection (conn)
    (myqlo:query conn "DROP TABLE IF EXISTS t_double")
    (myqlo:query conn "CREATE TABLE t_double (d double)")
    (myqlo:query conn "INSERT INTO t_double VALUES (3.1415)")
    (myqlo:execute conn "INSERT INTO t_double VALUES (?)" '("-3.1415"))
    (is (equal
         (myqlo:query conn "SELECT * from t_double")
         '(("3.1415") ("-3.1415"))))
    (is (equal
         (myqlo:execute conn "SELECT * from t_double" nil)
         '(("3.1415") ("-3.1415"))))))
