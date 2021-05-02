(defpackage :myqlo-test
  (:use :cl :fiveam))
(in-package :myqlo-test)
(def-suite :myqlo)
(in-suite :myqlo)

(defmacro with-connection ((var &rest spec) &body body)
  `(let ((,var (myqlo:connect ,@spec)))
     (unwind-protect (progn ,@body)
       (myqlo:disconnect ,var))))

(defun ensure-db-exists-and-use (conn)
  (myqlo:query conn "CREATE DATABASE IF NOT EXISTS myqlo_test")
  (myqlo:query conn "USE myqlo_test"))

(test query-and-execute
  (with-connection (conn "127.0.0.1" "root" nil nil 3306 nil)
    (ensure-db-exists-and-use conn)
    (myqlo:query conn
     "DROP TABLE IF EXISTS users")
    (myqlo:query conn
     "CREATE TABLE users (
        user_id bigint(20) NOT NULL AUTO_INCREMENT PRIMARY KEY,
        name varchar(64) NOT NULL,
        created_on datetime NOT NULL
      )")
    (myqlo:query conn
     "INSERT INTO users (name, created_on)
        VALUES (\"AAA\", \"2021-01-01 10:00:00\")")
    (myqlo:execute conn
     "INSERT INTO users (name, created_on) VALUES (?, ?)"
     (list (myqlo:make-param
            :sql-type :string :value "BBB")
           (myqlo:make-param
            :sql-type :string :value "2021-01-01 12:00:00")))

    (is (equal
         (myqlo:query conn
          "SELECT * from users")
         '((1 "AAA" "2021-01-01 10:00:00")
           (2 "BBB" "2021-01-01 12:00:00"))))
    (is (equal
         (myqlo:execute conn
          "SELECT * from users WHERE user_id = ?"
          (list (myqlo:make-param
                 :sql-type :long :value 2)))
         '((2 "BBB" "2021-01-01 12:00:00"))))
    (is (equal
         (myqlo:execute conn
          "SELECT * from users WHERE name = ?"
          (list (myqlo:make-param
                 :sql-type :string :value "AAA")))
        '((1 "AAA" "2021-01-01 10:00:00"))))
    (is (equal
         (myqlo:execute conn
          "SELECT * from users WHERE created_on < ?"
          (list (myqlo:make-param
                 :sql-type :string :value "2021-01-1 11:00:00")))
         '((1 "AAA" "2021-01-01 10:00:00"))))))


(test decimal
  (with-connection (conn "127.0.0.1" "root" nil nil 3306 nil)
    (ensure-db-exists-and-use conn)
    (myqlo:query conn
     "DROP TABLE IF EXISTS t_decimal")
    (myqlo:query conn
     "CREATE TABLE t_decimal (d decimal)")
    (myqlo:query conn
     "INSERT INTO t_decimal (d) VALUES (NULL)")
    (myqlo:execute conn
     "INSERT INTO t_decimal (d) VALUES (?),(?)"
     (list (myqlo:make-param :sql-type :long :value 100)
           (myqlo:make-param :sql-type :long :value 200)))
    (is (equal
         (myqlo:query conn
          "SELECT * from t_decimal")
         '((nil) ("100") ("200"))))
    (is (equal
         (myqlo:execute conn
          "SELECT * from t_decimal where d = ?"
          (list (myqlo:make-param :sql-type :long :value 100)))
         '(("100"))))))
