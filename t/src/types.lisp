(defpackage :myqlo-test.types
  (:use :cl :myqlo-test :fiveam))
(in-package :myqlo-test.types)
(in-suite :myqlo)

(test decimal
  (with-connection (conn)
    (myqlo:query conn "DROP TABLE IF EXISTS tbl")
    (myqlo:query conn "CREATE TABLE tbl (d decimal)")
    (myqlo:execute conn "INSERT INTO tbl (d) VALUES (?),(?)"
     (list (myqlo:make-param :sql-type :long :value 100)
           (myqlo:make-param :sql-type :long :value 200)))

    (is (equal
         (myqlo:query conn
          "SELECT * from tbl")
         '(("100") ("200"))))
    (is (equal
         (myqlo:execute conn
          "SELECT * from tbl where d = ?"
          (list (myqlo:make-param :sql-type :long :value 100)))
         '(("100"))))))

(test null
  (with-connection (conn)
    (myqlo:query conn "DROP TABLE IF EXISTS tbl")
    (myqlo:query conn "CREATE TABLE tbl (d decimal, t text)")
    (myqlo:query conn "INSERT INTO tbl (d, t) VALUES (NULL, NULL)")
    (is (equal
         (myqlo:query conn "SELECT * from tbl")
         '((nil nil))))
    (is (equal
         (myqlo:execute conn "SELECT * from tbl" nil)
         '((nil nil))))))

(test double
  (with-connection (conn)
    (myqlo:query conn "DROP TABLE IF EXISTS tbl")
    (myqlo:query conn "CREATE TABLE tbl (d double)")
    (myqlo:query conn "INSERT INTO tbl VALUES (3.1415)")
    (myqlo:execute conn "INSERT INTO tbl VALUES (?)" '("-3.1415"))
    (let ((rows (myqlo:query conn "SELECT * from tbl")))
      (is (= (first (nth 0 rows)) 3.1415d0))
      (is (= (first (nth 1 rows)) -3.1415d0)))
    (let ((rows (myqlo:execute conn "SELECT * from tbl" nil)))
      (is (= (first (nth 0 rows)) 3.1415d0))
      (is (= (first (nth 1 rows)) -3.1415d0)))))

(test timestamp
  (with-connection (conn)
    (myqlo:query conn "DROP TABLE IF EXISTS tbl")
    (myqlo:query conn "CREATE TABLE tbl (ts timestamp)")
    (myqlo:query conn "INSERT INTO tbl VALUES (\"2021-01-01 12:34:45\")")
    (is (equal
         (myqlo:query conn "SELECT * from tbl")
         '(("2021-01-01 12:34:45"))))
    (is (equal
         (myqlo:execute conn "SELECT * from tbl" nil)
         '(("2021-01-01 12:34:45"))))))
