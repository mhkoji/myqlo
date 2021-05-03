(defpackage :myqlo-test.query-and-execute
  (:use :cl :myqlo-test :fiveam))
(in-package :myqlo-test.query-and-execute)
(in-suite :myqlo)

(defun setup-table (conn)
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
      VALUES (\"A123\", \"2021-01-01 10:00:00\")")
  (myqlo:execute conn
   "INSERT INTO users (name, created_on) VALUES (?, ?)"
   (list "BB456789" "2021-01-01 12:00:00")))

(test query-and-execute
  (with-connection (conn)
    (setup-table conn)
    (is (equal
         (myqlo:query conn
          "SELECT * from users")
         '((1 "A123" "2021-01-01 10:00:00")
           (2 "BB456789" "2021-01-01 12:00:00"))))
    (is (equal
         (myqlo:execute conn "SELECT * from users WHERE user_id = ?" '(2))
         '((2 "BB456789" "2021-01-01 12:00:00"))))
    (is (equal
         (myqlo:execute conn "SELECT * from users WHERE name = ?" '("A123"))
        '((1 "A123" "2021-01-01 10:00:00"))))
    (is (equal
         (myqlo:execute conn
          "SELECT * from users WHERE created_on < ?" '("2021-01-1 11:00:00"))
         '((1 "A123" "2021-01-01 10:00:00"))))))

(defstruct user id name created-on)

(test map-fn
  (with-connection (conn)
    (setup-table conn)
    (labels ((to-user (cols)
               (make-user :id (first cols)
                          :name (second cols)
                          :created-on (third cols))))
      (let ((user (car (myqlo:query conn
                        "SELECT * from users WHERE user_id = 1"
                        :map-fn #'to-user))))
        (is (= (user-id user) 1))
        (is (string= (user-name user) "A123"))
        (is (string= (user-created-on user) "2021-01-01 10:00:00")))
      (let ((user (car (myqlo:execute conn
                        "SELECT * from users WHERE user_id = ?" '(1)
                        :map-fn #'to-user))))
        (is (= (user-id user) 1))
        (is (string= (user-name user) "A123"))
        (is (string= (user-created-on user) "2021-01-01 10:00:00"))))))
