# myqlo: A Common Lisp interface to MySQL

## Prerequisites

```
sudo apt install libmysqlclient-dev
```

## Usage

```common-lisp
(ql:quickload :myqlo)
;=> (:MYQLO)

(defvar conn (myqlo:connect "127.0.0.1" "root" nil  "test" 3306 nil))
;=> CONN

(myqlo:query conn "CREATE TABLE users (user_id bigint(20) NOT NULL AUTO_INCREMENT PRIMARY KEY, name varchar(64) NOT NULL, created_on datetime NOT NULL)")
;=> NIL

(myqlo:query conn "INSERT INTO users (name, created_on) VALUES (\"A\", \"2021-01-01 10:00:00\")")
;=> NIL

(myqlo:query conn "INSERT INTO users (name, created_on) VALUES (\"B\", \"2021-01-01 11:00:00\")")
;=> NIL

(myqlo:query conn "SELECT * FROM users")
;=> ((1 "A" "2021-01-01 10:00:00") (2 "B" "2021-01-01 11:00:00"))

(myqlo:disconnect conn)
; No value
```

Prepared Statements
---

You can also use prepared statements.

```common-lisp
(myqlo:execute conn "SELECT * FROM users where user_id = ?" (list (myqlo:make-param :sql-type :long :value 1)))
;=> ((1 "A" "2021-01-01 10:00:00"))

(myqlo:execute conn "SELECT * FROM users where name = ?" (list (myqlo:make-param :sql-type :string :value "A")))
;=> ((1 "A" "2021-01-01 10:00:00"))

(myqlo:execute conn "SELECT * FROM users where created_on > ?" (list (myqlo:make-param :sql-type :string :value "2021-01-01 10:30:00")))
;=> ((2 "B" "2021-01-01 11:00:00"))
```

Response Object Mapping
---


```common-lisp
(defstruct user id name created-on)

(myqlo:execute conn
 "SELECT * from users WHERE user_id = ?" '(1)
 :map-fn (lambda (cols)
           (make-user :id (first cols)
                      :name (second cols)
                      :created-on (third cols))))
;=> (#S(USER :ID 1 :NAME "A" :CREATED-ON "2021-01-01 10:00:00"))
```

## Copyright

Copyright (c) 2021 mhkoji (mhkoji2@gmail.com)

## License

Licensed under the BSD 2-Clause License.
