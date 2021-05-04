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

### Prepared Statements


You can also use prepared statements.

```common-lisp
(myqlo:execute conn "SELECT * FROM users where user_id = ?" (list 1))
;=> ((1 "A" "2021-01-01 10:00:00"))

(myqlo:execute conn "SELECT * FROM users where name = ?" (list "A"))
;=> ((1 "A" "2021-01-01 10:00:00"))

(myqlo:execute conn "SELECT * FROM users where created_on > ?" (list "2021-01-01 10:30:00"))
;=> ((2 "B" "2021-01-01 11:00:00"))
```

### Response Object Mapping

Specify `:map-fn` to convert each row object.


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


### Parameter Conversion

Defining `convert-to-param` enables you to use your own object as a sql parameter.

Below is an example that `uuid` is used as the identifer of a user object.

```common-lisp
(defmethod myqlo:convert-to-param ((id uuid:uuid))
  (myqlo:make-param :sql-type :string :value (format nil "~A" id)))

(defstruct user id name)

(defun create-user (name)
  (make-user :id (uuid:make-v4-uuid) :name name))

(defvar *u* (create-user "john"))

*u*
;=> #S(USER :ID 4D1986C1-0B9A-4B73-BA99-26C7B6C37AF7 :NAME "john")

(myqlo:query conn "CREATE TABLE users (user_id binary(16) NOT NULL, name varchar(64) NOT NULL)")

(myqlo:execute conn "INSERT INTO users (user_id, name) VALUES (UUID_TO_BIN(?),?)"
 (list (user-id *u*) ;; type =  uuid
       (user-name *u*)))

(myqlo:execute conn "SELECT BIN_TO_UUID(user_id), name from users where user_id = UUID_TO_BIN(?)" (list (user-id *u*))
 :map-fn (lambda (cols)
           (make-user :id (uuid:make-uuid-from-string (first cols))
                      :name (second cols))))
;=> (#S(USER :ID 4D1986C1-0B9A-4B73-BA99-26C7B6C37AF7 :NAME "john"))
```

## Copyright

Copyright (c) 2021 mhkoji (mhkoji2@gmail.com)

## License

Licensed under the BSD 2-Clause License.
