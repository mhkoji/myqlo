# myqlo: A Common Lisp interface to MySQL

## Prerequisites

```
sudo apt install libmysqlclient-dev
```

## Usage

```
CL-USER> (ql:quickload :myqlo)
(:MYQLO)

CL-USER> (defvar conn (myqlo:connect "127.0.0.1" "root" nil  "test" 3306 nil))
CONN

CL-USER> (myqlo:query conn "CREATE TABLE users (user_id bigint(20) NOT NULL AUTO_INCREMENT PRIMARY KEY, name varchar(64) NOT NULL, created_on datetime NOT NULL)")
NIL

CL-USER> (myqlo:query conn "INSERT INTO users (name, created_on) VALUES (\"B\", \"2021-01-01 11:00:00\")")
NIL

CL-USER> (myqlo:query conn "SELECT * FROM users")
((1 "A" "2021-01-01 10:00:00") (2 "B" "2021-01-01 11:00:00"))

CL-USER> (myqlo:disconnect conn)
; No value
```

###  Prepared Statements

You can also execute prepared statements.

```
CL-USER> (myqlo:execute conn "SELECT * FROM users where user_id = ?" (list (myqlo:make-param :sql-type :long :value 1)))
((1 "A" "2021-01-01 10:00:00"))

CL-USER> (myqlo:execute conn "SELECT * FROM users where name = ?" (list (myqlo:make-param :sql-type :string :value "A")))
((1 "A" "2021-01-01 10:00:00"))

CL-USER> (myqlo:execute conn "SELECT * FROM users where created_on > ?" (list (myqlo:make-param :sql-type :string :value "2021-01-01 10:30:00")))
((2 "B" "2021-01-01 11:00:00"))
```

## Copyright

Copyright (c) 2021 mhkoji (mhkoji2@gmail.com)

## License

Licensed under the BSD 2-Clause License.
