(defpackage :myqlo.cffi
  (:use :cl)
  (:export :mysql-init
           :mysql-close
           :mysql-error
           :mysql-errno
           :mysql-query
           :mysql-real-connect))
(in-package :myqlo.cffi)

(cffi:define-foreign-library libmysqlclient
  (:unix "libmysqlclient.so"))

(cffi:use-foreign-library libmysqlclient)

(cffi:defcfun ("memset" memset) :pointer
  (buf :pointer)
  (c :int)
  (n :uint64))

;; C api functions in alphabetical order.
(cffi:defcfun ("mysql_close" mysql-close) :void
  (mysql :pointer))

(cffi:defcfun ("mysql_commit" mysql-commit) :int
  (mysql :pointer))

(cffi:defcfun ("mysql_errno" mysql-errno) :unsigned-int
  (mysql :pointer))

(cffi:defcfun ("mysql_error" mysql-error) :string
  (mysql :pointer))

(cffi:defcfun ("mysql_fetch_fields" mysql-fetch-fields) :pointer
  (mysql-res :pointer))

(cffi:defcfun ("mysql_fetch_row" mysql-fetch-row) :pointer
  (mysql-res :pointer))

(cffi:defcfun ("mysql_fetch_lengths" mysql-fetch-lengths) :pointer
  (mysql-res :pointer))

(cffi:defcfun ("mysql_free_result" mysql-free-result) :void
  (mysql-res :pointer))

(cffi:defcfun ("mysql_init" mysql-init) :pointer
  (mysql :pointer))

(cffi:defcfun ("mysql_num_fields" mysql-num-fields) :unsigned-int
  (mysql-res :pointer))

(cffi:defcfun ("mysql_ping" mysql-ping) :int
  (mysql :pointer))

(cffi:defcfun ("mysql_query" mysql-query) :int
  (mysql :pointer)
  (statement :string))

(cffi:defcfun ("mysql_real_connect" mysql-real-connect) :pointer
  (mysql :pointer)
  (host :string)
  (user :string)
  (password :string)
  (database :string)
  (port :int)
  (unix-socket :string)
  (client-flag :unsigned-long))

(cffi:defcfun ("mysql_rollback" mysql-rollback) :int
  (mysql :pointer))

(cffi:defcfun ("mysql_store_result" mysql-store-result) :pointer
  (mysql :pointer))


;; C api prepared statement functions in alphabetical order.
(cffi:defcfun ("mysql_stmt_bind_param" mysql-stmt-bind-param) :int
  (stmt :pointer)
  (bind :pointer))

(cffi:defcfun ("mysql_stmt_bind_result" mysql-stmt-bind-result) :int
  (stmt :pointer)
  (bind :pointer))

(cffi:defcfun ("mysql_stmt_close" mysql-stmt-close) :boolean
  (stmt :pointer))

(cffi:defcfun ("mysql_stmt_errno" mysql-stmt-errno) :unsigned-int
  (mysql :pointer))

(cffi:defcfun ("mysql_stmt_error" mysql-stmt-error) :string
  (mysql :pointer))

(cffi:defcfun ("mysql_stmt_execute" mysql-stmt-execute) :int
  (stmt :pointer))

(cffi:defcfun ("mysql_stmt_result_metadata" mysql-stmt-result-metadata)
    :pointer
  (mysql-stmt :pointer))

(cffi:defcfun ("mysql_stmt_fetch" mysql-stmt-fetch) :int
  (stmt :pointer))

(cffi:defcfun ("mysql_stmt_fetch_column" mysql-stmt-fetch-column) :int
  (stmt :pointer)
  (bind :pointer)
  (column :unsigned-int)
  (offset :unsigned-long))

(cffi:defcfun ("mysql_stmt_free_result" mysql-stmt-free-result) :int
  (stmt :pointer))

(cffi:defcfun ("mysql_stmt_init" mysql-stmt-init) :pointer
  (mysql :pointer))

(cffi:defcfun ("mysql_stmt_next_result" mysql-stmt-next-result) :int
  (stmt :pointer))

(cffi:defcfun ("mysql_stmt_prepare" mysql-stmt-prepare) :int
  (stmt :pointer)
  (stmt-str :string)
  (length :unsigned-long))


;; https://dev.mysql.com/doc/dev/mysql-server/latest/field__types_8h.html
(cffi:defcenum enum-field-types
  :decimal :tiny :short :long :float :double :null :timestamp
  :longlong :int24 :date :time :datetime :year :newdate :varchar :bit
  (:invalid 243)
  (:bool 246)
  (:json 245)
  (:newdecimal 246)
  (:enum 247)
  (:set 248)
  (:tiny-blob 249)
  (:medium-blob 250)
  (:long-blob 251)
  (:blob 252)
  (:var-string 253)
  (:string 254)
  (:geometry 255))

(cffi:defcstruct mysql-field
  (name :string)
  (org-name :string)
  (table :string)
  (org-table :string)
  (db :string)
  (catalog :string)
  (def :string)
  (length :unsigned-long)
  (max-length :unsigned-long)
  (name-length :unsigned-int)
  (org-name-length :unsigned-int)
  (table-length :unsigned-int)
  (org-table-length :unsigned-int)
  (db-length :unsigned-int)
  (catalog-length :unsigned-int)
  (def-length :unsigned-int)
  (flags :unsigned-int)
  (decimals :unsigned-int)
  (charsetnr :unsigned-int)
  (type enum-field-types)
  (extension :pointer))

;; https://dev.mysql.com/doc/dev/mysql-server/latest/structMYSQL__BIND.html
(cffi:defcstruct mysql-bind
  (length :pointer)
  (is-null :pointer)
  (buffer :pointer)
  (error :pointer)
  (row-ptr :pointer)
  (store-param-func :pointer)
  (fetch-result-func :pointer)
  (skip-result-func :pointer)
  (buffer-length :unsigned-long)
  (offset :unsigned-long)
  (length-value :unsigned-long)
  (param-number :unsigned-int)
  (pack-length :unsigned-int)
  (buffer-type enum-field-types)
  (error-value :char)
  (is-unsigned :char)
  (long-data-used :char)
  (is-null-value :char)
  (extension :pointer))

(cffi:defcenum enum-mysql-timestamp-type
  (:none -2)
  (:error -1)
  (:date 0)
  (:datetime 1)
  (:time 2)
  (:datetime-tz 3))

;; https://dev.mysql.com/doc/dev/mysql-server/latest/structMYSQL__TIME.html
(cffi:defcstruct mysql-time
  (year :unsigned-int)
  (month :unsigned-int)
  (day :unsigned-int)
  (hour :unsigned-int)
  (minute :unsigned-int)
  (second :unsigned-int)
  (second-part :unsigned-int)
  (meg :boolean)
  (time-type enum-mysql-timestamp-type)
  (time-zone-dispacement :int))
