(defpackage :myqlo
  (:use :cl)
  (:export :connection
           :make-param
           :convert-to-param

           :connect
           :disconnect
           :query
           :execute
           :commit
           :rollback
           :ping))
(in-package :myqlo)

(defvar *mysql-time-struct*
  '(:struct myqlo.cffi::mysql-time))

(defvar *mysql-bind-struct*
  '(:struct myqlo.cffi::mysql-bind))

(defvar *mysql-field-struct*
  '(:struct myqlo.cffi::mysql-field))

(defvar *ptr-mysql-bind-struct*
  '(:pointer (:struct myqlo.cffi::mysql-bind)))

(defvar *ptr-mysql-field-struct*
  '(:pointer (:struct myqlo.cffi::mysql-field)))

(defmacro field-type (field)
  `(cffi:foreign-slot-value
    ,field *mysql-field-struct* 'myqlo.cffi::type))


(defmacro bind-buffer (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'myqlo.cffi::buffer))

(defmacro bind-buffer-type (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'myqlo.cffi::buffer-type))

(defmacro bind-buffer-length (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'myqlo.cffi::buffer-length))

(defmacro bind-length (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'myqlo.cffi::length))

(defmacro bind-is-null (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'myqlo.cffi::is-null))

(defmacro bind-error (bind)
  `(cffi:foreign-slot-value
    ,bind *mysql-bind-struct* 'myqlo.cffi::error))


(defmacro time-year (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'myqlo.cffi::year))

(defmacro time-month (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'myqlo.cffi::month))

(defmacro time-day (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'myqlo.cffi::day))

(defmacro time-hour (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'myqlo.cffi::hour))

(defmacro time-minute (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'myqlo.cffi::minute))

(defmacro time-second (time)
  `(cffi:foreign-slot-value
    ,time *mysql-time-struct* 'myqlo.cffi::second))


(defmacro bind-length-ref (bind)
  `(cffi:mem-ref (bind-length ,bind) :ulong))


(defun alloc-sql-tiny ()
  (cffi:foreign-alloc :int8))

(defmacro ref-sql-tiny (ptr)
  `(cffi:mem-ref ,ptr :int8))

(defun alloc-sql-short ()
  (cffi:foreign-alloc :int16))

(defmacro ref-sql-short (ptr)
  `(cffi:mem-ref ,ptr :int16))

(defun alloc-sql-long ()
  (cffi:foreign-alloc :int32))

(defmacro ref-sql-long (ptr)
  `(cffi:mem-ref ,ptr :int32))

(defun alloc-sql-longlong ()
  (cffi:foreign-alloc :int64))

(defmacro ref-sql-longlong (ptr)
  `(cffi:mem-ref ,ptr :int64))

(defun alloc-sql-double ()
  (cffi:foreign-alloc :double))

(defmacro ref-sql-double (ptr)
  `(cffi:mem-ref ,ptr :double))

(defun alloc-sql-time ()
  (cffi:foreign-alloc *mysql-time-struct*))

(defun ref-sql-octets (ptr len)
  (cffi:foreign-array-to-lisp ptr (list :array :uint8 len)
                              :element-type '(unsigned-byte 8)))

(defun (setf ref-sql-octets) (octets ptr len)
  (cffi:lisp-array-to-foreign octets ptr (list :array :uint8 len)))

(defun alloc-binds (count)
  (let ((ptr (cffi:foreign-alloc *mysql-bind-struct* :count count)))
    ;; Binds are filled with zeros because:
    ;; - garbage data make MySQL APIs work incorrectly
    ;; - bind-release may release unallocated memories
    (myqlo.cffi::memset
     ptr 0 (* (cffi:foreign-type-size *mysql-bind-struct*) count))
    ptr))

(defun string-to-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun octets-to-string (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(defun string-to-double (string)
  (parse-float:parse-float string :type 'double-float))

(defun time->sql-string (time)
  (format nil "~2,'0d:~2,'0d:~2,'0d"
          (time-hour time)
          (time-minute time)
          (time-second time)))

(defun date->sql-string (time)
  (format nil "~2,'0d-~2,'0d-~2,'0d"
          (time-year time)
          (time-month time)
          (time-day time)))

(defun datetime->sql-string (time)
  (concatenate 'string
               (date->sql-string time)
               " "
               (time->sql-string time)))


(defun column-converter-for (sql-type)
  (ecase sql-type
    ((:null)
     (lambda (octets)
       (assert (null octets))
       nil))
    ((:tiny :short :long :longlong)
     (lambda (octets)
       (and octets (parse-integer (octets-to-string octets)))))
    ((:float :double)
     (lambda (octets)
       (and octets (string-to-double (octets-to-string octets)))))
    ((:string :var-string
      :newdecimal
      :datetime :time :date :timestamp)
     (lambda (octets)
       (and octets (octets-to-string octets))))
    ((:blob)
     #'identity)))

(defun parse-column-octets (sql-type octets)
  (funcall (column-converter-for sql-type) octets))


(define-condition mysql-error (error)
  ((error :initarg :error :reader mysql-error-error)
   (errno :initarg :errno :reader mysql-error-errno))
  (:report (lambda (condition stream)
             (format stream "MySQL error (~D): \"~A\""
                     (mysql-error-errno condition)
                     (mysql-error-error condition)))))

(defun mysql-error (mysql)
  (error 'mysql-error
         :error (myqlo.cffi:mysql-error mysql)
         :errno (myqlo.cffi:mysql-errno mysql)))

(defun stmt-error (stmt)
  (error 'mysql-error
         :error (myqlo.cffi::mysql-stmt-error stmt)
         :errno (myqlo.cffi::mysql-stmt-errno stmt)))

(defun maybe-mysql-error (mysql ret)
  (if (= ret 0)
      ret
      (mysql-error mysql)))

(defun maybe-stmt-error (stmt ret)
  (if (= ret 0)
      ret
      (stmt-error stmt)))


(defclass connection ()
  ((mysql
    :initarg :mysql
    :documentation "holds a pointer to an instance of MYSQL"
    :reader connection-mysql)))

(defstruct param sql-type value)

(defun connect (hostname username password database port flags)
  (let ((mysql (myqlo.cffi:mysql-real-connect
                (myqlo.cffi:mysql-init (cffi:null-pointer))
                (or hostname "127.0.0.1")
                (or username (cffi:null-pointer))
                (or password (cffi:null-pointer))
                (or database (cffi:null-pointer))
                (or port 0)
                (cffi:null-pointer)
                (or flags 0))))
    (when (cffi:null-pointer-p mysql)
      (error "Failed to connect"))
    (make-instance 'connection :mysql mysql)))

(defun disconnect (conn)
  (myqlo.cffi:mysql-close (connection-mysql conn)))

(defun commit (conn)
  (let ((mysql (connection-mysql conn)))
    (maybe-mysql-error mysql (myqlo.cffi::mysql-commit mysql))))

(defun rollback (conn)
  (let ((mysql (connection-mysql conn)))
    (maybe-mysql-error mysql (myqlo.cffi::mysql-rollback mysql))))

(defun ping (conn)
  (let ((mysql (connection-mysql conn)))
    (maybe-mysql-error mysql (myqlo.cffi::mysql-ping mysql))))


(defun call-with-store-result (mysql res-fn)
  (let ((res (myqlo.cffi::mysql-store-result mysql)))
    (cond ((not (cffi:null-pointer-p res))
           (unwind-protect (funcall res-fn res)
             (myqlo.cffi::mysql-free-result res)))
          ((/= (myqlo.cffi:mysql-errno mysql) 0)
           (mysql-error mysql)))))

(defmacro with-store-result ((res mysql) &body body)
  `(call-with-store-result ,mysql (lambda (,res) (progn ,@body))))

;; on-each-row-fn is a callback function invoked when a new row is fetched.
;; on-each-row-fn, for example, maps an input to another object and collects it to somewhere, which is not a concern of query-fetch-all.
(defun query-fetch-all (mysql on-each-row-fn)
  ;; https://dev.mysql.com/doc/c-api/8.0/en/mysql-store-result.html
  ;; > it does not do any harm or cause any notable performance degradation if you call mysql_store_result() in all cases.
  (with-store-result (res mysql)
    (let* ((fields (myqlo.cffi::mysql-fetch-fields res))
           (field-types
            (loop repeat (myqlo.cffi::mysql-num-fields res)
                  for i from 0
                  for field = (cffi:mem-aptr fields *mysql-field-struct* i)
                  collect (field-type field))))
      (labels ((parse-row (row lens)
                 (loop
                   for i from 0
                   for nth-ptr = (cffi:mem-aref row :pointer i)
                   for len = (cffi:mem-aref lens :unsigned-long i)
                   for type in field-types
                   collect (let ((octets (ref-sql-octets nth-ptr len)))
                             (parse-column-octets type octets)))))
        (loop
          for row = (myqlo.cffi::mysql-fetch-row res)
          if (not (cffi:null-pointer-p row))
            do (let ((lens (myqlo.cffi::mysql-fetch-lengths res)))
                 (when (cffi:null-pointer-p lens)
                   (mysql-error mysql))
                 (funcall on-each-row-fn (parse-row row lens)))
          else if (/= (myqlo.cffi:mysql-errno mysql) 0)
            do (mysql-error mysql)
          else
            do (return))))))

(defun query (conn string &key (map-fn #'identity))
  (let ((mysql (connection-mysql conn)))
    (maybe-mysql-error mysql (myqlo.cffi:mysql-query mysql string))
    (let ((rows nil))
      (query-fetch-all mysql
        (lambda (row)
          (push (funcall map-fn row) rows)))
      (nreverse rows))))

;; https://dev.mysql.com/doc/c-api/8.0/en/mysql-bind-param.html
(defun setup-bind-for-param (bind param)
  (with-accessors ((value param-value)
                   (sql-type param-sql-type)) param
    ;; It is not required to support for all the types
    ;; because this is a part of the API of this software.
    (ecase sql-type
      ((:long)
       (assert (integerp value))
       (setf (bind-buffer bind) (alloc-sql-long))
       (setf (ref-sql-long (bind-buffer bind)) value))
      ((:double)
       (assert (typep value 'double-float))
       (setf (bind-buffer bind) (alloc-sql-double))
       (setf (ref-sql-double (bind-buffer bind)) value))
      ((:string)
       (assert (stringp value))
       ;; https://dev.mysql.com/doc/c-api/8.0/en/mysql-stmt-fetch.html
       (let ((octets (string-to-octets value)))
         (let ((len (length octets)))
           (setf (bind-buffer bind) (cffi:foreign-alloc :char :count len))
           (setf (bind-length bind) (cffi:foreign-alloc :ulong))
           (setf (bind-length-ref bind) len)
           (setf (bind-buffer-length bind) len)
           (setf (ref-sql-octets (bind-buffer bind) len) octets)))))
    ;; MEMO:
    ;; If is-null is allocated in bind-for-param, exeute returns nothing.
    (setf (bind-buffer-type bind) sql-type)))

(defun bind-release (bind)
  (labels ((free-if-not-null (ptr)
             (when (not (cffi:null-pointer-p ptr))
               (cffi:foreign-free ptr))))
    (ignore-errors
      (free-if-not-null (bind-buffer bind)))
    (ignore-errors
      (free-if-not-null (bind-length bind)))
    (ignore-errors
      (free-if-not-null (bind-is-null bind)))))

(defun call-with-stmt-result-metadata (stmt res-fn)
  (let ((res (myqlo.cffi::mysql-stmt-result-metadata stmt)))
    (if (cffi:null-pointer-p res)
        ;; res is null when the query is INSERT for example.
        ;; Error check is not needed, maybe.
        nil
        (unwind-protect (funcall res-fn res)
          (myqlo.cffi::mysql-free-result res)))))

(defmacro with-stmt-result-metadata ((res stmt) &body body)
  `(call-with-stmt-result-metadata ,stmt
                                   (lambda (,res) (progn ,@body))))

(defmacro with-binds ((var &key (count 1)) &body body)
  (let ((g (gensym)))
    `(let ((,g ,count))
       (let ((,var (alloc-binds ,g)))
         (unwind-protect (progn ,@body)
           (dotimes (i ,g)
             (bind-release (cffi:mem-aptr binds *mysql-bind-struct* i)))
           (cffi:foreign-free binds))))))

(defun statement-fetch-all (stmt field-types on-each-row-fn)
  (let ((num-fields (length field-types)))
    ;; setup-bind and parse-bind are factored out together because how the buffer meomory is processed is tightly coupled in the both procedures.
    (labels
        ((setup-bind (bind sql-type)
           (ecase sql-type
             ((:null)) ;; Doing nothing seems to work when null.
             ((:tiny)
              (setf (bind-buffer bind) (alloc-sql-tiny)))
             ((:short)
              (setf (bind-buffer bind) (alloc-sql-short)))
             ((:long)
              (setf (bind-buffer bind) (alloc-sql-long)))
             ((:longlong)
              (setf (bind-buffer bind) (alloc-sql-longlong)))
             ((:float :double)
              (setf (bind-buffer bind) (alloc-sql-double)))
             ((:newdecimal :string :var-string
               :blob)
              ;; https://dev.mysql.com/doc/c-api/8.0/en/mysql-stmt-fetch.html
              ;; > Invoke mysql_stmt_fetch() with a zero-length buffer for the column in question and a pointer in which the real length can be stored.
              ;; > Then use the real length with mysql_stmt_fetch_column().
              ;;
              ;; MEMO:
              ;; Should the followings be set every time parse-bind is called,
              ;; or is it enough to set these once before calling mysql-stmt-bind-result?
              (setf (bind-buffer-length bind) 0
                    (bind-length bind) (cffi:foreign-alloc :ulong)))
             ((:date :time :datetime :timestamp)
              ;; https://dev.mysql.com/doc/refman/5.6/ja/c-api-prepared-statement-date-handling.html
              (setf (bind-buffer bind) (alloc-sql-time))))
           (setf (bind-buffer-type bind) sql-type)
           (setf (bind-is-null bind) (cffi:foreign-alloc :bool)))

         (parse-bind (bind index)
           (let ((sql-type (bind-buffer-type bind)))
             (cond ((eql sql-type :null)
                    nil)
                   ((and (not (cffi:null-pointer-p (bind-is-null bind)))
                         (cffi:mem-ref (bind-is-null bind) :bool))
                    nil)
                   (t
                    (ecase sql-type
                      ((:tiny)
                       (ref-sql-tiny (bind-buffer bind)))
                      ((:short)
                       (ref-sql-short (bind-buffer bind)))
                      ((:long)
                       (ref-sql-long (bind-buffer bind)))
                      ((:longlong)
                       (ref-sql-longlong (bind-buffer bind)))
                      ((:float :double)
                       (ref-sql-double (bind-buffer bind)))
                      ((:newdecimal :string :var-string)
                       (octets-to-string
                        (fetch-octets-using-real-length bind index)))
                      ((:blob)
                       (fetch-octets-using-real-length bind index))
                      ((:date)
                       (date->sql-string (bind-buffer bind)))
                      ((:time)
                       (time->sql-string (bind-buffer bind)))
                      ((:datetime :timestamp)
                       (datetime->sql-string (bind-buffer bind))))))))

         (fetch-octets-using-real-length (bind index)
           (let* ((len (bind-length-ref bind))
                  (buf (cffi:foreign-alloc :char :count len)))
             (unwind-protect
                  (progn
                    (setf (bind-buffer bind) buf
                          (bind-buffer-length bind) len)
                    (maybe-stmt-error
                     stmt (myqlo.cffi::mysql-stmt-fetch-column
                           stmt bind index 0))
                    (ref-sql-octets (bind-buffer bind) len))
               (cffi:foreign-free buf)
               ;; Prevent from double free by bind-release.
               (setf (bind-buffer bind) (cffi:null-pointer))))))

      (with-binds (binds :count num-fields)
        ;; Bind result
        ;; Fetch fields to set field types to binds
        (loop for i from 0
              for sql-type in field-types
              for bind = (cffi:mem-aptr binds *mysql-bind-struct* i)
              do (setup-bind bind sql-type))
        (maybe-stmt-error
         stmt (myqlo.cffi::mysql-stmt-bind-result stmt binds))
        ;; Fetch rows
        (loop for ret = (myqlo.cffi::mysql-stmt-fetch stmt)
              if (= ret 1)
                do (stmt-error stmt)
              else if (= ret 100)
                do (return)
              else ;; MYSQL_DATA_TRUNCATED was returned, maybe.
                do (funcall on-each-row-fn
                            (loop
                              repeat num-fields
                              for i from 0
                              for bind = (cffi:mem-aptr
                                          binds *mysql-bind-struct* i)
                              collect (parse-bind bind i))))))))

(defun call-with-prepared-statement (conn query stmt-fn)
  (let ((mysql-stmt (myqlo.cffi::mysql-stmt-init (connection-mysql conn))))
    (when (cffi:null-pointer-p mysql-stmt)
      (error "Failed to initialize statement"))
    (unwind-protect
         ;; Prepare
         (progn
           (let ((len (length (string-to-octets query))))
             (maybe-stmt-error
              mysql-stmt
              (myqlo.cffi::mysql-stmt-prepare mysql-stmt query len)))
           (funcall stmt-fn mysql-stmt))
      (myqlo.cffi::mysql-stmt-close mysql-stmt))))

(defmacro with-prepared-statement ((stmt conn query) &body body)
  `(call-with-prepared-statement
    ,conn ,query (lambda (,stmt) (progn ,@body))))


;; This function creates the boundary between the application input family and the sql param family.
;; The application input family consists of the objects the application is easy to use.
;; Those objects are likely to change and not always useful for the sql procedures, which is the reason why the boundary is required.
(defgeneric convert-to-param (param))

(defun execute (conn query params &key (map-fn #'identity))
  (with-prepared-statement (stmt conn query)
    (with-binds (binds :count (length params))
      ;; Bind
      (loop for i from 0
            for param in (mapcar #'convert-to-param params)
            for bind = (cffi:mem-aptr binds *mysql-bind-struct* i)
            do (setup-bind-for-param bind param))
      (maybe-stmt-error stmt (myqlo.cffi::mysql-stmt-bind-param stmt binds))
      ;; Execute
      ;; binds are released after execution because execute seems to use the values in the bindings:
      ;; https://dev.mysql.com/doc/c-api/8.0/en/c-api-prepared-statement-data-structures.html
      ;; > When you call mysql_stmt_execute(), MySQL use the value stored in the variable
      ;; > in place of the corresponding parameter marker in the statement
      (maybe-stmt-error stmt (myqlo.cffi::mysql-stmt-execute stmt))
      ;; Fetch
      (with-stmt-result-metadata (res stmt)
        (let ((fields (myqlo.cffi::mysql-fetch-fields res))
              (num-fields (myqlo.cffi::mysql-num-fields res)))
          (let ((field-types
                  (loop repeat num-fields
                        for i from 0
                        for field = (cffi:mem-aptr
                                     fields *mysql-field-struct* i)
                        collect (field-type field))))
            (let ((rows nil))
              (statement-fetch-all stmt field-types
                (lambda (row)
                  (push (funcall map-fn row) rows)))
              (nreverse rows))))))))

(defmethod convert-to-param ((param param))
  param)

(defmethod convert-to-param ((x integer))
  (make-param :value x :sql-type :long))

(defmethod convert-to-param ((x string))
  (make-param :value x :sql-type :string))

(defmethod convert-to-param ((x double-float))
  (make-param :value x :sql-type :double))
