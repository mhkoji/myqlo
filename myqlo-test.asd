(asdf:defsystem :myqlo-test
  :serial t
  :pathname "t/src"
  :components
  ((:file "test")
   (:file "query-and-execute")
   (:file "types"))

  :perform (asdf:test-op (o s)
             (funcall (intern (symbol-name :run!) :fiveam)
                      :myqlo))
  :depends-on (:myqlo
               :fiveam))
