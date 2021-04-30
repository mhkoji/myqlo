(asdf:defsystem :myqlo-test
  :serial t
  :pathname "t"
  :components
  ((:file "test"))

  :perform (asdf:test-op (o s)
             (funcall (intern (symbol-name :run!) :fiveam)
                      :myqlo))
  :depends-on (:myqlo
               :fiveam))
