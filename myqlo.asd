(asdf:defsystem :myqlo
  :serial t
  :pathname "src"

  :components
  ((:file "cffi")
   (:file "myqlo"))

  :depends-on
  (:babel
   :cffi
   :parse-float))
