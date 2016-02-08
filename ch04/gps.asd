(defpackage :gps-system (:use :asdf :cl))
(in-package :gps-system)

(defsystem gps
  :name "gps"
  :author "dan sutton"
  :components ((:file "packages")
               (:file "gps2" :depends-on ("packages"))))
