(asdf:defsystem marguerite
  :version "0.0.1"
  :author "Laurent Cimon <laurent@nilio.ca>"
  :maintainer "Laurent Cimon <laurent@nilio.ca>"
  :license "bsd-2-clause"
  :description "Raven's web page"
  :components ((:file "server"))
  :depends-on (#:hunchentoot #:cl-who #:sqlite))
