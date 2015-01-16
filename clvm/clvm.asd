(asdf:defsystem :clvm
  :description "Common Lisp virtual machine implemented in Common Lisp"
  :long-description ""
  :version "0.0.1"
  :author ""
  :license "BSD like"
  :depends-on (:inferior-shell :hu.dwim.serializer :osicat :cl-openstack :cl-openstack :fare-utils)
  :components ((:file packages)
	       (:file list :depends-on (packages))
	       (:file compiler :depends-on (list))
	       (:file website-reverse-engineer :depends-on (octopus))
	       (:file magnolia-repo-management :depends-on (octopus))))
