(in-package :asdf)

(defsystem :wind-loads
  :name "wind-loads"
  :author "Vdovik Yuriy <babah.yuriy06@gmail.com>"
  :version "1"
  :maintainer "Vdovik Yuriy <babah.yuriy06@gmail.com>"
  :licence "MIT License"
  :description "Some function for engineering work"
  :components ((:file "package")
			(:file "interpol")
			(:file "short-utils")
	       (:file "wind-loads")
	       (:file "esta-ob-1")
		   (:file "plosk-opora")
		   (:file "plates")
		   )
  :serial t)
  
