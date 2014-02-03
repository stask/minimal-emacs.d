(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)
;;(package-refresh-contents)

(defun package-install-unless-installed (p)
  "Installs package unless last version is installed."
  (unless (package-installed-p p)
    (package-install p)))
