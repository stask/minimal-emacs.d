(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(require 'cl)
(defvar my-packages
  '(paredit rainbow-delimiters auto-complete shell-switcher magit solarized-theme
	    clojure-mode cider ac-nrepl
	    erlang elixir-mode
	    go-mode
	    yaml-mode
	    markdown-mode
	    haskell-mode)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))
