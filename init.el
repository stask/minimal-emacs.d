;; basic stuff

;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; disable backup files
(setq make-backup-files nil)

;; disable auto-save
(setq auto-save-default nil)

(server-start)

;; install packages
(load "~/.emacs.d/packages.el")

;; appearance stuff
(load "~/.emacs.d/appearance.el")

;; environment
(load "~/.emacs.d/environment.el")

;; general programming stuff
(load "~/.emacs.d/programming.el")

;; clojure stuff
(load "~/.emacs.d/clojure.el")
