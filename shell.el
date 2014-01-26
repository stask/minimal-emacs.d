(package-install-unless-installed 'shell-switcher)

(require 'shell-switcher)
(setq shell-switcher-mode t)

;; always insert at the bottom
(setq comint-scroll-to-bottom-on-input t)
;; no duplicates in command history
(setq comint-input-ignoredups t)
;; don't run random crap
(setq comint-get-old-input (lambda () ""))
;; shell history size
(setq comint-input-ring-size 5000)
;; show all in emacs interactive output
(setenv "PAGER" "cat")
;; unicode support in shell-mode
(setenv "LANG" "en_US.UTF-8")
