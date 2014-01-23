;; prevent the cursor from blinking
(blink-cursor-mode 0)

;; don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "stask")

;; don't let Emacs hurt your ears
(setq visible-bell t)

;; fullscreen
(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "C-<f11>") 'toggle-frame-fullscreen)

;; no scrollbar
(scroll-bar-mode 0)

;; no toolbar
(tool-bar-mode 0)

;; parens
(show-paren-mode t)

;; tabs and indents
(setq tab-stop-list (number-sequence 4 200 4))
(setq tab-width 4)
(setq js-indent-level 2)
(setq sh-indentation 2)

;; uniqify stuff
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*")

;; theme
(load-theme 'solarized-dark t)

;; font
(set-frame-font "Menlo-13" nil t)
