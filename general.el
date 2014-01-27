(set-language-environment "UTF-8")
(setq make-backup-files nil)
(setq auto-save-default nil)
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "stask")
(setq visible-bell t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode t)
(set-frame-font "Menlo-13" nil t)
(global-hl-line-mode t)

;; revert a buffer automagically if file is changed on disk
(global-auto-revert-mode t)

(setq require-final-newline t)
(setq next-line-add-newlines nil)
(setq ring-bell-function 'ignore)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-subword-mode t)
(delete-selection-mode t)

;; use icomplete in minibuffer
(icomplete-mode t)

;; fullscreen
(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "C-<f11>") 'toggle-frame-fullscreen)

;; tabs and indents
(setq tab-stop-list (number-sequence 4 200 4))
(setq-default tab-width 4)

;; column and line numbers
(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))

(dolist (mode-hook '(text-mode-hook prog-mode-hook))
  (add-hook mode-hook
	    (lambda ()
	      (linum-mode 1))))

(fringe-mode 4)

(server-start)
