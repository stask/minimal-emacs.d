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

;; fullscreen
(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "C-<f11>") 'toggle-frame-fullscreen)

;; tabs and indents
(setq tab-stop-list (number-sequence 4 200 4))
(setq tab-width 4)

(server-start)
