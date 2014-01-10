;; frame setup
(setq initial-frame-alist '((top . 0) (left . 0) (width . 100) (height . 52)))

(setq default-directory "~/")

;; package setup stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(require 'cl)
(defvar my-packages
  '(clojure-mode magit cider paredit rainbow-delimiters solarized-theme elixir-mode
                 yaml-mode markdown-mode gist haskell-mode erlang
                 dash-at-point spacegray-theme auto-complete ac-nrepl
                 shell-switcher)
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

;;
(server-start)

;; fullscreen stuff
(setq ns-use-native-fullscreen nil)

;; PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

(setenv "EDITOR" "emacsclient")

;; config
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-tab-command 'indent-for-tab-command)
(setq cider-auto-select-error-buffer t)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; ac-nrepl stuff
(require 'auto-complete-config)
(ac-config-default)

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; custom extension->mode mapping
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.edn$" . clojure-mode))

(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

(require 'elixir-mode) ;; have no idea why it's not required
(add-to-list 'auto-mode-alist '("\.exs$" . elixir-mode ))

(add-to-list 'auto-mode-alist '("\.conf\.tmpl$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\.cfg\.tmpl$" . js-mode))
(add-to-list 'auto-mode-alist '("\.edn\.tmpl$" . clojure-mode))

(require 'dash-at-point)
(add-to-list 'dash-at-point-mode-alist '(clojure-mode . "java-all"))
(global-set-key "\C-cd" 'dash-at-point)
;;

;; theme settings
(global-hl-line-mode 1)

;; some random goodies
;; taken from https://gist.github.com/gnufied/7160310
(global-set-key [f2] 'comment-region)
(global-set-key [f3] 'uncommend-region)
(global-set-key [f5] 'indent-region)
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

(global-set-key "\C-xg" 'magit-status)

(global-set-key [f9] 'hs-toggle-hiding)

;; org-mode stuff
(require 'org-install)
(require 'ob-tangle)

;; clojure helm stuff
(defun helm-clojure-headlines ()
  "Display headlines for the current Clojure file."
  (interactive)
  (helm-mode t)
  (helm :sources '(((name . "Clojure Headlines")
                    (volatile)
                    (headline "^[;(]")))))

;; cider-reload
(require 'clojure-mode)
(defun cider-namespace-refresh ()
  "Uses Stuart Sierra's clojure.tools.namespace to reload all changed namespaces."
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(defun cider-project-reset ()
  "Uses Stuart Sierra's workflow to reload and restart Clojure application."
  (interactive)
  (cider-interactive-eval "(user/reset)"))

(defun reindent-whole-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min)
                 (point-max)))

;; random goodies
(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file file-name new-name t)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(blink-cursor-mode nil)
 '(clojure-defun-indents (quote (context GET PUT POST DELETE with-db wcar aset cond-> component/using section ul li build-all footer span transact! read div header p)))
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(global-whitespace-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(make-backup-files nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (clojure . t))))
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(scroll-bar-mode nil)
 '(sh-indentation 2)
 '(shell-switcher-mode t)
 '(show-paren-mode t)
 '(syslog-debug-face (quote ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face (quote ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face (quote ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face (quote ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(whitespace-line-column 100)
 '(whitespace-style (quote (face trailing space-before-tab empty space-after-tab))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "Menlo")))))
