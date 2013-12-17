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
                 dash-at-point)
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
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

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

(global-set-key "\C-cd" 'dash-at-point)
;;

;; some random goodies
;; taken from https://gist.github.com/gnufied/7160310
(global-set-key [f2] 'comment-region)
(global-set-key [f3] 'uncommend-region)
(global-set-key [f5] 'indent-region)
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;; org-mode stuff
(require 'org-install)
(require 'ob-tangle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(clojure-defun-indents (quote (context GET POST with-db wcar cond->)))
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(dash-at-point-mode-alist (quote ((actionscript-mode . "actionscript") (arduino-mode . "arduino") (c++-mode . "cpp") (c-mode . "c") (clojure-mode . "clojure") (coffee-mode . "coffee") (common-lisp-mode . "lisp") (cperl-mode . "perl") (css-mode . "css") (elixir-mode . "elixir") (emacs-lisp-mode . "elisp") (enh-ruby-mode . "ruby") (erlang-mode . "erlang") (gfm-mode . "markdown") (go-mode . "go") (groovy-mode . "groovy") (haskell-mode . "haskell") (html-mode . "html") (java-mode . "java") (java-mode . "lucene") (js2-mode . "javascript") (js3-mode . "nodejs") (less-css-mode . "less") (lua-mode . "lua") (markdown-mode . "markdown") (objc-mode . "iphoneos") (perl-mode . "perl") (php-mode . "php") (processing-mode . "processing") (puppet-mode . "puppet") (python-mode . "python3") (ruby-mode . "ruby") (sass-mode . "sass") (scala-mode . "scala") (tcl-mode . "tcl") (vim-mode . "vim"))))
 '(global-whitespace-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (clojure . t))))
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(scroll-bar-mode nil)
 '(sh-indentation 2)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(whitespace-line-column 100)
 '(whitespace-style (quote (face trailing space-before-tab empty space-after-tab))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "Menlo")))))
