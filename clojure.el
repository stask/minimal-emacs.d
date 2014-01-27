(package-install-unless-installed 'clojure-mode)
(package-install-unless-installed 'cider)
(package-install-unless-installed 'ac-nrepl)

(setq cider-auto-select-error-buffer t)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; custom indents
(require 'clojure-mode)
(put-clojure-indent 'div 'defun)
(put-clojure-indent 'ul 'defun)
(put-clojure-indent 'li 'defun)

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

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
