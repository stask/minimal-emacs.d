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
;; compojure
(put-clojure-indent 'context 'defun)
(put-clojure-indent 'GET 'defun)
;; om
(put-clojure-indent 'div 'defun)
(put-clojure-indent 'ul 'defun)
(put-clojure-indent 'li 'defun)
;; carmine
(put-clojure-indent 'wcar* 'defun)

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

;; cljsbuild support
;; adopted from https://github.com/kototama/cljsbuild-mode/blob/master/cljsbuild-mode.el

(require 'ansi-color)

(defun cljsbuild--insertion-filter (proc string)
  "When PROC sends STRING, apply ansi color codes and insert into buffer."
  (with-current-buffer (process-buffer proc)
	(let ((moving (= (point) (process-mark proc))))
	  (save-excursion
		(goto-char (process-mark proc))
		(insert (ansi-color-apply string))
		(set-marker (process-mark proc) (point)))
	  (when moving
		(goto-char (process-mark proc))))))

(defun cljsbuild-auto (id)
  "Run 'lein cljsbuild auto <id>' in a background buffer."
  (interactive)
  (let ((dir (locate-dominating-file default-directory "project.clj")))
	(unless dir (error "Not inside a leiningen project"))
	(with-current-buffer (get-buffer-create "*cljsbuild*")
	  (when (get-buffer-process (current-buffer))
		(error "lein cljsbuild is already running"))
	  (cd dir)
	  (buffer-disable-undo)
	  (let* ((proc (start-process "cljsbuild"
								  (current-buffer)
								  "lein" "cljsbuild" "auto" id)))
		;; colorize output
		(set-process-filter proc 'cljsbuild--insertion-filter)
		(font-lock-mode)
		(message "started cljsbuild.")))))

(defun cljsbuild-auto-dev ()
  (interactive)
  (cljsbuild-auto "dev"))
