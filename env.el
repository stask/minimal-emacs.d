(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
	 (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system
  (set-exec-path-from-shell-PATH)
  (setenv "GOPATH" (expand-file-name "~/Projects/stask/go-musings/")))

;; aliases
(defalias 'e 'find-file)
(defalias 'E 'find-file-other-window)
