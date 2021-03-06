(global-set-key [f2] 'comment-region)
(global-set-key [f3] 'uncomment-region)
(global-set-key [f5] 'indent-region)
(global-set-key (kbd "C-s-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<up>") 'shrink-window)
(global-set-key (kbd "C-s-<down>") 'enlarge-window)

(global-set-key [f9] 'hs-toggle-hiding)

(defun reindent-whole-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min)
		 (point-max)))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc (lambda (buffer)
	  (kill-buffer buffer))
	(buffer-list))
  (delete-other-windows))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and the file it is visiting."
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
	       (message "File '%s' successfully renamed to '%s'" name
			(file-name-nondirectory new-name))))))))
