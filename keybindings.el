;; adopted from https://github.com/xiaohanyu/oh-my-emacs/blob/master/core/ome-keybindings.org

;; align your code in a pretty way
(global-set-key (kbd "C-x \\") 'align-regexp)

;; zoom in/out
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; file finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)

;; indentation help
(global-set-key (kbd "C-x ^") 'join-line)
