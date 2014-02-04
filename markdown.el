(package-install-unless-installed 'markdown-mode)

(require 'markdown-mode)
(setq markdown-command "multimarkdown")

(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))
