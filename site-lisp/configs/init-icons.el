(require 'nerd-icons)
;; (require 'nerd-icons-dired)
(require 'nerd-icons-completion)


(nerd-icons-completion-mode)
;; (add-hook 'dirvish-mode-hook #'nerd-icons-dired-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)


(provide 'init-icons)
