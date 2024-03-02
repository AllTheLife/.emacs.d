(require 'nerd-icons)
(require 'nerd-icons-completion)


(nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)


(provide 'init-icons)
