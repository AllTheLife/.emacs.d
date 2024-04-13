(require 'nerd-icons)
(require 'nerd-icons-corfu)
(require 'nerd-icons-completion)


(nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)


(provide 'init-icons)
