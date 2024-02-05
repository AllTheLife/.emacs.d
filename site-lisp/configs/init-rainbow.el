(require 'rainbow-identifiers)
(require 'rainbow-delimiters)
(require 'rainbow-mode)


(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)


(provide 'init-rainbow)
