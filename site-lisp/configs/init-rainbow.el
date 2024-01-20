(require 'rainbow-identifiers)
(require 'rainbow-delimiters)


(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)


(provide 'init-rainbow)
