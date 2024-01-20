(require 'goggles)


(add-hook! (prog-mode-hook text-mode-hook) 'goggles-mode)
(setq-default goggles-pulse t)


(provide 'init-goggles)
