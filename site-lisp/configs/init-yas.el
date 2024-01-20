(require 'yasnippet)


(run-with-idle-timer 1 nil
 (lambda ()
  (require 'yasnippet-snippets)
  (yas-global-mode)))


(provide 'init-yas)
