(use-package rime
  :custom
  ((default-input-method "rime"))
  :config
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p))
  (setq rime-show-candidate 'posframe)
  :bind
  (("~" . toggle-input-method)
   ("C-c s" . toggle-input-method)))

(provide 'init-rime)
