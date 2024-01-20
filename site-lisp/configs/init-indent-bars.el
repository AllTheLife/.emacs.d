(require 'indent-bars)


(setq indent-bars-color '(highlight :face-bg t :blend 0.2)
      indent-bars-pattern "."
      indent-bars-width-frac 0.1
      indent-bars-pad-frac 0.1
      indent-bars-zigzag nil
      indent-bars-color-by-depth nil
      indent-bars-highlight-current-depth '(:blend 0.5)
      indent-bars-display-on-blank-lines t
      indent-bars-treesit-support t
      indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator)))

(add-hook! (prog-mode-hook org-mode-hook text-mode-hook markdown-mode-hook)
  (indent-bars-mode 1))


(provide 'init-indent-bars)
