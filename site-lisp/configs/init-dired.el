(require 'dirvish)
(require 'dirvish-extras)
(require 'dirvish-ls)
(require 'dirvish-side)

(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group")
(setq dirvish-attributes
        '(nerd-icons file-time file-size subtree-state))

(dirvish-define-preview eza (file)
  "Use `eza' to generate directory preview."
  :require ("eza") ; tell Dirvish to check if we have the executable
  (when (file-directory-p file) ; we only interest in directories here
    `(shell . ("eza" "-al" "--color=always" "--icons=always"
               "--group-directories-first" ,file))))

(push 'eza dirvish-preview-dispatchers)

(dirvish-override-dired-mode)


(provide 'init-dired)
