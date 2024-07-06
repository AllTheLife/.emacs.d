(require 'dirvish)
(autoload 'dirvish-yank-menu "dirvish-yank" "Open dirvish yank menu." t)
(autoload 'dirvish-side "dirvish-side" "Open dirvish on one side." t)


(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group")
(setq dirvish-attributes
        '(nerd-icons file-time file-size subtree-state))

(dirvish-override-dired-mode)


(provide 'init-dired)
