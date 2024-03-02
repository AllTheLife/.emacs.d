(require 'dirvish)
(autoload 'dirvish-yank-menu "dirvish-yank" "Open dirvish yank menu." t)
(autoload 'dirvish-side "dirvish-side" "Open dirvish on one side." t)
(autoload 'holo-layer-kill-process "holo-layer")
(autoload 'holo-layer-restart-process "holo-layer")


(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group")
(setq dirvish-attributes
        '(nerd-icons file-time file-size subtree-state))
(advice-add 'dirvish :before 'holo-layer-kill-process)
(advice-add 'dirvish-quit :after 'holo-layer-restart-process)

(dirvish-override-dired-mode)


(provide 'init-dired)
