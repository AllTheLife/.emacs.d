(require 'undo-fu-session)


(setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
(undo-fu-session-global-mode)


(provide 'init-undo)
