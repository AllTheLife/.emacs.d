(require 'transient)
(require 'diff-hl)
(require 'llama)
(autoload 'magit "magit" "Load magit." t)


(setq diff-hl-draw-borders nil)

(global-diff-hl-mode t)

(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  (defun magit-fullscreen (orig-fun &rest args)
    (window-configuration-to-register :magit-fullscreen)
    (apply orig-fun args)
    (delete-other-windows))

  (defun magit-restore-screen (&rest args)
    (jump-to-register :magit-fullscreen))

  (advice-add 'magit-status :around #'magit-fullscreen)
  (advice-add 'magit-mode-quit-window :after #'magit-restore-screen))


(provide 'init-vc)
