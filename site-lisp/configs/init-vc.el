(require 'transient)
(require 'diff-hl)
(autoload 'magit "magit" "Load magit." t)


(setq diff-hl-draw-borders nil)
;; (custom-set-faces
;;  '(diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
;;  '(diff-hl-insert ((t (:inherit diff-added :background unspecified))))
;;  '(diff-hl-delete ((t (:inherit diff-removed :background unspecified)))))

(global-diff-hl-mode t)

;; Integration with magit
(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))


(provide 'init-vc)
