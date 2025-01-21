(require 'holo-layer)


(setq holo-layer-enable-cursor-animation t)
(setq holo-layer-cursor-alpha 255)
(setq holo-layer-cursor-animation-interval 10)
;; (setq holo-layer-cursor-animation-color-gradient-start-value "white")
(setq holo-layer-cursor-animation-type "jelly easing")
(setq holo-layer-enable-place-info t)
(setq holo-layer-place-info-font-size 13)
(setq holo-layer-enable-window-border t)
(setq holo-layer-active-window-color "#357ea6")
(setq holo-layer-inactive-window-color "#224254")
(setq holo-layer-window-number-color "#b262de")

;; (lazy-load-configs
;;  '((when holo-layer-enable-cursor-animation
;;      (add-hook 'post-command-hook #'holo-layer-monitor-cursor-change))

;;    (add-hook 'post-command-hook #'holo-layer-show-place-info)

;;    (add-hook 'window-size-change-functions #'holo-layer-monitor-configuration-change)
;;    (add-hook 'window-configuration-change-hook #'holo-layer-monitor-configuration-change)
;;    (add-hook 'buffer-list-update-hook #'holo-layer-monitor-configuration-change)

;;    (advice-add #'other-frame :after #'holo-layer-monitor-frame-changed)
;;    (advice-add #'maximize-frame :after #'holo-layer-monitor-frame-changed)
;;    (advice-add #'mouse-set-point :after #'holo-layer-monitor-frame-changed)
;;    (add-hook 'move-frame-functions #'holo-layer-monitor-frame-change)
;;    (add-hook 'delete-frame-functions #'holo-layer-monitor-frame-change)
;;    (add-hook 'after-make-frame-functions #'holo-layer-monitor-make-frame)
;;    (holo-layer-start-process)))
(load-configs (holo-layer-enable))


(provide 'init-holo-layer)
