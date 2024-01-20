(add-to-list 'load-path "~/.emacs.d/site-lisp/configs")

;; 让窗口启动更平滑
(setq frame-inhibit-implied-resize t)
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(require 'cl-lib)
(require 'init-utils)
(require 'init-generic)

(run-with-timer
 0.0005 nil
 (lambda ()

   (load-configs
    (add-subdirs-to-load-path (concat user-emacs-directory "site-lisp/"))
    (require 'init-theme)
    (require 'init-fonts)
    (require 'init-svg)
    (require 'init-tab)
    (require 'init-vc)
    (require 'init-meow)
    (require 'init-windows)
    (require 'init-minibuffer)
    (require 'init-auto-save)
    (require 'init-treesit)
    (require 'init-indent-bars)
    (require 'init-compile-and-run)
    (require 'init-undo)
    (require 'init-key)
    (require 'init-sessions))

   (run-with-timer
    3 nil
    (lambda ()
      (load-configs
       (require 'init-icons)
       (require 'init-dired)
       (require 'init-yas)
       (require 'init-rime)
       (require 'init-goggles)
       (require 'init-rainbow)
       ;; (require 'init-telega)
       (require 'init-lsp))))))
