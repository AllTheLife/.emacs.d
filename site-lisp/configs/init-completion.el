(require 'vertico)
(require 'marginalia)
(require 'orderless)
(require 'vertico-posframe)
(require 'corfu)
(require 'corfu-history)
(autoload 'consult-grep "consult" "Load consult.el and call consult-grep." t)
(autoload 'consult-buffer "consult" "Load consult.el and call consult-buffer." t)
(autoload 'consult-line "consult" "Load consult.el and call consult-line." t)
(autoload 'consult-yank-pop "consult" "Load consult.el and call consult-yank-pop." t)


(vertico-mode)				;; 使用vertico做为minibuffer中的补全框架
(marginalia-mode)			;; 使补全提示信息更详细
(vertico-posframe-mode 1)               ;; 在posframe中进行补全
(global-corfu-mode)                     ;; 全局启用 corfu
(corfu-history-mode)                    ;; 记忆corfu的选择历史

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion)))
      vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8))
      vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center
      consult-preview-key 'any
      corfu-auto t
      corfu-cycle nil
      corfu-auto-delay 0
      corfu-quit-at-boundary t
      corfu-preview-current nil)

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)


(provide 'init-completion)
