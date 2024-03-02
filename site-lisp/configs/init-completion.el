(require 'vertico)
(require 'marginalia)
(require 'orderless)
(require 'vertico-posframe)
(autoload 'consult-grep "consult" "Load consult.el and call consult-grep." t)
(autoload 'consult-buffer "consult" "Load consult.el and call consult-buffer." t)
(autoload 'consult-line "consult" "Load consult.el and call consult-line." t)
(autoload 'consult-yank-pop "consult" "Load consult.el and call consult-yank-pop." t)


(vertico-mode)				;; 使用vertico做为minibuffer中的补全框架
(marginalia-mode)			;; 使补全提示信息更详细
(vertico-posframe-mode 1)               ;; 在posframe中进行补全

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion)))
      vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8))
      vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center
      consult-preview-key 'any)


(provide 'init-completion)
