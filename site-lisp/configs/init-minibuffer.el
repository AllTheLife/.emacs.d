(require 'vertico)
(require 'marginalia)
(require 'orderless)
(require 'vertico-posframe)
(autoload 'consult-grep "consult" "Load consult.el and call consult-grep." t)
(autoload 'consult-buffer "consult" "Load consult.el and call consult-buffer." t)
(autoload 'consult-line "consult" "Load consult.el and call consult-line." t)
(autoload 'consult-yank-pop "consult" "Load consult.el and call consult-yank-pop." t)


(vertico-mode)				;; 开启vertico补全框架
(marginalia-mode)			;; 使补全提示信息更详细
(vertico-posframe-mode 1)               ;; 在posframe中进行补全

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion)))
      vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8))
      vertico-posframe-poshandler 'posframe-poshandler-point-window-center
      consult-preview-key 'any
      ;; read-file-name-function #'consult-find-file-with-preview
      )

(defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred)))


(provide 'init-minibuffer)
