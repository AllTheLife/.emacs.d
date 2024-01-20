(require 'lsp-bridge)
(require 'breadcrumb)
(autoload 'avy-goto-word-0 "avy")


(setq acm-enable-tabnine nil)
(setq acm-enable-yas t)
(setq acm-enable-citre nil)
(setq acm-enable-quick-access t)
(setq acm-enable-telega t)
(setq lsp-bridge-enable-org-babel t)
(setq lsp-bridge-c-lsp-server "ccls")
(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
(setq lsp-bridge-signature-show-with-frame-position "point")
(setq lsp-bridge-peek-ace-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
(setq lsp-bridge-diagnostic-fetch-idle 0.1)
(setq markdown-enable-highlighting-syntax t)
(setq-default markdown-fontify-code-blocks-natively t)

(global-lsp-bridge-mode)
(breadcrumb-mode t)

(defun lsp-bridge-avy-peek ()
  "Peek any symbol in the file by avy jump."
  (interactive)
  (setq lsp-bridge-peek-ace-list (make-list 5 nil))
  (setf (nth 1 lsp-bridge-peek-ace-list) (point))
  (setf (nth 2 lsp-bridge-peek-ace-list) (buffer-name))
  (save-excursion
    (call-interactively 'avy-goto-word-0)
    (lsp-bridge-peek)
    (setf (nth 3 lsp-bridge-peek-ace-list) (buffer-name))
    (setf (nth 4 lsp-bridge-peek-ace-list) (point))))


(provide 'init-lsp)
