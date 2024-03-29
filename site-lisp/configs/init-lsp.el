(require 'lsp-bridge)
(require 'breadcrumb)
(autoload 'avy-goto-word-0 "avy")


(setq acm-enable-tabnine nil)
(setq acm-enable-yas t)
(setq acm-enable-citre nil)
(setq acm-enable-quick-access nil)
(setq acm-enable-telega nil)
(setq lsp-bridge-enable-org-babel nil)
(setq lsp-bridge-c-lsp-server "ccls")
(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
(setq lsp-bridge-signature-show-with-frame-position "point")
(setq lsp-bridge-peek-ace-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
(setq lsp-bridge-diagnostic-fetch-idle 0.1)
(setq markdown-enable-highlighting-syntax t)
(dolist (i '(1 2 3 4 5 6 7 8 9 0))
  (add-to-list 'lsp-bridge-completion-hide-characters (number-to-string i)))
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
