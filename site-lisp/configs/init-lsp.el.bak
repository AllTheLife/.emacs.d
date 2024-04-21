(require 'breadcrumb)
(require 'eglot)
(require 'eglot-booster)


(breadcrumb-mode t)
(eglot-booster-mode)

(setq eglot-autoshutdown t
      eglot-send-changes-idle-time 0.5
      eglot-extend-to-xref t)

(add-to-list 'eglot-server-programs
	     '((c++-mode c++-ts-mode) .
	       ;; ("clangd"
	       ;;  "--header-insertion=never")
	       ("ccls")
	       ))
(add-hook! (c++-mode-hook c++-ts-mode-hook) #'eglot-ensure)


(provide 'init-lsp)
