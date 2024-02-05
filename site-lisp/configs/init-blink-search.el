(require 'blink-search)


(setq blink-search-common-directory
      '(("HOME" "~/")
	("CONFIGS" "~/.emacs.d/site-lisp/configs")
	("CONFIGROOT" "~/.emacs.d/")
	("REPO" "~/.emacs.d/site-lisp/extensions/")
	("WORK" "~/C/Luogu/")))

;; (setq blink-search-enable-posframe)

(advice-add 'blink-search :after 'meow-insert-mode)


(provide 'init-blink-search)
