(require 'blink-search)


(setq blink-search-common-directory
      '(("HOME" "~/")
	("CONFIG" "~/.emacs.d/site-lisp/config")
	("CONFIGROOT" "~/.emacs.d/")
	("REPO" "~/.emacs.d/site-lisp/extensions/")
	("WORK" "~/C/")))

;; (setq blink-search-enable-posframe)

(advice-add 'blink-search :after 'meow-insert-mode)


(provide 'init-blink-search)
