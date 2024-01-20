(require 'combobulate)


(add-hook! (c-ts-mode
	    c++-ts-mode
	    cmake-ts-mode
	    toml-ts-mode
	    css-ts-mode
	    js-ts-mode
	    json-ts-mode
	    python-ts-mode
	    typescript-ts-mode
	    rust-ts-mode
	    elisp-mode)
  'combobulate-mode)


(provide 'init-combobulate)
