(require 'jsonrpc)
(add-to-list 'package--builtin-versions '(jsonrpc 1 0 24))
(autoload 'dape-breakpoint-toggle "dape" "Make breakpoints." t)
(autoload 'dape "dape" "Start debuging session." t)


(provide 'init-dape)
