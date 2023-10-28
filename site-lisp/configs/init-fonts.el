(require 'init-utils)


(cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
         when (font-installed-p font)
         return (set-fontset-font t 'unicode font nil 'prepend))

(set-fontset-font t 'unicode (font-spec :family "WenQuanYi Micro Hei Mono" :size 13))


(provide 'init-fonts)
