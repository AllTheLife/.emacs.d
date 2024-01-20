(require 'rime)
(require 'posframe)
(autoload 'jieba-mode "jieba.el" "Load Jieba.el.")


(setq rime-user-data-dir "~/.local/share/fcitx5/rime"
      rime-show-candidate 'posframe
      rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "WenQuanYi Micro Hei Mono"
            :internal-border-width 10)
      default-input-method "rime")


(provide 'init-rime)
