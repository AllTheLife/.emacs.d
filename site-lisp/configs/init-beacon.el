(require 'beacon)

;; disable beacon for horizontal movement
(setq beacon-blink-when-point-moves-vertically 1)
;; remove specific symbols from the variable 'beacon-dont-blink-commands
(setq beacon-blink-when-point-moves-horizontally 1)
(setq beacon-dont-blink-commands '(forward-char backward-char meow-right meow-left))

(beacon-mode 1)
