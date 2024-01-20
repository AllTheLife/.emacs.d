(require 'svg-lib)
(require 'svg-tag-mode)
(require 'face-remap)

(defun svg-lib-tag (label &optional style &rest args)
  "Create an image displaying LABEL in a rounded box using given STYLE
and style elements ARGS."

  (let* ((default svg-lib-style-default)
         (style (if style (apply #'svg-lib-style nil style) default))
         (style (if args  (apply #'svg-lib-style style args) style))

         (foreground  (plist-get style :foreground))
         (background  (plist-get style :background))

         (crop-left   (plist-get style :crop-left))
         (crop-right  (plist-get style :crop-right))

         (alignment   (plist-get style :alignment))
         (stroke      (plist-get style :stroke))
         ;; (width       (plist-get style :width))
         (height      (plist-get style :height))
         (radius      (plist-get style :radius))
         ;; (scale       (plist-get style :scale))
         (margin      (plist-get style :margin))
         (padding     (plist-get style :padding))
         (font-size   (plist-get style :font-size))
         (font-family (plist-get style :font-family))
         (font-weight (plist-get style :font-weight))

         ;; use `fixed-pitch' while in `mixed-pitch-mode'
         (txt-char-width  (window-font-width nil 'fixed-pitch))
         (txt-char-height (window-font-height nil 'fixed-pitch))
         (txt-char-height (if line-spacing
                              (+ txt-char-height line-spacing)
                            txt-char-height))
         (font-info       (font-info (format "%s-%d" font-family font-size)))
         (font-size       (aref font-info 2)) ;; redefine font-size
         (ascent          (plist-get style :ascent))
         (scale          (plist-get style :scale))
         (tag-char-width  (aref font-info 11))
         ;; (tag-char-height (aref font-info 3))
         (tag-width       (* (+ (length label) padding) txt-char-width))
         (tag-height      (* txt-char-height height))

         (svg-width       (+ tag-width (* margin txt-char-width)))
         (svg-height      tag-height)

         (tag-x  (* (- svg-width tag-width)  alignment))
         (text-x (+ tag-x (/ (- tag-width (* (length label) tag-char-width))
                             2)))
         (text-y ascent)

         (tag-x      (if crop-left  (- tag-x     txt-char-width) tag-x))
         (tag-width  (if crop-left  (+ tag-width txt-char-width) tag-width))
         (text-x     (if crop-left  (- text-x (/ stroke 2)) text-x))
         (tag-width  (if crop-right (+ tag-width txt-char-width) tag-width))
         (text-x     (if crop-right (+ text-x (/ stroke 2)) text-x))

         (svg (svg-create svg-width svg-height)))

    (if (>= stroke 0.25)
        (svg-rectangle svg tag-x 0 tag-width tag-height
                       :fill foreground :rx radius))
    (svg-rectangle svg (+ tag-x (/ stroke 2.0)) (/ stroke 2.0)
                   (- tag-width stroke) (- tag-height stroke)
                   :fill background :rx (- radius (/ stroke 2.0)))
    (svg-text svg label
              :font-family font-family :font-weight font-weight
              :font-size font-size :fill foreground :x text-x :y  text-y)
    (svg-lib--image svg :ascent 'center :scale scale)))

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}\\(?: [.+]?\\+[0-9]+[dwmy]\\)?")
(defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0) nil
                                    :margin 0 :stroke 2 :radius 3
                                    :padding 2 :width 4 :height 0.4
                                    :foreground "#B0BEC5")
              (svg-lib-tag (concat value "%") nil
                           :stroke 0 :margin 0 :foreground "#384d57"
                           :ascent 12))
             :ascent 70 :scale 0.7))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ count total) nil
                                      :margin 0 :stroke 2 :radius 3
                                      :padding 2 :width 4 :height 0.4
                                      :foreground "#B0BEC5")
                (svg-lib-tag value nil
                             :stroke 0 :margin 0 :foreground "#384d57"
                             :ascent 12))
               :ascent 70 :scale 0.7)))

(global-svg-tag-mode t)


(provide 'init-svg)
