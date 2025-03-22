;; lib-svg-tag-mode.el --- Initialize lib-svg-tag-mode configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.elemacs

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;
;;

;;; Code:

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

(defun eli/org-agenda-show-svg ()
  (let* ((case-fold-search nil)
         (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
         (keyword (car keywords)))
    (while keyword
      (save-excursion
        (while (re-search-forward (nth 0 keyword) nil t)
          (put-text-property (match-beginning 0) (match-end 0)
                             'display  (nth 3 (eval (nth 2 keyword)))) ))
      (pop keywords)
      (setq keyword (car keywords)))))

(defvar eli/svg-tag-cache nil)

(defun eli/svg-tag-with-cache (orig &rest args)
  (unless eli/svg-tag-cache
    (setq eli/svg-tag-cache (make-hash-table :test 'equal)))
  (with-memoization (gethash args eli/svg-tag-cache)
    (apply orig args)))

(defun eli/svg-tag-todo-keywords-tag (tag face)
  (let ((scale 1.0)
        (tag (propertize tag 'face face)))
    (when (org-at-heading-p)
      (let* ((level (number-to-string (org-current-level)))
             (face (intern (concat "org-level-" level)))
             (height (face-attribute face :height)))
        (setq scale (if (eq height 'unspecified) 1.0 height))))
    (when text-scale-mode
      (setq scale (* scale (expt eli/image-scale-mode-step
                                 text-scale-mode-amount))))
    (svg-tag-make tag :face face
                  :inverse t :margin 0
                  :height 1.1 :ascent 16
                  :scale scale)))

;; lib-tab-bar.el --- Initialize lib-tab-bar configurations.    -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.elemacs

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;
;;

;;; Code:


(defvar emms-playing-time-mode)
(defvar emms-playing-time-string)
(defvar emms-lyrics-display-on-modeline)
(defvar emms-lyrics-mode-line-string)

;;;; tab-bar
(defface tab-bar-hints
  '((default
     :height 0.8 :inherit tab-bar-tab-inactive))
  "Tab bar face for non-selected tab."
  :version "30.0.50"
  :group 'tab-bar-faces)

(defface tab-bar-tab-space-inactive
  '((default
     :height 0.8 :inherit tab-bar-tab-inactive))
  "Tab bar face for non-selected tab."
  :version "30.0.50"
  :group 'tab-bar-faces)

(defface tab-bar-tab-space-active
  '((default
     :height 0.8 :inherit tab-bar-tab))
  "Tab bar face for non-selected tab."
  :version "30.0.50"
  :group 'tab-bar-faces)

(defface tar-bar-icons
  '((default
     :height 1.3 :inherit mindre-keyword))
  "Tab bar face for non-selected tab."
  :version "30.0.50"
  :group 'tab-bar-faces)

(defun eli/tab-bar-icon ()
  "Show `' in tab-bar."
  (propertize ""
              'face 'tar-bar-icons))

(defun eli/tab-bar-emms ()
  (concat
   (when (and (boundp 'emms-lyrics-display-on-modeline)
              emms-lyrics-display-on-modeline
              (not (string-empty-p emms-lyrics-mode-line-string)))
     (format-mode-line emms-lyrics-mode-line-string 'mood-line-unimportant))
   (when (and (boundp 'emms-playing-time-mode)
              emms-playing-time-mode
              (not (string-empty-p emms-playing-time-string)))
     (format-mode-line emms-playing-time-string 'mood-line-unimportant))))

(defface tab-bar-svg-active
  '((t (:family "Cascadia Mono" :foreground "#a1aeb5"
                :box (:line-width (4 . 5)
                                  :color "#a1aeb5"
                                  :style flat-button))))
  "Tab bar face for selected tab.")

(defvar eli/tab-bar-svg-padding-cache nil)

(defun eli/tab-bar-svg-padding (width string)
  (unless eli/tab-bar-svg-padding-cache
    (setq eli/tab-bar-svg-padding-cache (make-hash-table :test #'equal)))
  (with-memoization (gethash (list width string)
                             eli/tab-bar-svg-padding-cache)
    (let* ((margin (plist-get svg-lib-style-default :margin))
           (txt-char-width  (window-font-width nil 'fixed-pitch))
           (tag-width (- width (* margin txt-char-width)))
           (padding (- (/ tag-width txt-char-width) (length string))))
      padding)))

(defun eli/tab-bar-tab-name-with-svg (tab i)
  (let* ((current-p (eq (car tab) 'current-tab))
         (width (/ (* (/ (frame-inner-width) 3) 2)
                   (length (funcall #'tab-bar-tabs))))
         (name (concat (if tab-bar-tab-hints (format "%d " i) "")
                       (alist-get 'name tab)
                       (or (and tab-bar-close-button-show
                                (not (eq tab-bar-close-button-show
                                         (if current-p 'non-selected 'selected)))
                                tab-bar-close-button)
                           ""))))
    (when tab-bar-auto-width-min
      (setq width (max width (if (window-system)
                                 (nth 0 tab-bar-auto-width-min)
                               (nth 1 tab-bar-auto-width-min)))))
    (when tab-bar-auto-width-max
      (setq width (min width (if (window-system)
                                 (nth 0 tab-bar-auto-width-max)
                               (nth 1 tab-bar-auto-width-max)))))
    (let ((continue t)
          (prev-width (string-pixel-width name))
          (padding (plist-get svg-lib-style-default :padding))
          curr-width)
      (cond
       ((< prev-width width)
        (setq padding (eli/tab-bar-svg-padding width name)))
       ((> prev-width width)
        (while continue
          (setf (substring name -1) "")
          (setq curr-width (string-pixel-width name))
          (if (and (> curr-width width)
                   (< curr-width prev-width))
              (setq prev-width curr-width)
            (setq continue nil)))))
      (propertize
       name
       'face 'tab-bar-tab-inactive
       'display
       (svg-tag-make
        name
        :face 'tab-bar-svg-active
        :inverse (eq (car tab) 'current-tab) :margin 0 :radius 6 :padding padding
        :height 1.3 :ascent 17 :scale 1.0)))))

(setq
 tab-bar-close-button-show nil
 tab-bar-separator "​​"
 tab-bar-tab-hints t
 tab-bar-new-tab-choice "*scratch*"
 tab-bar-select-tab-modifiers '(super)
 tab-bar-tab-name-truncated-max 15
 tab-bar-border nil
 tab-bar-auto-width nil
 tab-bar-format '(;; eli/tab-bar-icon
                  tab-bar-format-tabs
                  tab-bar-separator
                  tab-bar-format-align-right
                  eli/tab-bar-emms)
 tab-bar-tab-name-function #'tab-bar-tab-name-truncated
 tab-bar-tab-name-format-function #'eli/tab-bar-tab-name-with-svg
 tab-bar-auto-width-max '(200  15))
(tab-bar-history-mode)
(tab-bar-mode 1)

;;;; provide
(provide 'init-svg-tab)
;;; lib-svg-tag-mode.el ends here.
