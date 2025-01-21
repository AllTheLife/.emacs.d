(require 'tab-bar)


(defface tab-bar-svg-active
  '((t (:foreground "#5b7980")))
  "Tab bar face for selected tab.")

(defface tab-bar-svg-inactive
  '((t (:foreground "#6f7273")))
  "Tab bar face for inactive tabs.")

(defface tab-bar-meow-normal-state
  '((t (:foreground "#548aba")))
  "Tab bar face for meow normal state.")

(defface tab-bar-meow-insert-state
  '((t (:foreground "#b57a43")))
  "Tab bar face for meow insert state.")

(defface tab-bar-meow-keypad-state
  '((t (:foreground "#96486e")))
  "Tab bar face for meow keypad state.")

(defface tab-bar-meow-motion-state
  '((t (:foreground "#48966e")))
  "Tab bar face for meow motion state.")

(defun +tab-bar-left ()
  (let* ((l (number-to-string (+ 1 (current-line))))
	 (c (number-to-string (+ 1 (current-column))))
	 (w (string-width (concat "   L" l " C" c)))
	 (mode (if (meow-normal-mode-p)
		   "NORMAL"
		 (if (meow-insert-mode-p)
		     "INSERT"
		   (if (meow-motion-mode-p)
		       "MOTION"
		     "KEYPAD")))))
    (concat (propertize " " 'display `((space :align-to (- (+ left left-fringe left-margin) ,w 1))))
	    (propertize mode 'display
                        (svg-tag-make
                         mode
                         :face (if (meow-normal-mode-p)
				   'tab-bar-meow-normal-state
				 (if (meow-insert-mode-p)
				     'tab-bar-meow-insert-state
				   (if (meow-motion-mode-p)
				       'tab-bar-meow-motion-state
				     'tab-bar-meow-keypad-state)))
                         :inverse t :margin 0 :radius 3
                         :height 1 :ascent 16 :scale 1.1))
	    "   L"
            l
	    " C"
	    c)))

(defun +tab-bar-right ()
  (let* ((f buffer-file-truename)
	 (time (format-time-string "%H:%M" (current-time)))
	 (w (string-width (concat f "\t" time "  "))))
    (concat (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,w 1))))
            f
	    "\t"
            time
	    " ")))

(defun +tab-bar-tab-format-function (tab i)
  (let ((current-p (eq (car tab) 'current-tab)))
    (concat
     (propertize (concat
                  " "
                  (alist-get 'name tab)
                  " ")
                 'face
                 (funcall tab-bar-tab-face-function tab))
     " ")))

(defun my/tab-bar-svg-padding (width string)
  (let* ((style svg-lib-style-default)
         (margin      (plist-get style :margin))
         (txt-char-width  (window-font-width nil 'fixed-pitch))
         ;; (tag-width (- width (* margin txt-char-width)))
	 (tag-width (- width (+ (* margin txt-char-width) 20)))
         (padding (- (/ tag-width txt-char-width) (length string))))
    padding))

(defun my/tab-bar-auto-width (items)
  "Return tab-bar items with resized tab names."
  (unless tab-bar--auto-width-hash
    (define-hash-table-test 'tab-bar--auto-width-hash-test
                            #'equal-including-properties
                            #'sxhash-equal-including-properties)
    (setq tab-bar--auto-width-hash
          (make-hash-table :test 'tab-bar--auto-width-hash-test)))
  (let ((tabs nil)    ;; list of resizable tabs
        (non-tabs "") ;; concatenated names of non-resizable tabs
        (width 0))    ;; resize tab names to this width
    (dolist (item items)
      (when (and (eq (nth 1 item) 'menu-item) (stringp (nth 2 item)))
        (if (memq (get-text-property 0 'face (nth 2 item))
                  tab-bar-auto-width-faces)
            (push item tabs)
          (unless (eq (nth 0 item) 'align-right)
            (setq non-tabs (concat non-tabs (nth 2 item)))))))
    (when tabs
      (add-face-text-property 0 (length non-tabs) 'tab-bar t non-tabs)
      (setq width (/ (* (/ (frame-inner-width) 3) 2)  ;; fix tab width
                     (length tabs)))
      (when tab-bar-auto-width-min
        (setq width (max width (if (window-system)
                                   (nth 0 tab-bar-auto-width-min)
                                 (nth 1 tab-bar-auto-width-min)))))
      (when tab-bar-auto-width-max
        (setq width (min width (if (window-system)
                                   (nth 0 tab-bar-auto-width-max)
                                 (nth 1 tab-bar-auto-width-max)))))
      (dolist (item tabs)
        (setf (nth 2 item)
              (with-memoization (gethash (list (selected-frame)
                                               width (nth 2 item))
                                         tab-bar--auto-width-hash)
                (let* ((name (nth 2 item))
                       (len (length name))
                       (close-p (get-text-property (1- len) 'close-tab name))
                       (continue t)
                       (prev-width (string-pixel-width name))
                       curr-width
                       (padding (plist-get svg-lib-style-default :padding)))
                  (cond
                   ((< prev-width width)
                    (setq padding (my/tab-bar-svg-padding width name)))
                   ((> prev-width width)
                    (let ((del-pos1 (if close-p -2 -1))
                          (del-pos2 (if close-p -1 nil)))
                      (while continue
                        (setf (substring name del-pos1 del-pos2) "")
                        (setq curr-width (string-pixel-width name))
                        (if (and (> curr-width width)
                                 (< curr-width prev-width))
                            (setq prev-width curr-width)
                          (setq continue nil))))))
                  (propertize name 'display
                              (svg-tag-make
                               name
                               :face (if (eq (car item) 'current-tab)
                                         'tab-bar-svg-active
                                       'tab-bar-svg-inactive)
                               :inverse t :margin 3 :radius 6 :padding padding
                               :height 1 :ascent 16 :scale 1.1)))))))
    items))

(setq tab-bar-border nil
      tab-bar-close-button-show nil
      tab-bar-back-button nil
      tab-bar-new-button nil
      tab-bar-format '(+tab-bar-left tab-bar-format-tabs +tab-bar-right)
      tab-bar-tab-name-format-function '+tab-bar-tab-format-function
      tab-bar-tab-name-truncated-max 10)

(tab-bar-mode 1)

(advice-add #'tab-bar-auto-width :override #'my/tab-bar-auto-width)


(provide 'init-tab)
