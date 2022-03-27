;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2021 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/AllTheLife/.emacs.d

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
;; Customization.
;;

;;; Code:

(defgroup liven nil
  "Liven Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/AllTheLife/.emacs.d"))

(defcustom liven-logo (expand-file-name
                       (if (display-graphic-p) "logo.png" "banner.txt")
                       user-emacs-directory)
  "Set Liven logo. nil means official logo."
  :group 'liven
  :type 'string)

(defcustom liven-full-name user-full-name
  "Set user full name."
  :group 'liven
  :type 'string)

(defcustom liven-mail-address user-mail-address
  "Set user email address."
  :group 'liven
  :type 'string)

(defcustom liven-org-directory (expand-file-name "~/org/")
  "Set org directory."
  :group 'liven
  :type 'string)

(defcustom liven-proxy "127.0.0.1:1087"
  "Set HTTP/HTTPS proxy."
  :group 'liven
  :type 'string)

(defcustom liven-socks-proxy "127.0.0.1:1086"
  "Set SOCKS proxy."
  :group 'liven
  :type 'string)

(defcustom liven-server t
  "Enable `server-mode' or not."
  :group 'liven
  :type 'boolean)

(defcustom liven-icon (or (display-graphic-p) (daemonp))
  "Display icons or not."
  :group 'liven
  :type 'boolean)

;; Emacs Lisp Package Archive (ELPA)
;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom liven-package-archives-alist
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    `(,(cons 'melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
      ,(cons 'bfsu
             `(,(cons "gnu"   (concat proto "://mirrors.bfsu.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.bfsu.edu.cn/elpa/melpa/"))))
      ,(cons 'emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
      ,(cons 'netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
      ,(cons 'ustc
             `(,(cons "gnu"   (concat proto "://mirrors.ustc.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.ustc.edu.cn/elpa/melpa/"))))
      ,(cons 'tencent
             `(,(cons "gnu"   (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))))
      ,(cons 'tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))))
  "The package archives group list."
  :group 'liven
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom liven-package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'liven
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value liven-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    liven-package-archives-alist)))

(defcustom liven-theme-alist
  '((default . doom-one)
    (pro     . doom-monokai-pro)
    (dark    . doom-dark+)
    (light   . doom-one-light)
    (warm    . doom-solarized-light)
    (cold    . doom-city-lights)
    (day     . doom-tomorrow-day)
    (night   . doom-tomorrow-night))
  "List of themes mapped to internal themes."
  :group 'liven
  :type '(alist :key-type (symbol :tag "Theme")
                :value-type (symbol :tag "Internal theme")))

(defcustom liven-auto-themes '(("8:00"  . doom-one-light)
				               ("19:00" . doom-one))
  "List of themes mapped to the time they should be loaded.

The keywords `:sunrise' and `:sunset' can be used for the time
if `calendar-latitude' and `calendar-longitude' are set.
For example:
  '((:sunrise . doom-one-light)
    (:sunset  . doom-one))"
  :group 'liven
  :type '(alist :key-type (string :tag "Time")
                :value-type (symbol :tag "Theme")))

(when (boundp 'ns-system-appearance)
  (defcustom liven-system-themes '((light . doom-one-light)
				                   (dark  . doom-one))
    "List of themes related the system appearance. It's only available on macOS."
    :group 'liven
    :type '(alist :key-type (symbol :tag "Appearance")
                  :value-type (symbol :tag "Theme"))))

(defcustom liven-theme 'default
  "The color theme."
  :group 'liven
  :type `(choice (const :tag "Auto" auto)
                 (const :tag "Random" random)
                 ,(if (boundp 'ns-system-appearance)
                      '(const :tag "System" system)
                    "")
                 ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    liven-theme-alist)
                 symbol))

(defcustom liven-completion-style 'childframe
  "Completion display style."
  :group 'liven
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom liven-dashboard (not (daemonp))
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'liven
  :type 'boolean)

(defcustom liven-restore-frame-geometry t
  "Restore the frame's geometry at startup.
If Non-nil, save and restore the frame's geometry."
  :group 'liven
  :type 'boolean)

(defcustom liven-lsp 'lsp-mode
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
tags: Use tags file instead of language server. See https://github.com/universal-ctags/citre.
nil means disabled."
  :group 'liven
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom liven-lsp-format-on-save-ignore-modes
  '(python-mode c++-mode c-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes.
"
  :group 'liven
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom liven-chinese-calendar nil
  "Use Chinese calendar or not."
  :group 'liven
  :type 'boolean)

(defcustom liven-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-" . ?←)
    ("->" . ?→)
    ("->>" . ?↠)
    ("=>" . ?⇒)
    ("map" . ?↦)
    ("/=" . ?≠)
    ("!=" . ?≠)
    ("==" . ?≡)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("=<<" . (?= (Br . Bl) ?≪))
    (">>=" . (?≫ (Br . Bl) ?=))
    ("<=<" . ?↢)
    (">=>" . ?↣)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("not" . ?¬))
  "Alist of symbol prettifications.
Nil to use font supports ligatures."
  :group 'liven
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom liven-prettify-org-symbols-alist
  '(("[ ]" . ?☐)
    ("[X]" . ?☑)
    ("[-]" . ?⛝)

    ("#+ARCHIVE:" . ?📦)
    ("#+AUTHOR:" . ?👤)
    ("#+CREATOR:" . ?💁)
    ("#+DATE:" . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    ("#+EMAIL:" . ?📧)
    ("#+OPTIONS:" . ?⛭)
    ("#+SETUPFILE:" . ?⛮)
    ("#+TAGS:" . ?🏷)
    ("#+TITLE:" . ?📓)

    ("#+BEGIN_SRC" . ?✎)
    ("#+END_SRC" . ?□)
    ("#+BEGIN_QUOTE" . ?»)
    ("#+END_QUOTE" . ?«)
    ("#+HEADERS" . ?☰)
    ("#+RESULTS:" . ?💻))
  "Alist of symbol prettifications for `org-mode'."
  :group 'liven
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
