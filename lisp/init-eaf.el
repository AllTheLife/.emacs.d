;; init-eaf.el --- Use EAF to live in emacs.	-*- lexical-binding: t -*-

;; Copyright (C) 2022 AllTheLife

;; Author: AllTheLife <xjn208930@gmail.com>
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
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-packages/emacs-application-framework")
(require 'eaf)
(require 'eaf-browser)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-org-previewer)
(require 'eaf-image-viewer)
;; (require 'eaf-system-monitor)
;; (require 'eaf-music-player)
(use-package eaf
  :load-path "~/.emacs.d/site-packages/emacs-application-framework"
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  (eaf-browser-translate-language "zh-CN")
  :config
  (defalias 'browse-web #'eaf-open-browser)
  :bind
  (("C-c mo" . eaf-open)
   ("C-c mb" . eaf-open-browser)
   ("C-c ms" . eaf-search-it)
   ("C-c b"  . eaf-open-bookmark)))

(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
