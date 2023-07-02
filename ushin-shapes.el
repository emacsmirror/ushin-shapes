;;; ushin-shapes.el --- USHIN shapes in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2022-23 Joseph Turner

;; Author: Joseph Turner <joseph@ushin.org>
;; URL: https://git.sr.ht/~ushin/ushin-shapes.el
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (svg-tag-mode "0.3.2") (svg-lib "0.2.5"))

;; Keywords: convenience

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.

;; For a full copy of the GNU Affero General Public License see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ushin-shapes.el replaces ushin tags with ushin shapes
;; (https://ushin.org/#shapes) in org documents.

;;;; Usage

;; Use `ushin-shapes-mode' in all org buffers
;; (global-ushin-shapes-mode)

;;; Code:

(require 'cl-lib)
(require 'svg-lib)
(require 'svg-tag-mode)

(defgroup ushin-shapes nil
  "Replace ushin tags with shapes corresponding to ushin keywords."
  :group 'convenience
  :prefix "ushin-shapes-")

(defcustom ushin-shapes-foreground-color "purple"
  "Foreground color for SVG shapes."
  :type 'color)

(defconst ushin-shapes-shapes
  '("facts" "thoughts" "feelings" "needs" "topics" "actions" "people")
  "List of ushin shapes.")

(defun ushin-shapes--build-tag (shape)
  "Build `svg-tag-mode' tag from SHAPE."
  `(,(concat ":\\(" shape "\\):") .
    ((lambda (tag)
       (svg-lib-icon ,shape
                     `(:background "transparent"
                                   :foreground ,ushin-shapes-foreground-color
                                   :padding 0
                                   :stroke 0
                                   :height 1
                                   :scale 0.8
                                   :collection "ushin"))))))

(defun ushin-shapes-tags ()
  "Return list of ushin `svg-tag-mode' tags."
  (mapcar #'ushin-shapes--build-tag ushin-shapes-shapes))

;;;###autoload
(define-minor-mode ushin-shapes-mode
  "Minor mode to replace ushin tags with shapes."
  :lighter " ushin-shapes"
  (if ushin-shapes-mode
      (when (derived-mode-p 'org-mode)
        (cl-pushnew '("ushin" . "https://git.sr.ht/~ushin/ushin-shapes.el/blob/master/shapes/%s.svg") svg-lib-icon-collections :test #'equal)
        (mapc (lambda (tag) (cl-pushnew tag svg-tag-tags :test #'equal))
              (ushin-shapes-tags))
        (svg-tag-mode +1))
    (setf svg-tag-tags (cl-set-difference svg-tag-tags (ushin-shapes-tags)
                                          :test #'equal :key #'car))
    (svg-tag-mode -1)))

;;;###autoload
(define-globalized-minor-mode global-ushin-shapes-mode ushin-shapes-mode
  (lambda () (ushin-shapes-mode 1))
  :group 'ushin-shapes)

(provide 'ushin-shapes)
;;; ushin-shapes.el ends here
