;;; ushin-shapes.el --- ushin shapes in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2022 Joseph Turner

;; Author: Joseph Turner <joseph@breatheoutbreathe.in>
;; URL: https://git.sr.ht/~breatheoutbreathein/ushin-shapes.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (svg-tag-mode "0.3.2") (svg-lib "0.2.5"))

;; Keywords: convenience

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ushin-shapes.el replaces ushin tags with ushin shapes
;; (https://ushin.org/#shapes) in org documents.

;;;; Usage

;; Use `ushin-shapes-mode' in all org buffers
;; (global-ushin-shapes-mode +1)

;;; Code:

(require 'cl-lib)
(require 'svg-lib)
(require 'svg-tag-mode)

(defconst ushin-shapes-shapes
  '("facts" "thoughts" "feelings" "needs" "topics" "actions" "people")
  "List of ushin shapes.")

(defun ushin-shapes--build-tag (shape)
  "Build `svg-tag-mode' tag from SHAPE."
  `(,(concat ":\\(" shape "\\):") .
    ((lambda (tag)
       (svg-lib-icon ,shape
                     '(:background "transparent"
                                   :padding 0
                                   :stroke 0
                                   :height 1
                                   :scale 1
                                   :collection "ushin"))))))

(defun ushin-shapes--tags ()
  "Return list of ushin `svg-tag-mode' tags."
  (mapcar #'ushin-shapes--build-tag ushin-shapes-shapes))

;;;###autoload
(define-minor-mode ushin-shapes-mode
  "Minor mode to replace ushin tags with shapes."
  (if ushin-shapes-mode
      (progn
        (cl-pushnew '("ushin" . "https://git.sr.ht/~breatheoutbreathein/ushin-shapes.el/blob/master/shapes/%s.svg") svg-lib-icon-collections :test #'equal)
        (mapc (lambda (tag) (cl-pushnew tag svg-tag-tags :test #'equal))
              (ushin-shapes--tags))
        (add-hook 'org-mode-hook #'svg-tag-mode))
    (setf svg-tag-tags (cl-set-difference svg-tag-tags (ushin-shapes--tags) :test #'equal))
    ;; FIXME: Instead of removing the hook, we should restore the previous value
    (remove-hook 'org-mode-hook #'svg-tag-mode)))

;;;###autoload
(define-globalized-minor-mode
  global-ushin-shapes-mode ushin-shapes-mode ushin-shapes-mode-on)

(provide 'ushin-shapes)
;;; ushin-shapes.el ends here
