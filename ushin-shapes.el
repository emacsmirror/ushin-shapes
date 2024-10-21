;;; ushin-shapes.el --- USHIN shapes in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2022-24 Joseph Turner

;; Author: Joseph Turner <joseph@ushin.org>
;; URL: https://git.sr.ht/~ushin/ushin-shapes.el
;; Version: 0.3-pre
;; Package-Requires: ((emacs "27.1") (svg-tag-mode "0.3.3") (svg-lib "0.3") (compat "30.0.0.0"))

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

;; This package replaces ushin tags with ushin shapes in org documents.
;; See https://ushin.org/#shapes to learn about ushin shapes.

;; To use `ushin-shapes-mode' in all org buffers enable
;; `global-ushin-shapes-mode'.

;;; Code:

(require 'cl-lib)
(require 'svg-lib)
(require 'svg-tag-mode)
(require 'compat)

(defgroup ushin-shapes nil
  "Replace ushin tags with shapes corresponding to ushin keywords."
  :group 'convenience
  :prefix "ushin-shapes-")

(defcustom ushin-shapes-foreground-color "purple"
  "Foreground color for SVG shapes."
  :type 'color
  :set (lambda (option value)
         (set-default option value)
         ;; Re-enable `ushin-shapes-mode' to apply newly chosen color.
         (dolist (ushin-shapes-buffer
                  (match-buffers
                   (lambda (buf _arg)
                     (buffer-local-value 'ushin-shapes-mode buf))))
           (with-current-buffer ushin-shapes-buffer
             (ushin-shapes-mode -1)
             (ushin-shapes-mode +1)))))

(defconst ushin-shapes-shapes
  '("facts" "thoughts" "feelings" "needs" "topics" "actions" "people")
  "List of ushin shapes.")

(defconst ushin-shapes-icon-collection
  "https://git.sr.ht/~ushin/ushin-shapes.el/blob/master/shapes/%s.svg"
  "Base-url of the ushin icon collection.")

(defun ushin-shapes--build-tag (shape)
  "Build `svg-tag-mode' tag from SHAPE."
  `(,(format ":\\(%s\\):" shape)
    (lambda (arg)
      (svg-lib-icon ,shape nil
                    :background "transparent"
                    :foreground ,ushin-shapes-foreground-color
                    :padding 0
                    :stroke 0
                    :height 1
                    :scale 0.8
                    :collection "ushin"))
    nil nil))

(defun ushin-shapes-tags ()
  "Return list of ushin `svg-tag-mode' tags."
  (mapcar #'ushin-shapes--build-tag ushin-shapes-shapes))

;;;###autoload
(define-minor-mode ushin-shapes-mode
  "Minor mode to replace ushin tags with shapes."
  :lighter " ushin-shapes"
  (cond (ushin-shapes-mode
         (cl-pushnew (cons "ushin" ushin-shapes-icon-collection)
                     svg-lib-icon-collections :test #'equal)
         (dolist (tag (ushin-shapes-tags))
           (cl-pushnew tag svg-tag-tags :test #'equal))
         (svg-tag-mode +1)
         (add-hook 'text-scale-mode-hook #'ushin-shapes-mode))
        (t
         (setf svg-tag-tags (cl-set-difference svg-tag-tags (ushin-shapes-tags)
                                               :test #'equal :key #'car))
         (svg-tag-mode -1)
         (remove-hook 'text-scale-mode-hook #'ushin-shapes-mode))))

(defun ushin-shapes-mode--turn-on ()
  "Turn on `ushin-shapes-mode' if desired."
  (when (derived-mode-p 'org-mode)
    (ushin-shapes-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-ushin-shapes-mode ushin-shapes-mode
  ushin-shapes-mode--turn-on
  :group 'ushin-shapes)

(provide 'ushin-shapes)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ushin-shapes.el ends here
