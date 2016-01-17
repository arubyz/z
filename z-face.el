;;; z-face.el --- Face and font utilities

;; Copyright (C) 2016 <arubyz@gmail.com>

;; Author: <arubyz@gmail.com>
;; Keywords: tools, maint, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'z-min)

(z:affirm :min-version "24.4" "z-face.el tested on Emacs 24.4+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;

(defun z:face-at-point ()
"Return the face or faces (if any) at point."
  (or (get-char-property (point) 'read-face-name)
      (get-char-property (point) 'face)))

(defvar *z:list-fonts-sample-text
  "The quick brown fox jumps over the lazy dog ´`''\"\"1lI|¦!Ø0Oo{[()]}.,:; "
"Text string to display as the sample text for `list-fonts-display'.")

;;;###autoload
(defun z:list-fonts-display (&optional *mono-only)
"List all fonts (actually font families), using the same sample text in each.
The sample text the string defined by the variable `list-fonts-sample-text'.

If *MONO-ONLY is non-nil (or called interactively with a prefix argument), only
list mono-spaced fonts."
  (interactive "P")
  (pop-to-buffer (get-buffer-create "*Fonts*"))
  (fundamental-mode)
  (read-only-mode -1)
  (widen)
  (delete-region (point-min) (point-max))
  (font-lock-mode +1)
  (let ((*sample *z:list-fonts-sample-text)
        (*families (cl-remove-duplicates
                    (sort (font-family-list)
                          (lambda (*x *y) (string< (upcase *x) (upcase *y))))
                    :test 'string=)))
    (dolist (*family *families)
      (when (or (not *mono-only)
                (let ((case-fold-search t))
                  (string-match "mono" *family)))
        (insert
         (propertize *sample 'font-lock-face `(:family ,*family))
         (propertize *family 'font-lock-face `(:family ,*family :box t))
         "\n"
         (propertize *sample 'font-lock-face `(:family ,*family :slant italic))
         (propertize *family 'font-lock-face `(:family ,*family :slant italic :box t))
         "\n"))))
  ;; Use the same mode as `list-faces-display' (eg, binds the q key to `quit-window')
  (help-mode))

;;;###autoload
(defun z:list-mono-fonts-display ()
"Like `list-faces-display' but only lists mono-spaced fonts."
  (interactive)
  (z:list-fonts-display t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'z-face)
;;; z-face.el ends here
