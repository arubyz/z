;;; z-buffer.el --- Buffer utilities

;; Copyright (C) 2016 <arubyz@gmail.com>

;; Author: <arubyz@gmail.com>
;; Keywords: tools, maint, files, convenience

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
(require 'dash)
(require 'z-min)

(z:affirm :min-version "24.4" "z-buffer.el tested on Emacs 24.4+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enumerate buffers
;;

(put
 (defmacro --each-buffer (&rest *forms)
"Same as `--each', except implicitly over `buffer-list'"
   `(--each (buffer-list) ,@*forms))
 'lisp-indent-function 0)

(put
 (defmacro --map-buffers (*form)
"Same as `--map', except implicitly over `buffer-list'"
   `(--map ,*form (buffer-list)))
 'lisp-indent-function 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get buffer (sub)strings
;;

(cl-defun z:full-buffer-string (&optional (*substring-function 'buffer-substring))
"Returns the content of the entire buffer as a string, including any properties
by default.  This behavior may be changed by specifying a function symbol for
*SUBSTRING-FUNCTION, which should be either  `buffer-substring-no-properties' or
`filter-buffer-substring'.  Narrowing is ignored."
  (apply *substring-function (list 1 (1+ (buffer-size)))))

(cl-defun z:buffer-string (&optional (*substring-function 'buffer-substring))
"Returns the content of the entire buffer as a string, including any properties.
This behavior may be changed by specifying a function symbol for
*SUBSTRING-FUNCTION, which should be either  `buffer-substring-no-properties' or
`filter-buffer-substring'.  Narrowing is honored."
  (apply *substring-function (list (point-min) (point-max))))

(cl-defun z:region-string (&optional (*substring-function 'buffer-substring))
"Returns the content of the region as a string.  This is the text in the current
buffer between point and mark, including any properties.  This behavior may be
changed by specifying a function symbol for *SUBSTRING-FUNCTION, which should be
either  `buffer-substring-no-properties' or `filter-buffer-substring'."
  (apply *substring-function (list (region-beginning) (region-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Position / content inspection
;;

(defsubst z:last-line? ()
"Returns t if point is on the last line of the buffer"
  (eq (point-at-eol) (point-max)))

(defsubst z:first-line? ()
"Returns t if point is on the first line of the buffer"
  (eq (point-at-bol) (point-min)))

(defsubst z:looking-at-line? (*regexp)
"Returns t if the line point is on matches *REGEXP.  This function modifies
the match data the same as `looking-at'."
  (save-excursion
    (beginning-of-line)
    (looking-at regexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer modification
;;

;; From EmacsWiki: http://www.emacswiki.org/emacs/UndoCommands
(put
 (defmacro z:temporary-invisible-change (&rest *forms)
"Executes *FORMS with a temporary `buffer-undo-list', undoing on return.  The
changes you make within *FORMS are undone before returning.  But more
importantly, the buffer's `buffer-undo-list' is not affected.  This allows
you to temporarily modify read-only buffers too.  The result of *FORMS is
returned."
   `(let* ((buffer-undo-list)
           (inhibit-read-only t)
           (*modified? (buffer-modified-p)))
      (save-excursion
        (unwind-protect
            (progn ,@*forms)
          (primitive-undo (length buffer-undo-list) buffer-undo-list)
          (set-buffer-modified-p *modified?)))))
 'lisp-indent-function 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'z-buffer)
;;; z-buffer.el ends here
