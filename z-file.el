;;; z-file.el --- File and path utilities

;; Copyright (C) 2016 <arubyz@gmail.com>

;; Author: <arubyz@gmail.com>
;; Keywords: tools, maint, files

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
(require 'cl)
(require 'f)
(require 'z-min)

(z:affirm :min-version "24.4" "z-file.el tested on Emacs 24.4+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path utilities
;;

(defun z:current-buffer-path ()
"Best effort to determine the path of the file or directory represented in the
current buffer"
  (or buffer-file-name
      load-file-name
      default-directory
      (and (eq major-mode 'dired-mode)
           dired-directory)))

(defun z:current-buffer-directory-name ()
"Best effort to get the name (not the full path) of the most specific directory
contained in the path for the current buffer.  For example:
    /path/to/foo/bar.c -> foo
    /path/to/foo/      -> foo
    /path/to/foo       -> foo (assuming foo is a directory)
    /path/to/foo       -> to (assuming foo is a file)
    /                  -> /"
  ;; Return just the last component of the directory path
  (car
   (last
    (f-split
     ;; `file-name-directory' will return "/foo/bar/" given "/foo/bar/", unlike
     ;; `f-dirname', which will return "/foo/" in this case
     (file-name-directory
      ;; `f-slash' will intelligently add a slash at the end of the path,
      ;; depending on whether the path points to a file or a directory
      (f-slash
       ;; `f-split' doesn't work correctly on "~/", so we always make sure to
       ;; expand the file name first
       (expand-file-name
        (z:current-buffer-path))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'z-file)
;;; z-file.el ends here
