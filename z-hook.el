;;; z-hook.el --- Hook utilities

;; Copyright (C) 2016 <arubyz@gmail.com>

;; Author: <arubyz@gmail.com>
;; Keywords: tools, maint

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

(z:affirm :min-version "24.4" "z-hook.el tested on Emacs 24.4+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook management
;;

(defun z:add-all-hooks (*hook-list)
"Adds all hooks in *HOOK-LIST via `add-hook'.  *HOOK-LIST has the format
  ( (HOOK FUNCTION &optional APPEND LOCAL) ... )
Where HOOK, FUNCTION, APPEND, and LOCAL are the corresponding arguments for
`add-hook'."
  (--each *hook-list (apply 'add-hook it)))
  
(defun z:remove-all-hooks (*hook-list)
"Removes all hooks in *HOOK-LIST via `remove-hook'.  See `add-all-hooks' for
the format of *HOOK-LIST."
  (--each hook-list
    (cl-destructuring-bind (*hook *function &optional *append *local) it
      (remove-hook *hook *function *local))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'z-hook)
;;; z-hook.el ends here
