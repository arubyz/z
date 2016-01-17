;;; z-frame.el --- Frame utilities

;; Copyright (C) 2016 <arubyz@gmail.com>

;; Author: <arubyz@gmail.com>
;; Keywords: tools, maint, frames

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

(z:affirm :min-version "24.4" "z-frame.el tested on Emacs 24.4+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enumerate frames
;;

(put
 (defmacro --each-frame (&rest *forms)
"Same as `--each', except implicitly over `frame-list'"
   `(--each (frame-list) ,@*forms))
 'lisp-indent-function 0)

(put
 (defmacro --map-frames (*form)
"Same as `--map', except implicitly over `frame-list'"
   `(--map ,*form (frame-list)))
 'lisp-indent-function 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'z-frame)
;;; z-frame.el ends here
