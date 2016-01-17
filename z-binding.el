;;; z-binding.el --- Key binding utilities

;; Copyright (C) 2016 <arubyz@gmail.com>

;; Author: <arubyz@gmail.com>
;; Keywords: tools, maint, convenience

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

(z:affirm :min-version "24.4" "z-binding.el tested on Emacs 24.4+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key binding assertions
;;

(cl-defun z:expect-key (*keys *commands &optional (*keymap-symbol 'global-map))
"Raises a warning when the value bound to *KEYS in the keymap referenced by
*KEYMAP-SYMBOL (a symbol, defaulting to 'global-map) is not `equal' to either
*COMMANDS, or any element of *COMMANDS (if it is a list).  The usage of this
function is meant to mirror that of `bind-key' and `bind-key*'."
  (assert (or (listp *commands) (functionp *commands)))
  (assert (symbolp *keymap-symbol))
  (when (stringp *keys) (setq *keys (kbd *keys)))
  (let ((*actual-command (lookup-key (eval *keymap-symbol) *keys)))
    (unless (or (equal *commands *actual-command)
                (-contains? *commands *actual-command))
      (display-warning 'z:expect-key
                       (format "Keys %s bound to %s in %s, but %s%s was expected"
                               (key-description *keys)
                               *actual-command
                               *keymap-symbol
                               (if (consp *commands) "any of " "")
                               *commands)))))

(cl-defmacro z:expect-keys (&rest *expectations
                            &key ((:map *keymap) 'global-map)
                            &allow-other-keys)
"Applies `z:expect-key' to the `car' and `cdr' (respectively) of each element
of *EXPECTATIONS, using keymap *KEYMAP (defaulting to 'global-map).  The usage
of this function is meant to mirror that of `bind-keys' and `bind-keys*'."
  ;; Remove expected keyword arguments from the argument list
  (remf *expectations ':map)
  `(progn
     ,@(--map `(z:expect-key ',(car it) ',(cdr it) ',*keymap)
              *expectations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'z-binding)
;;; z-binding.el ends here
