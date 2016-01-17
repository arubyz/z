;;; z-math.el --- Numeric / math utilities

;; Copyright (C) 2016 <arubyz@gmail.com>

;; Author: <arubyz@gmail.com>
;; Keywords: tools, maint, extensions

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
(require 'z-min)

(z:affirm :min-version "24.4" "z-math.el tested on Emacs 24.4+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numeric constants
;;

(defmacro z:n (&rest *args)
"A macro to create a numeric constant from a description which is intended to
maximize human readability.  Since it is a macro, it can only produce constant
values, however it expands at compile time and has no performance penalty when
compiled.

It operates by multiplying all arguments together, with particular substitutions
performed before the multiplication operation.  In effect, this allows a numeric
constant to be specified using both digit groupings and standard SI suffixes.
For example:

    (z:n 0)                     => 0
    (z:n 1)                     => 1
    (z:n 2 k)                   => 2 * 1000 = 2000
    (z:n 3 000)                 => 3 * 1000 = 3000
    (z:n 44 mib)                => 44 * 1024^2 = 46137344
    (z:n 555 000 gib)           => 55 * 1000 * 1024^3 = 59055800320000

It can also be used to construct duration values, for example:

    (z:n 7 60 60)               => The number of seconds in 7 hours (25200)

The full list of substitutions understood by this macro is as follows:

    0 (may be written as 000)   => 1000 (except for the first argument)
    k                           => 1000 (one thousand)
    m                           => 1000000 (one million)
    g                           => 1000000000 (one billion)
    t                           => 1000000000000 (one trillion)
    kib                         => 1024 (one kibibyte)
    mib                         => 1048576 (one mibibyte)
    gib                         => 1073741824 (one gibibyte)
    tib                         => 1099511627776 (one tibibyte)"
  (apply '* (loop for *i from 0
                  for *arg in *args
                  ;; Replace 000 (ie, 0) with 1000 (except for the first arg)
                  collect (cond ((and (< 0 *i) (eq *arg 0)) 1000)
                                ;; Leave all other numbers as-is
                                ((numberp *arg) *arg)
                                ;; Traditional SI suffixes
                                ((eq *arg 'k) 1000)
                                ((eq *arg 'm) 1000000)
                                ((eq *arg 'g) 1000000000)
                                ((eq *arg 't) 1000000000000)
                                ;; Binary SI suffixes
                                ((eq *arg 'kib) 1024)
                                ((eq *arg 'mib) 1048576)
                                ((eq *arg 'gib) 1073741824)
                                ((eq *arg 'tib) 1099511627776)
                                ;; Unexpected argument
                                (t (error "Unexpected argument: %s" *arg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'z-math)
;;; z-math.el ends here
