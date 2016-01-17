;;; z-min.el --- Core set of utilities required by all z.el sub-modules

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing platform version and capabilities
;;

(cl-defmacro z:affirm (&rest *args
                       &key
                       ((:min-version *min-version) nil *min-version?)
                       ((:exact-version *exact-version) nil *exact-version?)
                       ((:fbound *fbound) nil *fbound?)
                       ((:bound *bound) nil *bound?)
                       ((:bound-and-true *bound-and-true) nil *bound-and-true?)
                       ((:feature *feature) nil *feature?)
                       ((:condition *condition) nil *condition?)
                       &allow-other-keys)
"Macro to succinctly test for various conditions that an Emacs package may
assume when being loaded.  Conditions are specified using keyword arguments.
If any specified condition is not met, a warning is raised by passing all
non-keyword parameters to `display-warning'.  The following conditions are
currently supported:
    :min-version VERSION        `emacs-version' must not be less than VERSION
    :exact-version VERSION      `emacs-version' must be equal to VERSION
    :fbound FUNCTION            FUNCTION must have a bound function value
    :bound VARIABLE             VARIABLE must have a bound value
    :bound-and-true VARIABLE    VARIABLE must have a bound value
    :feature FEATURE            FEATURE must be loaded as per `featurep'
    :condition CONDITION        CONDITION must evaluate to non-nil"
  ;; Remove expected keyword arguments from the argument list
  (remf *args ':min-version)
  (remf *args ':exact-version)
  (remf *args ':fbound)
  (remf *args ':bound)
  (remf *args ':bound-and-true)
  (remf *args ':feature)
  (remf *args ':condition)
  ;; Create tests based on supplied predicates
  `(when (or
          ,(when *min-version?
             `(version< emacs-version ,*min-version))
          ,(when *exact-version?
             `(not (version= emacs-version ,*exact-version)))
          ,(when *fbound?
             `(not (fboundp ',*fbound)))
          ,(when *bound?
             `(not (boundp ',*bound)))
          ,(when *bound-and-true?
             `(not (bound-and-true-p ,*bound-and-true)))
          ,(when *feature?
             `(not (featurep ',*feature)))
          ,(when *condition?
             `(not ,*condition)))
     ;; Predicate(s) failed, show the specified warning message
     (display-warning 'z:affirm (format ,@*args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'z-min)
;;; z-min.el ends here
