;;; z-window.el --- Window utilities

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

(z:affirm :min-version "24.4" "z-window.el tested on Emacs 24.4+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enumerate windows
;;

(put
 (defmacro --each-window (&rest *forms)
"Same as `--each', except implicitly over `window-list'"
   `(--each (window-list) ,@*forms))
 'lisp-indent-function 0)

(put
 (defmacro --map-windows (*form)
"Same as `--map', except implicitly over `window-list'"
   `(--map ,*form (window-list)))
 'lisp-indent-function 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Position and geometry inspection
;;

;;;###autoload
(defun z:beginning-of-window ()
"Moves point in the current buffer to the top of the selected window, or as
close as possible without causing the window to scroll."
  (interactive)
  ;; Note: (goto-char (window-start)) causes the window to scroll.
  ;; This is borrowed from the definition of `move-to-window-line-top-bottom'.
  (move-to-window-line scroll-margin))
  
;;;###autoload
(defun z:end-of-window ()
"Moves point in the current buffer to the bottom of the selected window, or as
close as possible without causing the window to scroll."
  (interactive)
  ;; Note: (goto-char (window-end)) causes the window to scroll.
  ;; This is borrowed from the definition of `move-to-window-line-top-bottom'.
  (move-to-window-line (- -1 scroll-margin)))

(defun z:window-top-or-bottom ()
"Determines if the selected window is more towards the top or the bottom of its
frame.  Returns a symbol, either 'top or 'bottom."
  (cl-destructuring-bind (x-min y-min x-max y-max)
      (window-edges)
    (if (< y-min (/ (- (frame-height) (window-height)) 2))
       'top 'bottom)))

(defun z:window-left-or-right ()
"Determines if the selected window is more towards the left or the right of its
frame.  Returns a symbol, either 'left or 'rigth."
  (cl-destructuring-bind (x-min y-min x-max y-max)
      (window-edges)
    (if (< x-min (/ (- (frame-width) (window-width)) 2))
        'left 'right)))

(cl-defun z:window-leftmost? (&optional (*window (selected-window)))
"Returns t if *WINDOW (the selected window by default) is leftmost in its frame."
  (or (window-minibuffer-p)
      (cl-destructuring-bind (x-min y-min x-max y-max)
          (window-edges *window)
        (eq x-min 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window resizing
;;

(defun z:-retry-window-resizing (resize-command)
"Assuming the current buffer is the minibuffer, attempt to select another
window and execute RESIZE-COMMAND"
  (assert (symbolp resize-command))
  (assert (window-minibuffer-p))
  (let ((next-window (window-in-direction 'above nil 'ignore)))
    (when (and next-window (not (eq (selected-window) next-window)))
      (with-selected-window next-window
        (unless (window-minibuffer-p)
          (apply (list resize-command)))))))

;;;###autoload
(defun z:window-vertical-edge-left ()
"Moves the vertical edge of the selected window left one column.  Does nothing
if the current buffer is the minibuffer."
  (interactive)
  (if (window-minibuffer-p)
      (z:-retry-window-resizing 'z:window-vertical-edge-left)
    (shrink-window-horizontally
     (if (eq (z:window-left-or-right) 'left) 1 -1))))

;;;###autoload
(defun z:window-vertical-edge-right ()
"Moves the vertical edge of the selected window right one column.  Does nothing
if the current buffer is the minibuffer."
  (interactive)
  (if (window-minibuffer-p)
      (z:-retry-window-resizing 'z:window-vertical-edge-right)
    (shrink-window-horizontally
     (if (eq (z:window-left-or-right) 'right) 1 -1))))

;;;###autoload
(defun z:window-horizontal-edge-up ()
"Moves the horizontal edge of the selected window up one row.  Does nothing if
the current buffer is the minibuffer."
  (interactive)
  (if (window-minibuffer-p)
      (z:-retry-window-resizing 'z:window-horizontal-edge-up)
    (shrink-window
     (if (eq (z:window-top-or-bottom) 'top) 1 -1))))

;;;###autoload
(defun z:window-horizontal-edge-down ()
"Moves the horizontal edge of the selected window down one row.  Does nothing
if the current buffer is the minibuffer."
  (interactive)
  (if (window-minibuffer-p)
      (z:-retry-window-resizing 'z:window-horizontal-edge-down)
    (shrink-window
     (if (eq (z:window-top-or-bottom) 'bottom) 1 -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'z-window)
;;; z-window.el ends here
