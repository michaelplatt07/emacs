;;; comment-block-templates.el --- File that contains a variety of comment block skeletons for auto insert  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  michael

;; Author: michael <michael@Sakura.com>
;; Keywords: 

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


;; Add comment block key binding for Java mode.
(define-skeleton java-comment-block-skeleton
  "Inserts a comment block for a Java file."
  nil
  "/**" \n
  _ "* " \n
  "*/ "
  (indent-region (point-min) (point-max) nil))
(defun  insert-java-comment-block ()
  (java-comment-block-skeleton)
  )

;; Add comment block key binding for JavaScript mode.
(define-skeleton javascript-comment-block-skeleton
  "Inserts a comment block for a JavaScript file."
  nil
  "/**" \n
  _ "* " \n
  "*/ "
  (indent-region (point-min) (point-max) nil))
(defun insert-javascript-comment-block ()
  (javascript-comment-block-skeleton)
  )

;; Add comment block key binding for Python mode.
(define-skeleton python-comment-block-skeleton
  "Inserts a comment block for a Python file."
  nil
  "\"\"\"" \n
  _ "" \n
  "\"\"\""
  (indent-region (point-min) (point-max) nil))
(defun insert-python-comment-block ()
  (python-comment-block-skeleton)
  )

;; Add comment block key binding for HTML mode.
(define-skeleton html-comment-block-skeleton
  "Inserts a comment block for a HTML file."
  nil
  "<!--" \n
  _ "" \n
  "-->"
  (indent-region (point-min) (point-max) nil))
(defun insert-html-comment-block ()
  (html-comment-block-skeleton)
  )

;; Add comment block key binding for C++ mode.
(define-skeleton cpp-comment-block-skeleton
  "Inserts a comment block for a C Plus Plus file."
  nil
  "/**" \n
  _ "* " \n
  "*/ "
  (indent-region (point-min) (point-max) nil))
(defun insert-cpp-comment-block ()
  (cpp-comment-block-skeleton)
  )

;; Add comment block key binding for C# mode.
(define-skeleton csharp-comment-block-skeleton
  "Inserts a comment block for a C# file."
  nil
  "/**" \n
  _ "* " \n
  "*/ "
  (indent-region (point-min) (point-max) nil))
(defun insert-csharp-comment-block ()
  (csharp-comment-block-skeleton)
  )

;; Add comment block key binding for PHP mode.
(define-skeleton php-comment-block-skeleton
  "Inserts a comment block for a PHP file."
  nil
  "/* " _ " */ "
  (indent-region (point-min) (point-max) nil))
(defun insert-php-comment-block ()
  (php-comment-block-skeleton)
  )

;; Add comment block key binding for Ruby mode.
(define-skeleton ruby-comment-block-skeleton
  "Inserts a comment block for a Ruby file."
  nil
  "\=begin" \n
  _ \n
  "\=end"
  (indent-region (point-min) (point-max) nil))
(defun insert-ruby-comment-block ()
  (ruby-comment-block-skeleton)
  )

(provide 'comment-block-templates)
;;; comment-block-templates.el ends here
