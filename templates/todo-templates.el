;;; todo-templates.el --- Templating for my TODO automatic insertions  -*- lexical-binding: t; -*-

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

;; TODO(map) : Figure out why this isn't working anymore at some point
;;   (indent-region (point-min) (point-max) nil))


;;; Code:

;; Add TODO key binding for Java mode.
(define-skeleton java-todo-skeleton
  "Inserts a TODO for a Java file."
  nil
  "// TODO(map) \: "
  )
(defun insert-java-todo ()
  (java-todo-skeleton)
  )

;; Add TODO key binding for JavaScript mode.
(define-skeleton javascript-todo-skeleton
  "Inserts a TODO for a JavaScript file."
  nil
  "// TODO(map) : "
)
(defun insert-javascript-todo ()
  (javascript-todo-skeleton)
  )

;; Add TODO key binding for Python mode.
(define-skeleton python-todo-skeleton
  "Inserts a TODO for a Python file."
  nil
  "# TODO(map) : "
)(defun insert-python-todo ()
  (python-todo-skeleton)
  )

;; Add TODO key binding for HTML mode.
(define-skeleton html-todo-skeleton
  "Inserts a TODO for a HTML file."
  nil
  "<!-- TODO(map) : -->"
)(defun insert-html-todo ()
  (html-todo-skeleton)
  )

;; Add TODO key binding for C++ mode.
(define-skeleton cpp-todo-skeleton
  "Inserts a TODO for a C Plus Plus file."
  nil
  "// TODO(map) : "
)(defun insert-cpp-todo ()
  (cpp-todo-skeleton)
  )

;; Add TODO key binding for C# mode.
(define-skeleton csharp-todo-skeleton
  "Inserts a TODO for a C# file."
  nil
  "// TODO(map) : "
)(defun insert-csharp-todo ()
  (csharp-todo-skeleton)
  )

;; Add TODO key binding for PHP mode.
(define-skeleton php-todo-skeleton
  "Inserts a TODO for a PHP file."
  nil
  "// TODO(map) : "
)(defun insert-php-todo ()
  (php-todo-skeleton)
  )

;; Add TODO key binding for Ruby mode.
(define-skeleton ruby-todo-skeleton
  "Inserts a TODO for a Ruby file."
  nil
  "# TODO(map) : "
)(defun insert-ruby-todo ()
  (ruby-todo-skeleton)
  )

;; Add TODO key binding for Emacs Lisp mode.
(define-skeleton elisp-todo-skeleton
  "Inserts a TODO for an ELisp file."
  nil
  ";; TODO(map) : "
)(defun insert-elisp-todo ()
  (elisp-todo-skeleton)
  )

;; Add TODO key binding for Kotlin mode.
(define-skeleton kotlin-todo-skeleton
  "Inserts a TODO for an Kotlin file."
  nil
  "// TODO(map) : "
)(defun insert-kotlin-todo ()
  (kotlin-todo-skeleton)
  )

;; Add TODO key binding for Org mode.
(define-skeleton org-todo-skeleton
  "Inserts a TODO for a Org file."
  nil
  "* TODO " (setq description (skeleton-read "Short Description: ")) " [[][]]"
  "" \n
  "" (setq description (skeleton-read "Long Description: ")) ""
  "" \n
  )
(defun insert-org-todo ()
  (org-todo-skeleton)
  )

(provide 'todo-templates)
;;; todo-templates.el ends here
