;;; my-templates.el --- Template file that loads a bunch of skeleton functions for what I need  -*- lexical-binding: t; -*-

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
(define-skeleton python-doc-skeleton
  "Inserts a PEP formatted Docstring for Python."
  nil
  "\"\"\"" (setq description (skeleton-read "Description: ")) \n
  "" \n
  "Args:" \n
  "param1: The first parameter." \n
  "" \n
  "Returns:" \n
  "The return value. True for success, False otherwise." \n
  "" \n
  "\"\"\""
  (indent-region (point-min) (point-max) nil))

(defun get-python-doc ()
  (python-doc-skeleton)
  )

(define-skeleton java-doc-skeleton
  "Inserts a Java formatted Docstring."
  nil
  "/**" \n
  "* " (setq short_description (skeleton-read "Short description: ")) \n
  "* " \n
  "* " (setq long_description (skeleton-read "Long description: ")) \n
  "* " \n
  "* @param PARAM DESCRIPTION" \n
  "* @return RETURNS" \n
  "*/"
  (indent-region (point-min) (point-max) nil))

(defun get-java-doc ()
  (java-doc-skeleton)
  )

(define-skeleton javascript-doc-skeleton
  "Inserts a JavaScript formatted Docstring."
  nil
  "/**" \n
  "* " (setq short_description (skeleton-read "Short description: ")) \n
  "* " \n
  "* " (setq long_description (skeleton-read "Long description: ")) \n
  "* " \n
  "* @param PARAM DESCRIPTION" \n
  "* @return RETURNS" \n
  "*/"
  (indent-region (point-min) (point-max) nil))

(defun get-javascript-doc ()
  (javascript-doc-skeleton)
  )

(define-skeleton javascript-swagger-skeleton
  "Inserts a JavaScript formatted Docstring."
  nil
  "/**" \n
  "* @swagger" \n
  "* " \n
  "* " (setq url (skeleton-read "URL: ")) \n
  "*   " (setq method_type (skeleton-read "Method Type: ")) \n
  "*     description: " (setq description (skeleton-read "Description: ")) \n
  "*     produces: " \n
  "*       - " (setq produces (skeleton-read "Produces: ")) \n
  "*     responses: " \n
  "*       ERROR_NUMBER:" \n
  "*         ERROR_TYPE: ERROR_MESSAGE," \n
  "*     example: " \n
  "*       " (setq example (skeleton-read "Example URL: ")) \n
  "*/"
  (indent-region (point-min) (point-max) nil))

(defun get-javascript-swagger ()
  (javascript-swagger-skeleton)
  )

(provide 'my-templates)
;;; my-templates.el ends here
