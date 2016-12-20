;;; cexp-test.el --- Test for cexp.el                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Tobias Zawada

;; Author: Tobias Zawada <naehring@smtp.1und1.de>
;; Keywords: lisp, matching

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

;; Test all functionality of `cexp-search-forward'.
;; Just load this package with `(load-library 'cexp-test)' and all tests will be executed.

;;; Code:
;; testcase1(first sexp [encapsulated sexp])
;; Some text between testcase1 and testcase2.
;; testcase2(only one sexp)

(require 'cexp)
(require 'cl-lib)

(find-library "cexp-test.el")

;; Test1: normal repeated search with noerror==t:
(let ((patterns '("(first sexp [encapsulated sexp])" "(only one sexp)")))
  (goto-char (point-min))
  (while (cexp-search-forward "\\<testcase[0-9]\\!(^(\\(.*\\))$\\!)" nil t)
    (cl-assert (string= (match-string 2) (car patterns)) "Test1: The sexps don't have the expected values")
    (setq patterns (cdr patterns)))
  (cl-assert (null patterns) "Test1: Not all patterns have been found."))

;; Test2: limit search to the text between testcase1 and testcase2:
(let ((patterns '("(first sexp [encapsulated sexp])"))
      (bound (progn (goto-char (point-min)) (search-forward "between testcase1 and testcase2"))))
  (goto-char (point-min))
  (while (cexp-search-forward "\\<testcase[0-9]\\!(^(\\(.*\\))$\\!)" bound t)
    (cl-assert (string= (match-string 2) (car patterns)) "Test1: The sexps don't have the expected values")
    (setq patterns (cdr patterns)))
  (cl-assert (null patterns) "Test2: Not all patterns have been found."))

;; Test3: Search text multiple times.
(let ((patterns '("(only one sexp)")))
  (goto-char (point-min))
  (while (cexp-search-forward "\\<testcase[0-9]\\!(^(\\(.*\\))$\\!)" nil t 2)
    (cl-assert (string= (match-string 2) (car patterns)) "Test1: The sexps don't have the expected values")
    (setq patterns (cdr patterns)))
  (cl-assert (null patterns) "Test1: Not all patterns have been found."))

(message "cexp-test done")

(provide 'cexp-test)
;;; cexp-test.el ends here
