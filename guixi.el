;;; guixi.el --- Emacs integration for Guix  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/ROCKTAKEY/guixi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs integration for Guix

;;; Code:

(defgroup guixi ()
  "Emacs integration for Guix."
  :group 'tools
  :prefix "guixi-"
  :link '(url-link "https://github.com/ROCKTAKEY/guixi"))

(define-derived-mode guixi-package-list-mode tabulated-list-mode
  "Guixi Package List"
  :group 'guixi
  :interactive nil
  (setq tabulated-list-format
              `[("Name" 30)
                ("Version" 20)
                ("Outputs" 20)
                ("Definition" 10)])
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  ;; (setq revert-buffer-function #'guixi-refresh-contents)
  )

(defun guixi-recutils-parse (input)
  "Parse string INPUT written in recutils into alist."
  (let* ((normalized-input (string-replace "\\\n" "" input))
         (lines (split-string normalized-input "\n")))
    (nreverse
     (seq-reduce
      (lambda (alist arg)
        (cond
         ((string-match "^\\+ ?\\(.*\\)" arg)
          (let ((line (match-string 1 arg)))
            (setf (cdar alist) (concat (cdar alist) "\n" line))
            alist))
         ((string-match "^\\([a-zA-Z%][a-zA-Z0-9_]*\\): ?\\(.*\\)" arg)
          `((,(match-string 1 arg) . ,(match-string 2 arg))
            ,@alist))
         (t alist)))
      lines nil))))

(defun guixi-package-describe-package (package-name version)
  "Describe Guix package named PACKAGE-NAME on version VERSION."
  (interactive
   (let ((id (tabulated-list-get-id)))
     (list (car id)
           (cdr id)))
   guixi-package-list-mode)
  (let ((buffer (get-buffer-create "*Guixi Package Help*")))
    (with-current-buffer buffer
      (insert (shell-command-to-string (concat "guix show " package-name "@" version))))
    (pop-to-buffer buffer)))



(defun guixi-package-list ()
  "Show a list of Guix packages."
  (interactive)
  (let ((buffer (get-buffer-create "*Guixi Package List*")))
    (with-current-buffer buffer
      (setq tabulated-list-entries
            (mapcar
             (lambda (arg)
               (list (cons (nth 0 arg) (nth 1 arg))
                     (vconcat arg)))
             (mapcar
              (lambda (arg) (split-string arg "\t" nil " *"))
               (split-string (shell-command-to-string "guix package -A") "\n" t))))
      (guixi-package-list-mode)
      (tabulated-list-print))
    (pop-to-buffer-same-window buffer)))

(provide 'guixi)
;;; guixi.el ends here
