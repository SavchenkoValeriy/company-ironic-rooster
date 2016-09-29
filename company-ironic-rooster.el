;;; company-ironic-rooster.el --- company-mode completion back-end for ironic-rooster-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: convenience
;; Version: 0.1.2-cvs
;; URL: https://github.com/Sarcasm/company-ironic-rooster/
;; Package-Requires: ((emacs "24.1") (company "0.8.0") (ironic-rooster "0.2.0") (cl-lib "0.5"))

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

;; Usage:
;;
;;     (eval-after-load 'company
;;       '(add-to-list 'company-backends 'company-ironic-rooster))

;;; Code:

(require 'ironic-rooster-completion)

(require 'company)
(require 'company-template)

(require 'cl-lib)

(defgroup company-ironic-rooster nil
  "Company-mode completion back-end for Ironic-Rooster."
  :group 'company
  :group 'ironic-rooster)

(defcustom company-ironic-rooster-ignore-case nil
  "Non-nil to ignore case when collecting completion candidates."
  :type 'boolean)

(defsubst company-ironic-rooster--ironic-rooster-candidate (candidate)
  (get-text-property 0 'company-ironic-rooster candidate))

(defun company-ironic-rooster-prefix ()
  (let ((symbol-start (ironic-rooster-completion-beginning-of-symbol)))
    (if symbol-start
        (let ((prefix (buffer-substring-no-properties symbol-start (point))))
          (save-excursion
            (goto-char symbol-start)
            (if (ironic-rooster-completion-at-trigger-point-p)
                (cons prefix t)
              prefix)))
      'stop)))

(defun company-ironic-rooster--filter-candidates (prefix candidates)
  (cl-loop for candidate in candidates
           when (string-prefix-p prefix (car candidate)
                                 company-ironic-rooster-ignore-case)
           collect (propertize (car candidate) 'company-ironic-rooster candidate)))

(defun company-ironic-rooster--candidates-async (prefix callback)
  (funcall callback
           (company-ironic-rooster--filter-candidates prefix
                                             (ironic-rooster-completion-candidates))))

(defun company-ironic-rooster--candidates (prefix)
  (if (ironic-rooster-completion-candidates-available-p)
      (company-ironic-rooster--filter-candidates prefix (ironic-rooster-completion-candidates))
    (cons :async
          (lambda (callback)
            (ironic-rooster-completion-candidates-async
             (lambda () ;; closure, lexically bound
               (company-ironic-rooster--candidates-async prefix callback)))))))

(defun company-ironic-rooster--annotation (candidate)
  (concat
   (ironic-rooster-completion-annotation candidate)
   (let ((type (ironic-rooster-completion-type candidate)))
     (when (not (zerop (length type)))
       (concat " -> " type)))))

(defun company-ironic-rooster--post-completion (candidate)
  ;; This check is necessary because Company triggers a 'post-completion even if
  ;; the candidate has just been typed without relying on the completion, but it
  ;; doesn't provide the full candidate information.
  (when candidate
    (let ((point-before-post-complete (point)))
      (if (ironic-rooster-snippet-available-p)
          (ironic-rooster-completion-post-complete candidate)
        (let ((str (ironic-rooster-completion-post-comp-str candidate)))
          (insert str)
          (company-template-c-like-templatify str)))
      ;; Here we set this-command to a `self-insert-command' so that company may
      ;; retrigger idle completion after the snippet expansion
      ;; (~`company-post-command'). This is a bit of a hack and maybe that will
      ;; change in the future. This is useful for example when the completed
      ;; candidate is a namespace and the annotation text (inserted snippet) is
      ;; the scope operator.
      ;;
      ;; std| -> std::   (=> idle completion desired here)
      ;;         stderr
      ;;         ...
      ;;
      ;; See https://github.com/company-mode/company-mode/issues/143
      (unless (eq (point) point-before-post-complete)
        (setq this-command 'self-insert-command)))))

;;;###autoload
(defun company-ironic-rooster (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ironic-rooster))
    (prefix (and ironic-rooster-mode (company-ironic-rooster-prefix)))
    (candidates (company-ironic-rooster--candidates arg))
    (annotation (company-ironic-rooster--annotation
                 (company-ironic-rooster--ironic-rooster-candidate arg)))
    (meta (ironic-rooster-completion-brief
           (company-ironic-rooster--ironic-rooster-candidate arg)))
    (post-completion (company-ironic-rooster--post-completion
                      (company-ironic-rooster--ironic-rooster-candidate arg)))
    (ignore-case company-ironic-rooster-ignore-case)
    (sorted t)))

;;;###autoload
(defun company-ironic-rooster-setup-begin-commands ()
  "Include ironic-rooster trigger commands to `company-begin-commands'.

This allow completion to be automatically triggered after member
accesses (obj.|, obj->|, ...).

This may be useful to company < `0.8.4', newer version of company
include these commands by default."
  (if (listp company-begin-commands)
      (set (make-local-variable 'company-begin-commands)
           (delete-dups
            (append company-begin-commands ironic-rooster-completion-trigger-commands)))
    (display-warning 'company-ironic-rooster
                     "`company-ironic-rooster-setup-begin-commands' expects \
`company-begin-commands' to be a list!")))

(provide 'company-ironic-rooster)
;;; company-ironic-rooster.el ends here
