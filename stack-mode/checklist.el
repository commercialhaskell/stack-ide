;;; checklist.el --- Simple checklist UI functionality

;; Copyright (c) 2015 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar checklist-read-checklist-result nil)
(make-variable-buffer-local 'checklist-read-checklist-result)

(defun checklist-read-checklist (prompt choices)
  "Prompt with PROMPT to choose one or more from the list of CHOICES."
  (switch-to-buffer (generate-new-buffer-name "checklist"))
  (let ((inhibit-read-only t))
    (special-mode)
    (setq buffer-read-only t)
    (setq cursor-type 'box)
    (insert (concat prompt "\n\n"))
    (use-local-map (copy-keymap special-mode-map))
    (local-set-key "n" 'next-line)
    (local-set-key "p" 'previous-line)
    (local-set-key (kbd "RET") 'checklist-checklist-choose)
    (local-set-key (kbd "SPC") 'checklist-checklist-choose)
    (local-set-key (kbd "C-c C-c") 'checklist-checklist-done)
    (save-excursion
      (cl-loop for choice in choices
               do (insert (propertize (concat "[ ] " (cdr choice) "\n")
                                      'checklist-check-item (car choice))))
      (insert "\n")
      (insert "Hit " (propertize "C-c C-c" 'face 'bold) " to finish."))
    (add-hook 'post-command-hook #'checklist-highlight-section t t)
    (setq checklist-read-checklist-result nil)
    (recursive-edit)
    (let ((result  checklist-read-checklist-result)
          (kill-buffer-query-functions nil))
      (kill-buffer (current-buffer))
      result)))

(defun checklist-checklist-done ()
  "Done with the checklist, return the result."
  (interactive)
  (throw 'exit nil))

(defun checklist-highlight-section ()
  "Highlight the item at point."
  (remove-overlays)
  (when (get-text-property (point) 'checklist-check-item)
    (let ((o (make-overlay (line-beginning-position)
                           (1+ (line-end-position)))))
      (overlay-put o 'face 'secondary-selection))))

(defun checklist-checklist-choose ()
  "Choose the current item at point."
  (interactive)
  (let ((inhibit-read-only t)
        (checked (get-text-property (point) 'checklist-item-checked)))
    (when (get-text-property (point) 'checklist-check-item)
      (if checked
          (setq checklist-read-checklist-result
                (delete (get-text-property (point) 'checklist-check-item)
                        checklist-read-checklist-result))
          (add-to-list 'checklist-read-checklist-result
                       (get-text-property (point) 'checklist-check-item)
                       t))
      (save-excursion (goto-char (line-beginning-position))
                      (delete-region (line-beginning-position)
                                     (+ (line-beginning-position) (length "[ ]")))
                      (if checked
                          (insert "[ ]")
                        (insert "[x]")))
      (put-text-property (line-beginning-position)
                         (1+ (line-end-position))
                         'checklist-item-checked (not checked))
      (put-text-property (line-beginning-position)
                         (1+ (line-end-position))
                         'face (if checked nil 'bold))
      (put-text-property (line-beginning-position)
                         (1+ (line-end-position))
                         'checklist-check-item t)
      (checklist-highlight-section))))

(provide 'checklist)
