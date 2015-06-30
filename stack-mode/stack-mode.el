;;; stack-mode.el --- A minor mode enabling various features based on
;;; stack-ide.

;; Copyright (c) 2015 Chris Done.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require 'haskell-mode)
(require 'cl-lib)
(require 'fifo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes

(define-minor-mode stack-mode
  "A minor mode enabling various features based on stack-ide."
  :lighter " Stack-IDE"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-.") 'stack-mode-goto)
            (define-key map (kbd "C-c C-k") 'stack-mode-clear)
            (define-key map (kbd "C-c C-t") 'stack-mode-type)
            (define-key map (kbd "C-c C-l") 'stack-mode-load)
            map))

(define-derived-mode inferior-stack-mode fundamental-mode "Inferior-Stack-IDE"
  "Major mode for interacting with an inferior stack-ide process.")

(define-key inferior-stack-mode-map (kbd "C-c C-c") 'stack-mode-stop)
(define-key inferior-stack-mode-map (kbd "C-c C-k") 'stack-mode-clear)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup stack-mode nil
  "IDE backend support for Haskell."
  :group 'haskell)

(defcustom stack-mode-proc-path
  "stack"
  "Path to the stack executable."
  :type 'string
  :group 'stack-mode)

(defvar stack-mode-queue nil)
(make-variable-buffer-local 'stack-mode-queue)

(defvar stack-mode-buffer nil)
(make-variable-buffer-local 'stack-mode-buffer)

(defvar stack-mode-name nil)
(make-variable-buffer-local 'stack-mode-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

(defun stack-mode-start ()
  "Start an inferior process and buffer."
  (interactive)
  (if (and (stack-mode-process)
           (process-live-p (stack-mode-process)))
      (switch-to-buffer (stack-mode-buffer))
    (let ((default-directory (stack-mode-dir)))
      (with-current-buffer (stack-mode-buffer)
        (setq buffer-read-only t)
        (cd (stack-mode-dir))
        (setq stack-mode-queue (fifo-make))
        (setq stack-mode-current-command nil)
        (setq stack-mode-buffer "")
        (stack-mode-log "Starting: stack ide")
        (let* ((name (stack-mode-process-name (stack-mode-name)))
               (process (or (get-process name)
                            (apply #'start-process
                                   (append (list name
                                                 nil
                                                 stack-mode-proc-path
                                                 "ide")
                                           (split-string (read-from-minibuffer "Targets: ")
                                                         " "))))))
          (set-process-sentinel process 'stack-mode-sentinel)
          (set-process-filter process 'stack-mode-filter))
        (inferior-stack-mode)))))

(defun stack-mode-stop ()
  "Stop the process."
  (interactive)
  (with-current-buffer (stack-mode-buffer)
    (when (stack-mode-process)
      (setq stack-mode-current-command nil)
      (setq stack-mode-buffer "")
      (kill-process (stack-mode-process))
      (delete-process (stack-mode-process)))))

(defun stack-mode-restart ()
  "Restart the process with a fresh command queue."
  (interactive)
  (stack-mode-stop)
  (stack-mode-start))

(defun stack-mode-clear ()
  "Clear the interaction buffer."
  (interactive)
  (with-current-buffer (stack-mode-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun stack-mode-load ()
  "Load the current buffer's file."
  (interactive)
  (save-buffer)
  (let ((filename (buffer-file-name)))
    (with-current-buffer (stack-mode-buffer)
      (stack-mode-update-file
       (file-relative-name filename default-directory)))))

(defun stack-mode-goto ()
  "Go to definition of thing at point."
  (interactive)
  (let ((filename (buffer-file-name))
        (module-name (haskell-guess-module-name))
        (span (stack-mode-span)))
    (let* ((infos
            (stack-contents
             (stack-mode-get-span-info
              module-name
              (with-current-buffer (stack-mode-buffer)
                (file-relative-name filename default-directory))
              span)))
           (parts (mapcar #'identity (elt infos 0)))
           (info (stack-contents (elt parts 0)))
           (span (elt parts 1))
           (scope (stack-lookup 'tag (stack-lookup 'idScope info)))
           (def-span (stack-lookup-contents
                      'idDefSpan
                      (stack-lookup 'idProp info))))
      (cond
       ((listp def-span)
        (stack-mode-goto-span def-span))
       (t
        (let* ((imported-from
                (stack-lookup
                 'idImportedFrom
                 (stack-lookup 'idScope info)))
               (imported-module (stack-lookup 'moduleName imported-from))
               (defined-in (stack-lookup
                            'idDefinedIn
                            (stack-lookup 'idProp info)))
               (package (stack-lookup 'modulePackage defined-in))
               (package-name (stack-lookup 'packageName package))
               (package-ver (stack-lookup 'packageVersion package))
               (module (stack-lookup 'moduleName defined-in)))
          (message "Imported via %s, defined in %s (%s-%s)"
                   (haskell-fontify-as-mode imported-module 'haskell-mode)
                   (haskell-fontify-as-mode module 'haskell-mode)
                   package-name
                   package-ver)))))))

(defun haskell-mode-show-type-at (&optional insert-value)
  "Show the type of the thing at point."
  (interactive "P")
  (let ((ty (haskell-mode-type-at)))
    (if insert-value
        (message "%s" (haskell-fontify-as-mode ty 'haskell-mode)))))

(defun stack-mode-type (&optional insert-value)
  "Display type info of thing at point."
  (interactive "P")
  (let* ((filename (buffer-file-name))
         (module-name (haskell-guess-module-name))
         (points (stack-mode-points))
         (orig (point))
         (span (stack-mode-span-from-points (car points)
                                            (cdr points))))
    (let* ((types (stack-contents
                   (stack-mode-get-exp-types
                    module-name
                    (with-current-buffer (stack-mode-buffer)
                      (file-relative-name filename default-directory))
                    span)))
           (types (mapcar #'identity types))
           (code (buffer-substring-no-properties
                  (car points)
                  (cdr points)))
           (type (stack-contents (car types)))
           (ty (stack-lookup 'text type)))
      (if insert-value
          (let ((ident-pos (haskell-ident-pos-at-point)))
            (cond
             ((region-active-p)
              (delete-region (region-beginning)
                             (region-end))
              (insert "(" code " :: " ty ")")
              (goto-char (1+ orig)))
             ((= (line-beginning-position) (car ident-pos))
              (goto-char (line-beginning-position))
              (insert code " :: " (haskell-fontify-as-mode ty 'haskell-mode)
                      "\n"))
             (t
              (save-excursion
                (goto-char (car ident-pos))
                (let ((col (current-column)))
                  (save-excursion (insert "\n")
                                  (indent-to col))
                  (insert code " :: " (haskell-fontify-as-mode ty 'haskell-mode)))))))
        (unless (null types)
          (message
           "%s"
           (mapconcat (lambda (type)
                        (haskell-fontify-as-mode
                         (concat
                          code
                          " :: "
                          (elt type 0))
                         'haskell-mode))
                      (subseq types 0 1)
                      "\n")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process filters and sentinel

(defun stack-mode-filter (process response)
  (with-current-buffer (stack-mode-buffer (stack-mode-name-from-process process))
    (if stack-mode-current-command
        (let* ((lines (split-string (concat stack-mode-buffer response) "\n")))
          (setq stack-mode-buffer (car (last lines)))
          (setq lines (butlast lines))
          (let ((data (plist-get stack-mode-current-command :data))
                (cont (plist-get stack-mode-current-command :cont)))
            (while lines
              (let ((line (pop lines)))
                (stack-mode-log
                 "<- %s"
                 (haskell-fontify-as-mode line 'javascript-mode))
                (when (let* ((error-msg nil)
                             (ret (condition-case e
                                      (funcall cont data (json-read-from-string line))
                                    (error (setq error-msg e)
                                           :error))))
                        (ecase ret
                          (:done t)
                          (:continue nil)
                          (:error
                           (setq stack-mode-buffer "")
                           (setq stack-mode-current-command nil)
                           (setq stack-mode-queue nil)
                           (error "Command handler error: %S\n\nThe command queue has been cleared."
                                  error-msg))
                          (t
                           (error "A command handler must return either :done or :continue,
but it returned: %S
command was: %S" ret stack-mode-current-command))))
                  (cl-loop for line in lines
                           do (stack-mode-log
                               "Extraneous lines after command completed: %s"
                               (haskell-fontify-as-mode line 'javascript-mode)))
                  (setq stack-mode-current-command nil)
                  (setq lines nil)
                  (stack-mode-queue-trigger))))))
      (stack-mode-log "Ignoring: %s"
                      (haskell-fontify-as-mode response 'javascript-mode)))))

(defun stack-mode-sentinel (process event)
  (with-current-buffer (stack-mode-buffer (stack-mode-name-from-process process))
    (stack-mode-log "Process event: %s" event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command queue

(defvar stack-mode-current-command nil
  "Current command handler.")
(make-variable-buffer-local 'stack-mode-current-command)

(defvar stack-mode-buffer ""
  "A buffer for the process.")
(make-variable-buffer-local 'stack-mode-buffer)

(defvar stack-mode-queue nil
  "Command queue.")
(make-variable-buffer-local 'stack-mode-queue)

(defun stack-mode-queue ()
  "Get the FIFO queue of this process."
  (or stack-mode-queue
      (setq stack-mode-queue (fifo-make))))

(defun stack-mode-enqueue (json data cont)
  "Enqueue a JSON command to the command queue, calling (CONT
DATA line) for each response line until CONT returns nil."
  (stack-mode-log "-> %s" (haskell-fontify-as-mode (json-encode json) 'javascript-mode))
  (fifo-push (stack-mode-queue)
             (list :json json :data data :cont cont))
  (stack-mode-queue-trigger))

(defun stack-mode-call (json)
  "Call a JSON command. Wait for any existing queued commands to
complete, then sends the request, blocking on the
response. Returns the response."
  (let ((data (list nil)))
    (stack-mode-enqueue
     json data
     (lambda (data reply)
       (setcar data reply)
       :done))
    (stack-mode-queue-flush)
    (car-safe data)))

(defun stack-mode-queue-processed-p ()
  "Return t if command queue has been completely processed."
  (and (fifo-null-p stack-mode-queue)
       (null stack-mode-current-command)))

(defun stack-mode-queue-flush ()
  "Block till PROCESS's command queue has been completely processed.
This uses `accept-process-output' internally."
  (let ((proc (stack-mode-process)))
    (while (not (stack-mode-queue-processed-p))
      (stack-mode-queue-trigger)
      (accept-process-output proc 1))))

(defun stack-mode-queue-trigger ()
  "Trigger the next command in the queue if there is no current
command."
  (unless stack-mode-current-command
    (unless (fifo-null-p (stack-mode-queue))
      (setq stack-mode-current-command
            (fifo-pop (stack-mode-queue)))
      (process-send-string
       (stack-mode-process)
       (concat (json-encode (plist-get stack-mode-current-command :json))
               "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project functions

(defun stack-mode-process ()
  "Get the current process."
  (get-process (stack-mode-process-name (stack-mode-name))))

(defun stack-mode-buffer (&optional name)
  "The inferior buffer."
  (get-buffer-create
   (stack-mode-buffer-name
    (or name
        (stack-mode-name)))))

(defun stack-mode-name-from-process (proc)
  "Get the name of the project from the process."
  (substring (process-name proc) (length "stack:")))

(defun stack-mode-process-name (name)
  "Name for the inferior process."
  (format "stack:%s"
          name))

(defun stack-mode-buffer-name (name)
  "Name for the inferior buffer."
  (format "*stack:%s*"
          name))

(defun stack-mode-dir ()
  "The directory for the project."
  (file-name-directory (stack-yaml-file)))

(defun stack-yaml-file ()
  "Get the .yaml file path."
  (shell-command-to-string "stack path --config-location"))

(defun stack-mode-name ()
  "The name for the current project based on the current
directory."
  (or stack-mode-name
      (setq stack-mode-name
            (downcase
             (file-name-nondirectory
              (directory-file-name
               (file-name-directory
                (stack-yaml-file))))))))

(defun stack-mode-log (&rest args)
  "Log a string to the inferior buffer."
  (with-current-buffer (stack-mode-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (apply #'format args)
              "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun stack-mode-update-file (filepath)
  "Load the given filepath."
  (with-current-buffer (stack-mode-buffer)
    (stack-mode-enqueue
     `((request . "UpdateSession")
       (update . (,(stack-mode-list->hashtable
                    `((update . "updateSourceFileFromFile")
                      (filePath . ,filepath))))))
     nil
     'stack-mode-loading-callback)))

(defun stack-mode-get-span-info (module file span)
  "Get the span info of the given location."
  (with-current-buffer (stack-mode-buffer)
    (stack-mode-call
     `((tag . "RequestGetSpanInfo")
       (contents
        . ((spanFilePath   . ,file)
           (spanFromLine   . ,(plist-get span :sl))
           (spanFromColumn . ,(plist-get span :sc))
           (spanToLine     . ,(plist-get span :el))
           (spanToColumn   . ,(plist-get span :ec))))))))

(defun stack-mode-get-exp-types (module file span)
  "Get the type info of the given location."
  (with-current-buffer (stack-mode-buffer)
    (stack-mode-call
     `((tag . "RequestGetExpTypes")
       (contents
        . ((spanFilePath   . ,file)
           (spanFromLine   . ,(plist-get span :sl))
           (spanFromColumn . ,(plist-get span :sc))
           (spanToLine     . ,(plist-get span :el))
           (spanToColumn   . ,(plist-get span :ec))))))))

(defun stack-mode-get-use-sites (module file span)
  "Get all uses of an identifier."
  )

(defun stack-mode-get-completions (module string)
  "Get all uses of an identifier."
  )

(defun stack-mode-loading-callback (_ reply)
  "Callback for when loading modules."
  (cond
   ((assoc 'progress reply)
    (let ((msg (stack-lookup 'parsedMsg (assoc 'progress reply)))
          (step (stack-lookup 'step (assoc 'progress reply)))
          (steps (stack-lookup 'numSteps (assoc 'progress reply))))
      (message "%s %s"
               (propertize msg 'face 'bold)
               (propertize (format "(%d of %d)" step steps)
                           'face 'font-lock-comment-face)))
    :continue)
   (t
    (stack-mode-enqueue
     `((request . "GetSourceErrors"))
     nil
     'stack-mode-get-source-errors-callback)
    :done)))

(defun stack-mode-get-source-errors-callback (_ reply)
  "Handle the reply from getting source errors."
  (let ((any-errors nil)
        (warnings 0))
    (cl-loop
     for item in (mapcar #'identity (stack-lookup 'errors reply))
     do (let* ((msg (stack-lookup 'msg item))
               (kind (stack-lookup 'kind item))
               (span (stack-lookup 'span item))
               (fp (stack-lookup 'filePath span))
               (sl (stack-lookup 'fromLine span))
               (sc (stack-lookup 'fromColumn span))
               (el (stack-lookup 'toLine span))
               (ec (stack-lookup 'toColumn span)))
          (cond ((string= kind "error")
                 (setq any-errors t))
                ((string= kind "warning")
                 (setq warnings (1+ warnings))))
          (message "%s"
                   (propertize
                    (format "%s:(%d,%d)-(%d,%d): \n%s"
                            fp sl sc el ec msg)
                    'face
                    (cond
                     ((string= kind "warning")
                      'compilation-warning)
                     ((string= kind "error")
                      'compilation-error)
                     (t nil))))))
    (unless any-errors
      (if (= 0 warnings)
          (message "OK.")
        (message (propertize "OK (%d warning%s)." 'face 'compilation-warning)
                 warnings
                 (if (= 1 warnings) "" "s")))))
  :done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Span functions

(defun stack-mode-points ()
  "Get the current points; either a selected region or an
identifier's points."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (let ((ident (haskell-ident-pos-at-point)))
      (cons (car ident)
            (cdr ident)))))

(defun stack-mode-span-from-points (beg end)
  "Get the span representation for the span from BEG to END."
  (save-excursion
    (list :sl (progn (goto-char beg)
                     (line-number-at-pos))
          :sc (1+ (current-column))
          :el (progn (goto-char end)
                     (line-number-at-pos))
          :ec (1+ (current-column)))))

(defun stack-mode-span ()
  "Get the span from the haskell points."
  (let ((points (or (haskell-spanable-pos-at-point)
                    (haskell-ident-pos-at-point)
                    (stack-mode-loose-ident-at-point))))
    (if points
        (stack-mode-span-from-points (car points) (cdr points))
      (error "No identifier at point."))))

(defun stack-mode-goto-span (span)
  "Get buffer points from a span."
  (with-current-buffer (stack-mode-buffer)
    (find-file (stack-lookup 'spanFilePath span))
    (goto-char (point-min))
    (let ((beg (point)))
      (goto-char (point-min))
      (forward-line (1- (stack-lookup 'spanFromLine span)))
      (goto-char (line-beginning-position))
      (forward-char (1- (stack-lookup 'spanFromColumn span))))))

(defun stack-mode-loose-ident-at-point ()
  "Get the loose ident at point."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON helpers

(defun stack-mode-list->hashtable (xs)
  "Convert a list to a hashtable."
  (let ((h (make-hash-table)))
    (cl-loop for (key . val)
             in xs
             do (puthash key val h))
    h))

(defun stack-lookup (key object)
  "Get from a JSON object."
  (cdr (assoc key (mapcar #'identity object))))

(defun stack-contents (object)
  "Get from a JSON object."
  (stack-lookup 'contents object))

(defun stack-lookup-contents (key object)
  "Get from a JSON object."
  (stack-contents (stack-lookup key object)))

(provide 'stack-mode)
