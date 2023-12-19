;;; init-funcs.el --- core functions -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun +rename-current-file (newname)
  "Rename current visiting file to NEWNAME.
If NEWNAME is a directory, move file to it."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (let ((name (read-file-name "Rename to: " nil buffer-file-name 'confirm)))
       (when (equal (file-truename name)
                    (file-truename buffer-file-name))
         (user-error "Can't rename file to itself"))
       (list name))))
  ;; NEWNAME is a directory
  (when (equal newname (file-name-as-directory newname))
    (setq newname (concat newname (file-name-nondirectory buffer-file-name))))
  (rename-file buffer-file-name newname)
  (set-visited-file-name newname)
  (rename-buffer newname))

;;;###autoload
(defun +delete-current-file (file)
  "Delete current visiting FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (when (y-or-n-p (format "Really delete '%s'? " file))
    (kill-this-buffer)
    (delete-file file)))

;;;###autoload
(defun +copy-current-file (new-path &optional overwrite-p)
  "Copy current buffer's file to `NEW-PATH'.
If `OVERWRITE-P', overwrite the destination file without
confirmation."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (list (read-file-name "Copy file to: ")
           current-prefix-arg)))
  (let ((old-path (buffer-file-name))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) t)
    (copy-file old-path new-path (or overwrite-p 1))))

;;;###autoload
(defun +copy-current-filename (file)
  "Copy the full path to the current FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (kill-new file)
  (message "Copying '%s' to clipboard" file))

;;;###autoload
(defun +copy-current-buffer-name ()
  "Copy the name of current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "Copying '%s' to clipboard" (buffer-name)))

;;;###autoload
(defun +transient-tab-bar-history ()
  "Transient map of command `tab-bar-history'."
  (interactive)
  (let ((echo-keystrokes nil))
    (tab-bar-history-back)
    (message "tab-bar-history: [u]back [r]forward")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map "u" #'tab-bar-history-back)
       (define-key map "r" #'tab-bar-history-forward)
       map)
     t)))

;;;###autoload
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all sub-entries are done, to TODO otherwise."
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;;BUFFER-MOVE
(require 'windmove)
;;;###autoload
(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
;;  "Switches between the current buffer, and the buffer above the
;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-down ()
"Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-left ()
"Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-right ()
"Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))


(defun my/system-clipboard-to-emacs-clipboard ()
  "Set Emacs kill ring to contents of system clipboard."
  (interactive)
  (kill-new (simpleclip-get-contents)))


(defvar my-default-line-spacing 0 "Baseline line spacing.")
(setq-default my-default-line-spacing 0)
(defun my/org-setup ()
  (org-indent-mode) ;; Keeps org items like text under headings, lists, nicely indented
  (visual-line-mode 1) ;; Nice line wrapping
  (centered-cursor-mode) ;; Enable centered cursor mode
  (hl-prog-extra-mode) ;; Highlighting with regexps
  (setq-local line-spacing (+ my-default-line-spacing 2)) ;; A bit more line spacing for orgmode
  (valign-mode)
  )

(defun my/prettify-symbols-setup ()
  ;; checkboxes
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  ;; (push '("[X]" . "☒" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)

  ;; org-babel
  (push '("#+BEGIN_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+END_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+begin_src" . ?≫) prettify-symbols-alist)
  (push '("#+end_src" . ?≫) prettify-symbols-alist)

  (push '("#+BEGIN_QUOTE" . ?❝) prettify-symbols-alist)
  (push '("#+END_QUOTE" . ?❞) prettify-symbols-alist)

  ;; (push '("#+BEGIN_SRC python" . ) prettify-symbols-alist) ;; This is the Python symbol. Comes up weird for some reason
  (push '("#+RESULTS:" . ?≚ ) prettify-symbols-alist)

  ;; drawers
  ; (push '(":PROPERTIES:" . ?) prettify-symbols-alist)

  ;; tags
  ;; (push '(":Misc:" . "" ) prettify-symbols-alist)

  (prettify-symbols-mode))


(defun my/save-and-close-this-buffer (buffer)
  "Saves and closes given buffer."
  (if (get-buffer buffer)
	  (let ((b (get-buffer buffer)))
		(save-buffer b)
		(kill-buffer b))))


;; Auto-display agenda
(defun my/jump-to-org-agenda ()
  "Create and jump to the my org agenda."
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer))))
      (my/org-agenda-with-tip nil))))

(defun my/idle-agenda (&optional arg)
  "Set or cancel idle agenda timer based on [ARG]."
  (interactive "P")
  (setq my/iagenda
        (if arg
            (cancel-timer my/iagenda)
          (run-with-idle-timer 3600 t 'my/jump-to-org-agenda))))


;;Weekly Score Goal in Org-Agenda
(defvar my/weekly-score-goal 42) ;; Define my goal to hit
;; Add up all the scores from DONE items in the agenda files
(defun my/agenda-score-goal ()
  "Add up scores from done items.

     In the agenda, this will show the number of done items and the
     target goal from `my/weekly-score-goal`."
  (let* ((score ;; Add up all scores from DONE items
          (apply '+
                 (org-map-entries
                  (lambda () (string-to-number (or (org-entry-get (point) "Score") "0")))
                  "/DONE" 'agenda)))
         (scaled-goal (* my/weekly-score-goal
                         (/ (string-to-number (format-time-string "%w"))
                            5.0)))
         (face (cond ((>= score scaled-goal) 'success)
                     ((>= score (* .8 scaled-goal)) 'warning)
                     (t 'error)))
         (goal-label (format "✧ Score Goal (%d): " scaled-goal))
         (goal-metric (format "%d/%d\n" score my/weekly-score-goal))
         (header-size (+ (string-width goal-label)
                         (string-width goal-metric)))
         (goal-separator (concat (make-string header-size ?┄) "\n")))
    (insert
     (concat
      (propertize goal-label 'face 'org-agenda-structure)
      (propertize goal-metric 'face face)
      (propertize goal-separator 'face 'org-agenda-structure)))))


;;Weekly Time Reporting
(defun my/find-year-create (year)
  "Find or create a [YEAR] in an Org journal."
  (let ((re "^\\**[ \t]+\\([12][0-9]\\{3\\}\\)")
        match)
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (while (and (setq match (re-search-forward re nil t))
                (goto-char (match-beginning 1))
                (< (string-to-number (match-string 1)) year)))
    (cond
     ((not match)
      (goto-char (point-max))
      (or (bolp) (newline))
      (insert (format  "** %s\n" year)))
     ((= (string-to-number (match-string 1)) year)
      (goto-char (point-at-bol)))
     (t
      (beginning-of-line 1)
      (insert (format  "** %s\n" year))))))

(defun my/find-ww-create (ww)
  "Find or create a [WW] (workweek) in an Org journal."
  (let ((re "^\\**[ \t]+\\WW\\([0-9]\\{2\\}\\)")
        match)
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (while (and (setq match (re-search-forward re nil t))
                (goto-char (match-beginning 1))
                (< (string-to-number (match-string 1)) ww)))
    (cond
     ((not match)
      (goto-char (point-max))
      (or (bolp) (newline))
      (insert (format "*** WW%02d\n" ww)))
     ((= (string-to-number (match-string 1)) ww)
      (goto-char (point-at-bol)))
     (t
      (beginning-of-line 1)
      (insert (format "*** WW%02d\n" ww))))))

(defun my/insert-weekly-time-sheet ()
  "Generated and insert a weekly time sheet generated from the default Org Agenda."
  (with-temp-buffer
    (insert
     (concat  "#+BEGIN: clocktable :maxlevel 3 :scope agenda-with-archives :block lastweek :fileskip0 t :properties (\"Score\") :indent nil \n"
              "#+TBLFM: $6='(org-clock-time% @2$4 $3..$5);%.1f::@2$2=vsum(@3$2..@>$2)\n"
              "#+END:\n\n"))
    (goto-char (point-min))
    (org-update-dblock)
    (buffer-substring (point-min) (point-max))))

(defun my/insert-weekly-clocking ()
  "Insert the weekly clocking clocking data."
  (let ((year (number-to-string (nth 2 (calendar-gregorian-from-absolute (org-today)))))
        (ww (my/workweek)))
    (goto-char (point-min))
    (goto-char (cdr (org-id-find "clocking")))
    (my/find-year-create (string-to-number year))
    (my/find-ww-create ww)))

;;Workweeks
(defun my/workweek ()
  "Return the current workweek number."
  (interactive)
  (let* ((now (current-time))
         (weeks (string-to-number
                 (format-time-string "%W" now)))
         (days (time-to-day-in-year now))
         (daynum (string-to-number
                  (format-time-string "%w" now)))
         (left (% days 7)))
    (if (and (= 0 daynum) (= left 0))
        weeks
      (+ 1 weeks))))

(defun my/workweek-string ()
  "Convert the current workweek into a string.

The string is of the format WW##."
  (interactive)
  (concat "WW"
          (number-to-string
           (my/workweek))))

(require 'calendar)
(defun my/workweek-from-gregorian (&optional date)
  "Calculate the workweek from the Gregorian calendar."
  (let* ((date (or date (calendar-current-date)))
         (year (calendar-extract-year date))
         (fst (calendar-day-of-week (list 1 1 year)))
         (x   (if (>= fst 4)1 0)))
    (+ x
       (car
        (calendar-iso-from-absolute
         (calendar-absolute-from-gregorian date))))))

(setq calendar-week-start-day 1
      calendar-intermonth-text
      '(propertize
        (format "%2d"
                (my/workweek-from-gregorian (list month day year)))
        'font-lock-face 'font-lock-function-name-face))

;;Capture
(defun my/org-count-tasks-by-status ()
  "Create a table entry for the tracking of task mix."
  (interactive)
  (let ((counts (make-hash-table :test 'equal))
        (today (format-time-string "%Y-%m-%d" (current-time)))
        values output)
    (org-map-entries
     (lambda ()
       (let ((status (elt (org-heading-components) 2)))
         (when status
           (puthash status (1+ (or (gethash status counts) 0)) counts))))
     "-HOME"
     'agenda)
    (setq values (mapcar (lambda (x)
                           (or (gethash x counts) 0))
                         '("DONE" "TODO" "INPROG" "CANCELLED" "SOMEDAY")))
    (setq output
          (concat "| " today " | "
                  (mapconcat 'number-to-string values " | ")
                  " | "
                  (number-to-string (apply '+ values))
                  " | "
                  (number-to-string
                   (round (/ (* 100.0 (car values)) (apply '+ values))))
                  "% |"))
    (if (called-interactively-p 'any)
        (insert output)
      output)))

(defun my/add-weekly-score-table-entry ()
  "Track my weekly scores in a table."
  (let ((score (apply
                '+
                (org-map-entries
                 (lambda ()
                   (string-to-number (or (org-entry-get (point) "Score") "0")))
                 "/DONE"
                 'agenda)))
        (year (format-time-string "%Y" (current-time)))
        (ww (number-to-string (my/workweek))))
    (format "| %s | %s | %s |" year ww score)))

(defun my/org-time-logged-table-entry (&optional additional-weeks)
  "Insert table of minutes per category.
  Optionally provide ADDITIONAL-WEEKS to get more history"
  (interactive "P")
  (unless additional-weeks (setq additional-weeks 0))
  (let* ((minh (make-hash-table :test 'equal))
         (now (decode-time))
         (start (encode-time 0 0 0 (- (nth 3 now) (nth 6 now) (* 7 (or additional-weeks 1))) (nth 4 now) (nth 5 now)))
         (today (format-time-string "%Y-%m-%d" (current-time))))
    ;; Collect minutes clocked per category
    (org-map-entries
     (lambda ()
       (let ((category (org-entry-get-with-inheritance "CATEGORY" t))
             (minutes (org-clock-sum-current-item start)))
         (puthash category (+ minutes
                              (or (gethash category minh) 0)) minh)))
     "LEVEL=1"
     'agenda)
    ;; Print out table lines
    (let ((rows nil))
      (maphash
       (lambda (k v)
         (when (> v 0)
           (setq rows
                 (cons (format "| %s | %s | %d |" today k v) rows))))
       minh)
      (if (called-interactively-p 'any)
          (insert (mapconcat 'identity rows "\n"))
        (mapconcat 'identity rows "\n")))))

(defun my/org-agenda-with-tip (arg)
  "Show agenda for ARG days."
  (org-agenda-list arg)
  (let ((inhibit-read-only t)
        (pos (point)))
    (goto-char (point-max))
    (goto-char pos)))

(provide 'init-funcs)
