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
  (push '(":PROPERTIES:" . ?) prettify-symbols-alist)

  ;; tags
  ;; (push '(":Misc:" . "" ) prettify-symbols-alist)

  (prettify-symbols-mode))


(defun my/save-and-close-this-buffer (buffer)
  "Saves and closes given buffer."
  (if (get-buffer buffer)
	  (let ((b (get-buffer buffer)))
		(save-buffer b)
		(kill-buffer b))))



(provide 'init-funcs)
