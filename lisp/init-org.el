;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'init-macros)

;; src block
(require 'org-tempo)

;; org-contrib
(use-package org-contrib
  :ensure t
)
(require 'org-checklist)

; Disable Electric Indent
(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(use-package org-super-agenda
  :defer
  :ensure t
  :after org
  :config
  (setq org-super-agenda-header-map nil) ;; takes over 'j'
  ;; (setq org-super-agenda-header-prefix " ◦ ") ;; There are some unicode "THIN SPACE"s after the ◦
  ;; Hide the thin width char glyph. This is dramatic but lets me not be annoyed
  (add-hook 'org-agenda-mode-hook
            #'(lambda () (setq-local nobreak-char-display nil)))

  (org-super-agenda-mode t)
  (add-to-list 'org-agenda-custom-commands
               '("rag" "Grouped Tasks"
                 ((todo "" ((org-super-agenda-groups
                             '((:name "All Tasks" :auto-category t))))))))
  (add-to-list 'org-agenda-custom-commands
               '("f1" "Score 1 Tasks"
                 ((tags "+Score=1" ((org-super-agenda-groups
                                     '((:name "Score 1 Tasks" :auto-category t))))))))
  (add-to-list 'org-agenda-custom-commands
               '("f2" "Score 2 Tasks"
                 ((tags "+Score=2" ((org-super-agenda-groups
                                     '((:name "Score 1 Tasks" :auto-category t))))))))
  (add-to-list 'org-agenda-custom-commands
               '("f3" "Score 3 Tasks"
                 ((tags "+Score=3" ((org-super-agenda-groups
                                     '((:name "Score 1 Tasks" :auto-category t))))))))
  (add-to-list 'org-agenda-custom-commands
               '("f5" "Score 5 Tasks"
                 ((tags "+Score=5" ((org-super-agenda-groups
                                     '((:name "Score 1 Tasks" :auto-category t))))))))
  (add-to-list 'org-agenda-custom-commands
               '("f8" "Score 8 Tasks"
                 ((tags "+Score=8" ((org-super-agenda-groups
                                     '((:name "Score 1 Tasks" :auto-category t))))))))
)

(use-package org-superstar
  :ensure t
  :config
    (setq org-agenda-files (file-expand-wildcards "~/Org/*.org"))
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("WAITING" . 9744)
                                          ("INPROG" . 9744)
                                          ("SOMEDAY" . 9744)
                                          ("CANCELLED" . 9744)
                                          ("DONE" . 9745)))
  ;; :hook (org-mode . org-superstar-mode)
)
;; Removes gap when you add a new heading
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
;;  inserting a new heading, do so after the current subtree.
(setq org-insert-heading-respect-content t)
(setq org-adapt-indentation t)

(use-package org-bullets
  :ensure t
  :custom
  (org-bullets-bullet-list '("◉" "✸" "○" "✿" "✤" "✜" "◆" "▶"))
  (org-ellipsis " ▼") ;; 设置标题行折叠符号
  ;; ⤵ ▼ ⬎  
  :hook (org-mode . org-bullets-mode))


(use-package evil-org
    :ensure t
    :diminish evil-org-mode
    :after org
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
             (lambda () (evil-org-set-key-theme)))
)

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)


;; sync with google calendar
; (use-package org-gcal
;   :ensure t
;   :defer t
;   :config
;   (setq org-gcal-down-days '20					;; Only fetch events 20 days into the future
;           org-gcal-up-days '10					;; Only fetch events 10 days into the past
;           org-gcal-recurring-events-mode 'top-level
;           org-gcal-remove-api-cancelled-events t) ;; No prompt when deleting removed events
; )

(use-package org-appear
    :ensure t
    :commands (org-appear-mode)
    :hook (org-mode . org-appear-mode)
    :init
    (setq org-hide-emphasis-markers t)		;; A default setting that needs to be t for org-appear
    (setq org-appear-autoemphasis t)		;; Enable org-appear on emphasis (bold, italics, etc)
    (setq org-appear-autolinks nil)		;; Don't enable on links
    (setq org-appear-autosubmarkers t);; Enable on subscript and superscript
)


(use-package ox-reveal
    :ensure t
    :defer 5
)

(eval-after-load 'org
  '(org-load-modules-maybe t))

(use-package org-preview-html
    :ensure t
  :defer t
  :config
  (setq org-preview-html-viewer 'xwidget)
)

(use-package org-fragtog
    :ensure t
  :hook (org-mode . org-fragtog-mode)
  :config
  (setq org-latex-create-formula-image-program 'dvisvgm) ;; sharper
  (plist-put org-format-latex-options :scale 1.5) ;; bigger
  (setq org-latex-preview-ltxpng-directory (concat (temporary-file-directory) "ltxpng/"))
)

(use-package org-tree-slide
    :ensure t
    :defer t
    :config
    (setq org-tree-slide-slide-in-effect nil
        org-tree-slide-skip-outline-level 3)
)

(use-package org-download
    :ensure t
    :defer 2
    :config
    (setq org-download-method 'attach)
    (advice-add 'org-download-yank :before 'my/system-clipboard-to-emacs-clipboard)
)

; (use-package appt
;   :ensure nil
;   :hook ((after-init . (lambda () (appt-activate 1)))
;          (org-finalize-agenda . org-agenda-to-appt))
;   :config
;   ;; 通知提醒
;   (defun appt-display-with-notification (min-to-app new-time appt-msg)
;     (notify-send :title (format "Appointment in %s minutes" min-to-app)
;                  :body appt-msg
;                  :urgency 'critical)
;     (appt-disp-window min-to-app new-time appt-msg))
;
;   ;; 每15分钟更新一次appt
;   (run-at-time t 900 #'org-agenda-to-appt)
;
;   :custom
;   ;; 是否显示日记
;   (appt-display-diary nil)
;   ;; 提醒间隔时间，每15分钟提醒一次
;   (appt-display-interval 15)
;   ;; 模式栏显示提醒
;   (appt-display-mode-line t)
;   ;; 设置提醒响铃
;   (appt-audible t)
;   ;; 提前30分钟提醒
;   (appt-message-warning-time 30)
;   ;; 通知提醒函数
;   (appt-disp-window-function #'appt-display-with-notification)
; )


(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . my/org-setup)
  :hook (org-mode . my/prettify-symbols-setup)
  :hook (org-capture-mode . evil-insert-state) ;; Start org-capture in Insert state by default
  :diminish org-indent-mode
  :diminish visual-line-mode
  :custom-face
  ;; 设置Org mode标题以及每级标题行的大小
  ; (org-level-1 ((t (:height 1.6 :weight bold))))
  ; (org-level-2 ((t (:height 1.5 :weight bold))))
  ; (org-level-3 ((t (:height 1.4 :weight bold))))
  ; (org-level-4 ((t (:height 1.3 :weight bold))))
  ; (org-level-5 ((t (:height 1.2 :weight bold))))
  ; (org-level-6 ((t (:height 1.1 :weight bold))))
  ;; 设置代码块用上下边线包裹
  (org-block-begin-line ((t (:underline t :background unspecified))))
  (org-block-end-line ((t (:overline t :underline nil :background unspecified))))
  :config
  (setq org-highlight-latex-and-related '(native)) ;; 高亮inline latex语法
  ; (setq org-startup-folded 'showeverything)
  ;; 标题行美化
  (setq org-fontify-whole-heading-line t)
  (setq org-pretty-entities t)
  ;; 自动显示图片
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width 300)
  (setq org-cycle-separator-lines 1)
  (setq org-catch-invisible-edits 'show-and-error) ;; 'smart

  ;; M-Ret can split lines on items and tables but not headlines and not on anything else (unconfigured)
  (setq org-M-RET-may-split-line '((headline) (item . t) (table . t) (default)))
  ;; disable在活动区域内的所有标题栏执行某些命令
  (setq org-loop-over-headlines-in-active-region nil)

  ;; Opens links to other org file in same frame (rather than splitting)
  (setq org-link-frame-setup '((file . find-file)))

  ;; 当状态从DONE改成其他状态时，移除 CLOSED: [timestamp]
  (setq org-closed-keep-when-no-todo t)

    (setq org-log-done (quote note)
        org-log-redeadline (quote time)
        org-log-reschedule (quote time)
        org-log-into-drawer t
        org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK"))

  ;; 紧接着标题行或者计划/截止时间戳后加上记录抽屉
  (setq org-log-state-notes-insert-after-drawers nil)

  ;; 允许字母列表
  (setq org-list-allow-alphabetical t)
  ;; 列表的下一级设置
  (setq org-list-demote-modify-bullet
        '(("+" . "1.") ("1." . "a.") ("-" . "+")))

  ;; Automatically save and close the org files I most frequently archive to.
  ;; I see no need to keep them open and crowding my buffer list.
  ;; Uses my own function my/save-and-close-this-buffer.
  (dolist (file '("homework-archive.org_archive" "todo-archive.org_archive"))
    (advice-add 'org-archive-subtree-default :after
                (lambda () (my/save-and-close-this-buffer file))))

  (defun my/post-org-goto ()
    (let ((current-prefix-arg '(4))) ;; emulate C-u
      (call-interactively 'org-reveal))
    (org-cycle))

  (advice-add 'counsel-org-goto :after #'my/post-org-goto)
  (advice-add 'org-agenda-goto :after #'my/post-org-goto)
  (advice-add 'org-agenda-switch-to :after #'my/post-org-goto)
  (setq org-tags-column -1)

    ;; Columns
    (setq
     org-columns-default-format
     "%80ITEM(Task) %5Score{+} %10Effort(Effort){:} %10CLOCKSUM")

    ;; Sub-tasks
    (setq org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t)

  ;; TOOD的关键词设置，可以设置不同的组
    (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "INPROG(i)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELED(c@/!)")
                (sequence "CANCELED(c@/!)"))))

  ;; TODO关键词的样式设置
  (setq org-todo-keyword-faces '(
                            ("TODO"       :foreground "#50a14f" :weight bold)
                            ("WAITING"    :foreground "#feb24c" :weight bold)
                            ("INPROG"			:foreground "#0098dd" :weight bold)
                            ("DONE"       :foreground "#7c7c75" :weight bold)
                            ("SOMEDAY"		:foreground "#ff6480" :weight bold)
                            ("CANCELLED"  :foreground "red" :weight bold)))

  ;; tags的样式设置
    (setq org-use-fast-todo-selection t)
    (setq org-treat-S-cursor-todo-selection-as-state-change nil)
    (setq org-todo-state-tags-triggers
        (quote (("CANCELED"
                 ("CANCELED" . t))
                ("WAITING"
                 ("WAITING" . t))
                ("SOMEDAY"
                 ("SOMEDAY" . t))
                (done
                 ("WAITING"))
                ("TODO"
                 ("WAITING")
                 ("CANCELED"))
                ("INPROG"
                 ("WAITING"))
                ("DONE"
                 ("WAITING")
                 ("CANCELED")))))

    ;; priorities off.
    (setq org-enable-priority-commands nil)

  ;; 2-refile
    (setq org-refile-use-cache t
        org-refile-targets '((org-agenda-files :maxlevel . 2)
                             (nil :maxlevel . 5))
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file)

  ;; 2-Org-src
  (use-package gnuplot
        :ensure t
        :defer t
    )
    (use-package org-src
    :ensure nil
    :hook (org-babel-after-execute . org-redisplay-inline-images)
    :custom
        ;; Don't prompt before running code in org
    (org-confirm-babel-evaluate nil)
        (python-shell-completion-native-enable nil)
    ;; 代码块语法高亮
    (org-src-fontify-natively t)
    ;; 使用编程语言的TAB绑定设置
    (org-src-tab-acts-natively t)
        ;; How to open buffer when calling `org-edit-special'.
        (org-src-window-setup 'current-window)

    (org-src-lang-modes '(("C"      . c)
                            ("C++"    . c++)
                            ("bash"   . sh)
                            ("cpp"    . c++)
                            ("dot"    . graphviz-dot) ;; was `fundamental-mode'
                            ("python" . python)
                            ("elisp"  . emacs-lisp)
                            ("ocaml"  . tuareg)
                            ("shell"  . sh)))
    (org-babel-load-languages '((C          . t)
                                (dot        . t)
                                (emacs-lisp . t)
                                (eshell     . t)
                                (python     . t)
                                (shell      . t)))
    )

    ;; 3-org-habit
    (use-package org-habit
    :ensure nil
    :defer t
    :config
        (setq org-habit-preceding-days 6
            org-habit-following-days 6
            org-habit-show-habits-only-for-today nil
            org-habit-today-glyph ?⍟ ;;‖
            org-habit-completed-glyph ?✓
            org-habit-graph-column 50)
  )

    ;; 4-org-agenda
  ;; custom time stamp format. I don't use this.

    (use-package org-agenda
    :ensure nil
    :config
    (setq org-time-stamp-custom-formats '("<%A, %B %d, %Y" . "<%m/%d/%y %a %I:%M %p>"))
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-agenda-window-setup 'current-window)
    ;; 日记插入精确时间戳
    (setq org-agenda-insert-diary-extract-time t)
    ;; 从星期一开始作为一周第一天
    (setq org-agenda-start-on-weekday 1)
    ;; Only show upcoming deadlines for the next X days. By default it shows
    ;; 14 days into the future, which seems excessive.
    (setq org-deadline-warning-days 3)
    ;; If something is done, don't show its deadline
    (setq org-agenda-skip-deadline-if-done t)
    ;; If something is done, don't show when it's scheduled for
    (setq org-agenda-skip-scheduled-if-done t)
    ;; If something is scheduled, don't tell me it is due soon
    (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
    ;; use AM-PM and not 24-hour time
    (setq org-agenda-timegrid-use-ampm t)
        (setq org-agenda-time-grid nil)

        ;; basic settings
        (setq org-agenda-show-inherited-tags t
            org-agenda-log-mode-items '(clock)
            org-agenda-clockreport-parameter-plist '(:link nil :maxlevel 2 :fileskip0 t)
            ;;org-agenda-block-separator ?┄
            org-agenda-block-separator nil
            org-agenda-dim-blocked-tasks nil
            ; org-agenda-inhibit-startup t
            org-agenda-breadcrumbs-separator " ❱ ")



        ;;;;; more true to defaults

    ; (setq org-agenda-prefix-format '((agenda . " %-12:T%?-12t% s")
    ;                                    (todo . " %i %-12:c")
    ;                                    (tags . " %i %-12:c")
    ;                                    (search . " %i %-12:c")))
    ;
    ; (setq org-agenda-deadline-leaders '("Deadline:  " "In %2d d.: " "%2d d. ago: "))
    ;
    ; (add-hook 'org-agenda-mode-hook
    ;           #'(lambda () (setq-local line-spacing 3)))
    ;
    ; (add-hook 'org-agenda-mode-hook
    ;           #'(lambda () (hide-mode-line-mode)))

        (setq org-agenda-custom-commands nil)
        (add-to-list 'org-agenda-custom-commands
                     '("o" "My Agenda"
                     ((todo "TODO" (
                                    (org-agenda-overriding-header "\n⚡ Do Today\n┄┄┄┄┄┄┄┄┄┄")
                                    (org-agenda-remove-tags t)
                                    (org-agenda-prefix-format " %-2i %-15b")
                                    (org-agenda-todo-keyword-format "")
                                    ))
                        (agenda "" (
                                    (org-agenda-start-day "+0d")
                                    (org-agenda-span 5)
                                    (org-agenda-overriding-header "⚡ Schedule\n┄┄┄┄┄┄┄┄┄┄")
                                    (org-agenda-repeating-timestamp-show-all nil)
                                    (org-agenda-remove-tags t)
                                    (org-agenda-prefix-format   "  %-3i  %-15b %t%s")
                                    (org-agenda-todo-keyword-format " ☐ ")
                                    (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
                                    (org-agenda-scheduled-leaders '("" ""))
                                    (org-agenda-time-grid (quote ((daily today remove-match)
                                                                (0900 1200 1500 1800 2100)
                                                                "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
                                    ))
                        ))
        )
        (add-to-list 'org-agenda-custom-commands
                     '("b" "Agenda" my/org-agenda-with-tip))

        (add-to-list 'org-agenda-custom-commands
                     '("c" . "COLLECT...") t)

        (add-to-list 'org-agenda-custom-commands
                     '("cb" "CollectBox"
                     ((alltodo ""))))

        (add-to-list 'org-agenda-custom-commands
                     '("f" . "FOCUS...") t)

        (add-to-list 'org-agenda-custom-commands
                     `("f." "Today"
                     ((agenda ""
                                ((org-agenda-entry-types '(:timestamp :sexp))
                                 (org-agenda-overriding-header
                                (concat "CALENDAR Today"
                                        (format-time-string "%a %d" (current-time))))
                                 (org-agenda-span 'day)))
                        (tags-todo "LEVEL=1+REFILE"
                                 ((org-agenda-overriding-header "COLLECTBOX (Unscheduled)")))
                        (tags-todo "DEADLINE=\"<+0d>\""
                                 ((org-agenda-overriding-header "DUE TODAY")
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'notedeadline))
                                    (org-agenda-sorting-strategy '(priority-down))))
                        (tags-todo "DEADLINE<\"<+0d>\""
                                 ((org-agenda-overriding-header "OVERDUE")
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'notedeadline))
                                    (org-agenda-sorting-strategy '(priority-down))))
                        (agenda ""
                                ((org-agenda-entry-types '(:scheduled))
                                 (org-agenda-overriding-header "SCHEDULED")
                                 (org-agenda-skip-function
                                '(org-agenda-skip-entry-if 'todo 'done))
                                 (org-agenda-sorting-strategy
                                '(priority-down time-down))
                                 (org-agenda-span 'day)
                                 (org-agenda-start-on-weekday nil)
                                 (org-agenda-time-grid nil)))
                        (todo "DONE"
                            ((org-agenda-overriding-header "COMPLETED"))))
                     ((org-agenda-format-date "")
                        (org-agenda-start-with-clockreport-mode nil))) t)

        (add-to-list 'org-agenda-custom-commands
                     '("fh" "Hotlist"
                     ((tags-todo "DEADLINE<\"<+0d>\""
                                 ((org-agenda-overriding-header "OVERDUE")))
                        (tags-todo "DEADLINE>=\"<+0d>\"+DEADLINE<=\"<+1w>\""
                                 ((org-agenda-overriding-header "DUE IN NEXT 7 DAYS")))
                        (tags-todo "DEADLINE=\"\"+FLAGGED|DEADLINE>\"<+1w>\"+FLAGGED"
                                 ((org-agenda-overriding-header "FLAGGED"))))
                     ((org-agenda-todo-ignore-scheduled 'future)))  t)

        (add-to-list 'org-agenda-custom-commands
                     '("r" . "REVIEW...") t)

        (add-to-list 'org-agenda-custom-commands
                     '("ra" . "All Tasks...") t)

        (add-to-list 'org-agenda-custom-commands
                     '("rad" "All Tasks (grouped by Due Date)"
                     ((tags-todo "DEADLINE<\"<+0d>\""
                                 ((org-agenda-overriding-header "OVERDUE")
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'notdeadline))))
                        (tags-todo "DEADLINE=\"<+0d>\""
                                 ((org-agenda-overriding-header "DUE TODAY")
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'notdeadline))))
                        (tags-todo "DEADLINE=\"<+1d>\""
                                 ((org-agenda-overriding-header "DUE TOMORROW")
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'notdeadline))))
                        (tags-todo "DEADLINE>\"<+1d>\"+DEADLINE<=\"<+7d>\""
                                 ((org-agenda-overriding-header "DUE WITHIN A WEEK")
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'notdeadline))))
                        (tags-todo "DEADLINE>\"<+7d>\"+DEADLINE<=\"<+28d>\""
                                 ((org-agenda-overriding-header "DUE WITHIN A MONTH")
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'notdeadline))))
                        (tags-todo "DEADLINE>\"<+28d>\""
                                 ((org-agenda-overriding-header "DUE LATER")
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'notdeadline))))
                        (tags-todo "TODO={WAIT}"
                                 ((org-agenda-overriding-header "WAITING FOR")
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'deadline))))
                        (todo ""
                            ((org-agenda-overriding-header "WAITING FOR")
                             (org-agenda-skip-function
                                '(org-agenda-skip-entry-if 'deadline)))))
                     ((org-agenda-sorting-strategy '(priority-down))
                        (org-agenda-write-buffer-name "All Tasks (grouped by Due Date)"))
                     "~/Org/all-tasks-by-due-date.pdf") t)

        (add-to-list 'org-agenda-custom-commands
                     '("ra1" "All Tasks with a due date"
                     ((alltodo ""))
                     ((org-agenda-overriding-header "All Tasks (sorted by Due Date)")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'notdeadline))
                        (org-agenda-sorting-strategy '(deadline-up)))) t)


        (add-to-list 'org-agenda-custom-commands
                     '("rag" "Grouped Tasks")
                     ())

        (add-to-list 'org-agenda-custom-commands
                     '("rt" . "Timesheet...") t)

        ;; Show what happened today.
        (add-to-list 'org-agenda-custom-commands
                     '("rtd" "Daily Timesheet"
                     ((agenda ""))
                     ((org-agenda-log-mode-items '(clock closed))
                        (org-agenda-overriding-header "DAILY TIMESHEET")
                        (org-agenda-show-log 'clockcheck)
                        (org-agenda-span 'day)
                        (org-agenda-start-with-clockreport-mode t)
                        (org-agenda-time-grid nil))) t)

        ;; Show what happened this week.
        (add-to-list 'org-agenda-custom-commands
                     '("rtw" "Weekly Timesheet"
                     ((agenda ""))
                     (
                        ;; (org-agenda-format-date "")
                        (org-agenda-overriding-header "WEEKLY TIMESHEET")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                        (org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-start-with-clockreport-mode t)
                        (org-agenda-time-grid nil))) t)


        (add-to-list 'org-agenda-custom-commands
                     '("rc" . "Calendar...") t)

        (add-to-list 'org-agenda-custom-commands
                     '("rc7" "Events and appointments for 7 days"
                     ((agenda ""))
                     ((org-agenda-entry-types '(:timestamp :sexp))
                        ;; (org-agenda-overriding-header "Calendar for 7 days")
                        (org-agenda-span 'week)
                        (org-agenda-format-date "\n%a %d")
                        ;; (org-agenda-date-weekend ... new face ...)
                        (org-agenda-time-grid nil))) t)

        (add-to-list 'org-agenda-custom-commands
                     '("rw" "Weekly review"
                     ((tags "CATEGORY={@REFILE}&LEVEL<=2"
                            ((org-agenda-overriding-header "NEW TASKS")))
                        (agenda ""
                                ((org-agenda-clockreport-mode t)
                                 (org-agenda-format-date
                                (concat "\n"
                                        "%Y-%m-%d" " %a "
                                        (make-string (window-width) ?_)))
                                 (org-agenda-overriding-header "PAST WEEK")
                                 (org-agenda-prefix-format " %?-11t %i %-12:c% s")
                                 (org-agenda-show-log 'clockcheck)
                                 (org-agenda-span 7)
                                 (org-agenda-start-day "-1w")
                                 (org-deadline-warning-days 0)))
                        (agenda ""
                                ((org-agenda-overriding-header "NEXT MONTH")
                                 (org-agenda-span 'month)
                                 (org-agenda-start-day "+0d")
                                 (org-deadline-warning-days 0)))
                        (todo "PROJECT"
                            ((org-agenda-overriding-header "PROJECT LIST")))
                        (todo "DONE|PROJECTDONE"
                            ((org-agenda-overriding-header
                                "Candidates to be archived"))))))

        ;; This hook runs first in the agenda (and before it is set to read-only)
        (add-hook 'org-agenda-mode-hook 'my/agenda-score-goal)
    )

    ;; 5-org-capture
    (use-package org-capture
    :ensure nil
    :config
    (with-no-warnings
        (defun org-capture-setup ()
        (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
    :custom
    (org-capture-use-agenda-date t)
    (org-capture-templates-contexts nil)
        (org-capture-templates
            '(("t" "Todo" entry
                 (file "~/Org/inbox.org")
                 "* TODO %?  :REFILE:\n  %U\n%^{Score}p" :clock-in t :clock-resume t)
            ("n" "Note" entry
                        (file+headline "~/Org/capture.org" "Notes")
                 "* %?  :NOTE:\n  %U\n  %a\n  :CLOCK:\n  :END:")
                ("c" "Capture current TODO mix in table" table-line (file+headline "~/Org/WeeklyReports.org" "Burndown")
                 "%(my/org-count-tasks-by-status)")
                ("s" "Capture Weekly Score in table" table-line (file+headline "~/Org/WeeklyReports.org" "Scores")
                 "%(my/add-weekly-score-table-entry)")
                ("e" "Capture Weekly time in table" table-line (file+headline "~/Org/WeeklyReports.org" "Minutes")
                 "%(my/org-time-logged-table-entry)")
            ("w" "Web Collections" entry
            (file+headline "~/Org/inbox.org" "Web")
            "* %^{url}\n%u\n")))
    )


    ;; 6-org-exproting
  ;; (setq org-export-backends '(ascii beamer html latex md odt))

  (setq org-export-with-broken-links t
        org-export-with-smart-quotes t
        org-export-allow-bind-keywords t)

  ;; From https://stackoverflow.com/questions/23297422/org-mode-timestamp-format-when-exported
  (defun org-export-filter-timestamp-remove-brackets (timestamp backend info)
    "removes relevant brackets from a timestamp"
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
     ((org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))

  ;; HTML-specific
  (setq org-html-validation-link nil) ;; No validation button on HTML exports

  ;; LaTeX Specific
  (eval-after-load 'ox '(add-to-list
                         'org-export-filter-timestamp-functions
                         'org-export-filter-timestamp-remove-brackets))

    ;; 7-org-latex
  (setq org-latex-listings t) ;; Uses listings package for code exports
  (setq org-latex-compiler "xelatex") ;; XeLaTex rather than pdflatex

  ;; not sure what this is, look into it
  ;; '(org-latex-active-timestamp-format "\\texttt{%s}")
  ;; '(org-latex-inactive-timestamp-format "\\texttt{%s}")

  ;; LaTeX Classes
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("org-plain-latex" ;; I use this in base class in all of my org exports.
                   "\\documentclass{extarticle}
  [NO-DEFAULT-PACKAGES]
  [PACKAGES]
  [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    )


    ;; 8-org-misc
  (setq org-clock-mode-line-total 'current) ;; Show only timer from current clock session in modeline
  (setq org-clock-clocked-in-display 'both)

  (setq org-attach-id-dir ".org-attach/"
        org-attach-use-inheritance t)

    (setq org-modules
        '(org-bbdb
            org-bibtex
            org-info
            org-calc))

    (setq org-global-properties
        '(("STYLE_ALL"  . "habit")
            ("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00")
            ("Score_ALL"  . "1 2 3 5 8")))


) ;; This parenthesis ends the org use-package.


(provide 'init-org)
