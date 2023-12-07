;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'init-macros)

(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (setq org-super-agenda-header-map nil) ;; takes over 'j'
  ;; (setq org-super-agenda-header-prefix " ◦ ") ;; There are some unicode "THIN SPACE"s after the ◦
  ;; Hide the thin width char glyph. This is dramatic but lets me not be annoyed
  (add-hook 'org-agenda-mode-hook
            #'(lambda () (setq-local nobreak-char-display nil)))
  (org-super-agenda-mode)
)

(use-package org-superstar
  :ensure t
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("INPROG" . 9744)
                                          ("WORK" . 9744)
                                          ("STUDY" . 9744)
                                          ("SOMEDAY" . 9744)
                                          ("READ" . 9744)
                                          ("CANCELLED" . 9744)
                                          ("CONTACT" . 9744)
                                          ("DONE" . 9745)))
  ;; :hook (org-mode . org-superstar-mode)
)
;; Removes gap when you add a new heading
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(use-package org-modern
	:ensure t
	:hook (org-mode . org-modern-mode)
	:config
 	(setq org-modern-label-border 1)
 	;; 标题行型号字符
 	(setq org-modern-star ["◉" "○" "✸" "✳" "◈" "◇" "✿" "❀" "✜"])
 	;; 列表符号美化
 	(setq org-modern-list
       	'((?- . "•")
         	(?+ . "◦")
         	(?* . "▹")))
 	(setq org-modern-tag nil)
 	(setq org-modern-priority nil)
 	(setq org-modern-todo nil)
 	(setq org-modern-progress nil)
 	; (setq org-modern-timestamp nil)
 	(setq org-modern-table nil)
)


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
(use-package org-gcal
 	:ensure t
 	:defer t
 	:config
 	(setq org-gcal-down-days '20					;; Only fetch events 20 days into the future
      	org-gcal-up-days '10					;; Only fetch events 10 days into the past
      	org-gcal-recurring-events-mode 'top-level
      	org-gcal-remove-api-cancelled-events t) ;; No prompt when deleting removed events
)

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

(setq org-modules '(org-habit))

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
  (setq org-ellipsis " ▼") ;; 设置标题行折叠符号
  ;; ⤵ ▼ ⬎  
  (setq org-highlight-latex-and-related '(native)) ;; 高亮inline latex语法
  (setq org-startup-folded 'showeverything)
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

  (setq org-closed-keep-when-no-todo t)
  ;; DONE时加上时间戳
  (setq org-log-done 'time)
  ;; 重复执行时加上时间戳
  (setq org-log-repeat 'time)
  ;; Deadline修改时加上一条记录
  (setq org-log-redeadline 'note)
  ;; Schedule修改时加上一条记录
  (setq org-log-reschedule 'note)
  ;; 以抽屉的方式记录
  (setq org-log-into-drawer t)
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

  ;; TOOD的关键词设置，可以设置不同的组
  (setq org-todo-keywords '((sequence "TODO(t)" "WORK(w)" "STUDY(s)" "INPROG(i)" "HOLD(h)" "|" "DONE(d!)" "CANCELLED(c@)")
					   (sequence "READ(r)" "SOMEDAY" "CONTACT(c)" "|" "FIXED(f)")))
  ;; TODO关键词的样式设置
  (setq org-todo-keyword-faces '(
							("TODO"       :inherit (region org-todo) :foreground "#50a14f" :weight bold)
							("WORK"       :inherit (region org-todo) :foreground "#feb24c" :weight bold)
							("STUDY"      :inherit (region org-todo) :foreground "#ff6480" :weight bold)
							("INPROG"			:inherit (org-todo region) :foreground "#0098dd" :weight bold)
							("HOLD"				:inherit (org-todo region) :foreground "#9f7efe"  :weight bold)
							("DONE"       :foreground "#7c7c75" :weight bold)
							("CANCELLED"  :foreground "red" :weight bold)
							("READ"				:foreground "magenta" :weight bold)
							("SOMEDAY"		:foreground "green" :weight bold)
							("CONTACT"		:foreground "red" :weight bold)))


  (setq org-lowest-priority ?F)  ;; Gives us priorities A through F
  (setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].
  (setq org-priority-faces
        '((65 . "red2")
          (66 . "Gold1")
          (67 . "Goldenrod2")
          (68 . "PaleTurquoise3")
          (69 . "DarkSlateGray4")
          (70 . "PaleTurquoise4")))

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
      		org-habit-graph-column 40)
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

  	;; (setq org-agenda-span 'day)

  	(setq org-agenda-block-separator ?-)

		;;;;; more true to defaults

  	(setq org-agenda-prefix-format '((agenda . " %-12:T%?-12t% s")
                                   	 (todo . " %i %-12:c")
                                   	 (tags . " %i %-12:c")
                                   	 (search . " %i %-12:c")))

  	(setq org-agenda-deadline-leaders '("Deadline:  " "In %2d d.: " "%2d d. ago: "))

  	(add-hook 'org-agenda-mode-hook
            	#'(lambda () (setq-local line-spacing 3)))

  	(add-hook 'org-agenda-mode-hook
            	#'(lambda () (hide-mode-line-mode)))
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
  	(org-capture-templates `(("t" "Study" entry (file+headline "study.org" "Reminders")
                            	"* TODO %i%?"
                            	:empty-lines-after 1
                            	:prepend t)
                           	 ("n" "Notes" entry (file+headline "capture.org" "Notes")
                            	"* %? %^g\n%i\n"
                            	:empty-lines-after 1)
                           	 ;; For EWW
                           	 ("b" "Bookmarks" entry (file+headline "capture.org" "Bookmarks")
                            	"* %:description\n\n%a%?"
                            	:empty-lines 1
                            	:immediate-finish t)
                           	 ("d" "Diary")
                           	 ("dt" "Today's TODO list" entry (file+olp+datetree "diary.org")
                            	"* Today's TODO list [/]\n%T\n\n** TODO %?"
                            	:empty-lines 1
                            	:jump-to-captured t)
                           	 ("do" "Other stuff" entry (file+olp+datetree "diary.org")
                            	"* %?\n%T\n\n%i"
                            	:empty-lines 1
                            	:jump-to-captured t)
                           	 ))
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

) ;; This parenthesis ends the org use-package.


(provide 'init-org)
