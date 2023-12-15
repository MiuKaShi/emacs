;;; init-ui.el --- ui setting -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-org-config)

  (let ((theme (if (display-graphic-p)
                   'doom-gruvbox
                 'doom-gruvbox)))
    (load-theme theme t)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project) ;; Just show file name (no path)
  (doom-modeline-height 35)
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-github nil)
	(doom-modeline-icon t)
	(doom-modeline-modal-icon nil)
  (doom-modeline-persp-name nil)
	(doom-modeline-major-mode-icon t)
	(doom-modeline-major-mode-color-icon nil)
  (doom-modeline-unicode-fallback t)
	(doom-modeline-buffer-state-icon nil)
	(doom-modeline-lsp-icon t)
  (doom-modeline-enable-word-count nil))

;; Customize popwin behavior
(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules '((magit-status-mode        :select t :inhibit-window-quit t :same t)
                   (magit-log-mode           :select t :inhibit-window-quit t :same t)
                   (vc-annotate-mode         :select t :inhibit-window-quit t :same t)
                   ("*quickrun*"             :select t :inhibit-window-quit t :same t)
                   (profiler-report-mode     :select t)
                   (xwidget-webkit-mode      :select t :same t)
                   (flycheck-error-list-mode :select t :align t :size 10)
                   (comint-mode              :select t :align t :size 0.4)
                   (grep-mode                :select t :align t)
                   (rg-mode                  :select t :align t)
                   ;; See also `help-window-select'
                   (apropos-mode             :select nil :align t :size 0.4)
                   (help-mode                :select nil :align t :size 0.4)
                   ("*Backtrace*"               :select t   :align t :size 15)
                   ("*Shell Command Output*"    :select nil :align t :size 0.4)
                   ("*Async Shell Command*"     :select nil :align t :size 0.4)
                   ("*Org-Babel Error Output*"  :select nil :align t :size 0.3)
                   ("*Process List*"            :select t   :align t :size 0.3)
                   ("*Occur*"                   :select t   :align t)
                   ("\\*eldoc\\( for \\)?.*\\*" :select nil :align t :size 15 :regexp t))))

(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  (help-enable-variable-value-editing t))

;; Windows layout recorder
;;
;; You can still use `winner-mode' on Emacs 26 or early. On Emacs 27, it's
;; preferred over `winner-mode' for better compatibility with `tab-bar-mode'.
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-history-mode)
  :custom
  (tab-bar-history-buttons-show nil))

(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :demand t)

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode)
  :hook org-mode prog-mode)

(use-package dashboard
  :ensure t
  :init
  ;; Format: "(icon title help action face prefix suffix)"
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
	(dashboard-center-content nil)
  (dashboard-items '((recents   . 10)
                     (agenda . 5 )
                     (bookmarks . 3)
                     (registers . 3)
                     (projects  . 3))))

;; 禁用一些GUI特性
(setq use-file-dialog nil
      inhibit-x-resources t)
(setq use-dialog-box nil)               ; 鼠标操作不使用对话框
(setq inhibit-default-init t)           ; 不加载 `default' 库
(setq inhibit-startup-screen t)         ; 不加载启动画面
(setq inhibit-startup-message t)        ; 不加载启动消息
(setq inhibit-startup-buffer-menu t)    ; 不显示缓冲区列表

;; 以16进制显示字节数
(setq display-raw-bytes-as-hex t)
;; 有输入时禁止 `fontification' 相关的函数钩子，能让滚动更顺滑
(setq redisplay-skip-fontification-on-input t)

;; BELL/WARNING ------------
(setq ring-bell-function 'ignore) ;; 禁止响铃
(setq blink-cursor-mode nil) ;; 禁止闪烁光标

;; 鼠标滚动设置
(setq scroll-step 2)
(setq scroll-margin 2)
(setq hscroll-step 2)
(setq hscroll-margin 2)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq scroll-preserve-screen-position 'always)

; 对于高的行禁止自动垂直滚动
(setq auto-window-vscroll nil)

;; 设置新分屏打开的位置的阈值
(setq split-width-threshold (assoc-default 'width default-frame-alist))
(setq split-height-threshold nil)

;; 行号显示
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Font size
(set-face-attribute 'default nil
  :font "MonoLisa Nerd Font"
  :height 180
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Source Code Pro"
  :height 180
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "MonoLisa Nerd Font"
  :height 180
  :weight 'medium)
;; Makes commented text italics.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
;; Makes keywords italics.
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)
;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

; transparency
(set-face-attribute 'highlight nil :foreground 'unspecified)
(if window-system (progn
  (set-background-color "Black")
  (set-foreground-color "LightGray")
  (set-cursor-color "Gray")
  (set-frame-parameter nil 'alpha 75))) ; 透明度
(defun set-transparency ()
  "set frame transparency"
  (set-frame-parameter nil 'alpha 75))  ; 透明度
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(75))))

(provide 'init-ui)
