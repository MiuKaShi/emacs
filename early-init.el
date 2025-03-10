;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; 设置垃圾回收参数
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; 禁止自动缩放窗口先
(setq frame-inhibit-implied-resize t)

;; skip the mtime checks on every *.elc file.
(setq load-prefer-newer 'noninteractive)

;; Define the level of native compilation optimization.
(setq native-comp-speed 3)
(setq native-comp-async-jobs-number 0)
(setq native-comp-jit-compilation t)

;; Compile warnings
(setq native-comp-async-report-warnings-errors 'silent) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))


;; MISC OPTIMIZATIONS ----
;;; optimizations (froom Doom's core.el). See that file for descriptions.
(setq idle-update-delay 1.0)

;; 禁止展示菜单栏、工具栏和纵向滚动条
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; 禁止菜单栏、工具栏、滚动条模式，禁止启动屏幕和文件对话框
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

;; 在这个阶段不编译
(setq comp-deferred-compilation nil)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)

;; 不压缩字体缓存，加快 GC
(setq inhibit-compacting-font-caches t)


(provide 'early-init)
