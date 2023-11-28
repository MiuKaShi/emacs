(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(package-selected-packages
   '(org-contrib valign marginalia embark-consult consult embark vertico diredfl pyvenv haskell-mode bazel tuareg cargo rust-mode cmake-font-lock cmake-mode bison-mode rmsbolt graphviz-dot-mode yaml-mode devdocs citre dumb-jump flycheck projectile quickrun hl-todo spdx diff-hl magit lsp-mode company evil-collection evil-surround evil atomic-chrome fanyi webpaste separedit gcmh avy rg which-key dashboard shackle doom-modeline doom-themes exec-path-from-shell try quelpa no-littering)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
