(menu-bar-mode 0)
(add-to-list 'load-path "~/.emacs.d")


(global-linum-mode)

(progn ;; current line highlight
  (global-hl-line-mode 1)
  (set-face-background hl-line-face "blue")
)

(setq column-number-mode t)

;; Remove completion buffer when done
(add-hook 'minibuffer-exit-hook
    '(lambda ()
        (let ((buffer "*Completions*"))
            (and (get-buffer buffer)
                (kill-buffer buffer)))))    

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


(progn ;; configure emacs backup behavior
    (setq backup-directory-alist `(("." . "~/.emacssaves")))
    (setq backup-by-copying t)
    (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
    ;; (setq make-backup-files nil)
)
;; (load "col-highlight") ;; they are loaded recursively from crosshairs
;; (load "hl-line+")
(load "crosshairs")
;;(toggle-crosshairs-when-idle t)
;;(crosshairs-mode)
;;(flash-crosshairs)
(global-set-key (kbd "C-x C-j") 'flash-crosshairs)

(progn ;; ace-jump-mode
  (require 'ace-jump-mode) 
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
    (define-key global-map (kbd "C-c C-c SPC") 'ace-jump-char-mode)
    (define-key global-map (kbd "C-c C-c C-c SPC") 'ace-jump-line-mode)
  ;;;; if you also use viper mode:
  ;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
)

(global-set-key (kbd "M-g g") 'goto-line)

(if t
    (progn ;; option A : customize using color-theme
        (progn ;; color-theme
          (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
          (require  'color-theme)
          (eval-after-load "color-theme"
          '(progn
             (color-theme-initialize)
            ;; these are some agreeable themes (examined 14.II.2011)
            ;; (color-theme-jsc-dark) ;; color-theme-arjen ;; color-theme-simple-1
            ;; (color-theme-vim-colors) ;; (color-theme-tty-dark)   ;; color-theme-euphoria
            ;;  (color-theme-tty-dark)
             (color-theme-jsc-dark)
            )
           )    
        )
    
        (if nil ;; alternative: zenburn
            (progn
              (require 'color-theme-zenburn)
              (color-theme-zenburn)
            )
        )
    )
    (progn ;; option B : customize by hand ;; I tried it on 2012-03-07 and wasn't very pleased with the results
        (custom-set-faces
           ;; custom-set-faces was added by Custom.
           ;; If you edit it by hand, you could mess it up, so be careful.
           ;; Your init file should contain only one such instance.
           ;; If there is more than one, they won't work right.
          '(default ((t (:inherit nil :stipple nil :background "lightyellow2" :foreground "gray20" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :width normal :family "liberation mono"))))
          '(background "blue")
          '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
          '(font-lock-comment-face ((t (:foreground "MediumAquamarine"))))
          '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
          '(font-lock-doc-string-face ((t (:foreground "green2"))))
          '(font-lock-function-name-face ((t (:foreground "SkyBlue"))))
          '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
          '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
          '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
          '(font-lock-string-face ((t (:foreground "LimeGreen"))))
        )
    )
)



(require 'repeat) ;; to repeat last command: "C-x z"; once pressed, additional 'z's will keep repeating it.
(setq initial-frame-alist '((top . 200) (left . 500)))

(progn ;; emacs tuareg
  (add-to-list 'load-path "~/.emacs.d/tuareg-2.0.4/compiled") ;; the "compiled" folder was created by running "make install DEST=compiled" - it was not part of the tgz archive - read README in the archive for more
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
)

(progn ;; AutoJavaComplete, AutoComplete and YASnippet install (together, as a group because their functionalities are linked and complementary and, further, AutoJavaComplete explicitly requires the other two
  (progn ;; YASnippet
    (add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c")
    (require 'yasnippet) ;; not yasnippet-bundle
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")
  )
 (progn ;; AutoComplete
    (add-to-list 'load-path "~/.emacs.d/auto-complete")
    (eval-after-load 'auto-complete-config
       '(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
    )
    (require 'auto-complete-config)   
    (ac-config-default)
 )
 (progn;; AutoJavaComplete
   (add-to-list 'load-path "~/.emacs.d/ajc-java-complete/")
   (require 'ajc-java-complete-config)
   (add-hook 'java-mode-hook 'ajc-java-complete-mode)
   (add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)
;; AutoJavaComplete bindings:
;; C-c i ajc-import-all-unimported-class
;; C-c m ajc-import-class-under-point
 )
)

;; useful emacs tips
;; how to byte-compile everything in a folder:
;;    C-u 0 M-x byte-recompile-directory
;;
;;    will compile all the .el files in the directory and in all subdirectories below.
;;    The C-u 0 part is to make it not ask about every .el file that does not have a .elc counterpart.
