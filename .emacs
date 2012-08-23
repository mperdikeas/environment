(menu-bar-mode 0)
(add-to-list 'load-path "~/.emacs.d")

(progn ;; progn
(global-linum-mode 1)
(add-hook 'linum-before-numbering-hook
    (lambda () (setq linum-format "%d  ")))
(defun linum-face-settings ()
  "Face settings for `linum'."
  (custom-set-faces
   '(linum
     ((((background dark))
       :foreground "green")
      (t :foreground "gray")))))

(eval-after-load 'linum
  `(linum-face-settings))
)

(provide 'linum-face-settings)
;; (setq linum-format "%d  ") ;; how is this line different from the above?

;; make all "yes or no" prompts show "y or m" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; no tabs, spaces instead!
(setq-default indent-tabs-mode nil)
(setq-default tabs-width 2)

;; syntax highlight everywhere
(global-font-lock-mode t)

(progn ;; current line highlight
  (global-hl-line-mode 1)
  ;;  (set-face-background hl-line-face "#4169E1") ;; royal blue
  (set-face-background hl-line-face "#E0B0FF") ;; mauve
  (set-face-foreground hl-line-face "#000000")
)

(setq column-number-mode t)

;; Remove completion buffer when done
(add-hook 'minibuffer-exit-hook
    '(lambda ()
        (let ((buffer "*Completions*"))
            (and (get-buffer buffer)
                (kill-buffer buffer)))))    

(global-set-key (kbd "C-x C-l") 'join-line) ;; shadows a disabled command (lowercase region)

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
        (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
        (if nil
            (progn ;; option A.1 use one of the color-themes
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

            (progn ;; option A.2 use color-theme-zenburn which seems to be in its own package
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
  (autoload 'tuareg-mode              "tuareg"           "Major mode for editing Caml code" t)
  (autoload 'camldebug                "camldebug"        "Run the Caml debugger" t)
  (autoload 'tuareg-imenu-set-imenu   "tuareg-imenu"     "Configuration of imenu for tuareg" t) 
  (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
)

(progn ;; AutoJavaComplete, AutoComplete and YASnippet install (together, as a group because their functionalities are linked and complementary and, further, AutoJavaComplete explicitly requires the other two
  (progn ;; YASnippet
    (add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c")
    (require 'yasnippet) ;; not yasnippet-bundle
    (yas/initialize)
    (progn ;; setup snippet directory for reference and development with 'yas/new-snippet'
        (setq yas/root-directory '("~/.emacs.d/mysnippets" "~/.emacs.d/yasnippet-0.6.1c/snippets")) ;; 
        (mapc 'yas/load-directory yas/root-directory) ;; Load the snippets - all are referenced but only the first element of the list is (~/.emacs.d/mysnippets) is used for new development
    )
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


(progn ;; show buffer's full path: http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
    (defun show-file-name()
      "Show the full path file name in the minibuffer."
      (interactive)
      (message (buffer-file-name))
      (kill-new (file-truename buffer-file-name))
    )
    (global-set-key "\C-cz" 'show-file-name)
)


(if nil  ;; only one of the following two make sense
    ;; whole path name of the buffer displayed in the mode line
    (setq-default mode-line-buffer-identification
    '((buffer-file-name
    #("%12f" 0 4 (face bold))
    #("%12b" 0 4 (face bold)))))
    
    ;; set the full path to the file to the title-bar
    (setq frame-title-format `("@" ,(system-name) ": "
    (buffer-file-name "%f"
    (dired-directory dired-directory
    "%b"))))
)

(if nil ;; it turns out that that's not very practical when doing C-x b
    (progn  ;; http://compgroups.net/comp.emacs/mode-line-file-name-with-path/296677
        (require 'uniquify)
        (setq uniquify-buffer-name-style 'forward)
        (setq uniquify-min-dir-content '5)
    )
)

;; the following presumable enables clipboard sharing between the
;; Emacs buffer and other applications but strangely I have found
;; that to work even with the following line absent; still, it
;; doesn't hurt to have it.
(setq x-select-enable-clipboard t)

(progn ;; emacs F# mode
    (setq load-path (cons "~/.emacs.d/fsharp" load-path))
    (setq auto-mode-alist (cons '("\\.fs[iylx]?$" . fsharp-mode) auto-mode-alist))
    (autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
    (autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)
    (setq inferior-fsharp-program "fsharpi --readline-")
    (setq fsharp-compiler "fsharpc")
)

(add-to-list 'auto-mode-alist '("\\.xhtml$" . nxml-mode))