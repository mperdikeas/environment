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
  (if nil
      (progn
        (set-face-background hl-line-face "#E0B0FF") ;; mauve
        (set-face-foreground hl-line-face "#000000"))
    (progn
      (set-face-background hl-line-face "#2E2E00")))) ;; background only: don't change foreground. Other interesting values I've tried: 470047, 2E2E00, 333300, 191975




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
 '(inhibit-startup-screen t)
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((((background dark)) :foreground "green") (t :foreground "gray"))))


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


(progn ;; ace-jump-mode
  (require 'ace-jump-mode) 
  (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
  (define-key global-map (kbd "C-c C-c C-SPC") 'ace-jump-char-mode)
  (define-key global-map (kbd "C-c C-c C-c C-SPC") 'ace-jump-line-mode)
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

        (if nil
            (progn ;; option A.2 use color-theme-zenburn which seems to be in its own package
              (require 'color-theme-zenburn)
              (color-theme-zenburn)))
        (progn ;; option A.3 (annum 2015, use the latest zenburn theme for Emacs 24)
          (require 'zenburn-theme)
          (load-theme 'zenburn t))
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
(add-to-list 'auto-mode-alist '("\\.xsd$"   . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.wsdl$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xjb$"   . nxml-mode))



(if
    (= emacs-major-version 23)
    (load "package") ;; NB: for Emacs 23 that means you need package.el on your load path.
  )




;; Clojure Mode https://github.com/technomancy/clojure-mode/blob/master/README.md
(progn ;; Clojure mode, Paredit and Inferior Lisp
  (if nil ;; apparently that doesn't work very well with Emacs 23 I received advice:
      ;; https://groups.google.com/forum/#!topic/clojure/3o__0ZCI8rE
      ;; switch to Emacs 24 which I will when the problems become too much.
      (progn
        (print "marmalade loading of clojure-mode and Paredit")
        (require 'package)
        (add-to-list 'package-archives
                     '("marmalade" . "http://marmalade-repo.org/packages/"))
        (package-initialize)
        (when (not (package-installed-p 'clojure-mode))
          (package-install 'clojure-mode)
          )
        (when (not (package-installed-p 'paredit))
          (package-install 'paredit)
          )
        )
    (progn
      (print "manual loading of clojure-mode")
      (load "clojure-mode")
      (load "paredit")
      )
    )
  (if nil ;; turn if off for now
      (progn ;; Paredit
        (require 'paredit) ;; if you didn't install via package.el
        (defun turn-on-paredit () (paredit-mode 1))
        (add-hook 'clojure-mode-hook 'turn-on-paredit)

        ;; good. Now turn it off (for now) -> seems's not working
        (add-hook 'clojure-mode-hook (lambda () (paredit-mode nil)))
        )
    )
  (progn ;; Inferior Lisp
    (add-hook 'clojure-mode-hook ;; copied from: http://ubercode.de/blog/make-emacs-evaluate-clojure-in-5-minutes
              (lambda ()
                (setq inferior-lisp-program "~/.emacs.d/clojure/repl.sh"))) ;  I've also seen the following:
                                        ;    (setq inferior-lisp-program "lein repl")))
    (setq inferior-lisp-program "~/.emacs.d/clojure/repl.sh") ;; the hook above is necessary to automatically set inferior-lisp when in clojure mode, this command here is not
    ;; redundant but necessary to be able to invoke inferior-lisp in a buffer when editing non-clojure files
    ;; ARE THE BELOW LINES NEEDED? I DON'T REMEMBER ANY MORE
    ;; (setq inferior-lisp-load-command "(load \"%s\")\n")
    ;; (setq lisp-function-doc-command "(doc %s)\n")
    ;; (setq lisp-var-doc-command "(doc %s)\n")
    )
  )

(progn ;; dirtree from: http://code.google.com/p/ywb-codes/source/browse/trunk/emacs/site-lisp/contrib/dirtree.el
  ;; through     : https://github.com/zkim/emacs-dirtree
  ;; through     : http://stackoverflow.com/questions/3538064/nerdtree-for-emacs
  (require 'tree-mode)
  (require 'windata)
  (require 'dirtree) ;; activated with M-x dirtree
  )

(global-set-key (kbd "C-x C-j") 'flash-crosshairs) ;; make sure crosshairs C-x C-j overrides dired similar binding
;; (which I believe is also available with C-x d)

(progn ;; Emacs Ant in Java mode - I can't get the error hyperlinks to work
  (defun ant-compile ()
    "Traveling up the path, find build.xml file and run compile."
    (interactive)
    (with-temp-buffer
      (while (and (not (file-exists-p "build.xml"))
                  (not (equal "/" default-directory)))
        (cd ".."))
      (call-interactively 'compile)))
  (add-hook 'java-mode-hook
            (lambda ()
              (progn
                (local-set-key (kbd "C-x RET") 'ant-compile)
                (setq compile-command "ant -emacs -find build.xml "))))
  )

(global-set-key "\C-x\C-b" 'buffer-menu)


(progn
  ;; see: http://stackoverflow.com/questions/1128927/how-to-scroll-line-by-line-in-gnu-emacs
  (setq scroll-step            1
        scroll-conservatively  10000)
  )

(progn
  (global-set-key [M-left] 'windmove-left)  ; move to left windnow
  (global-set-key [M-right] 'windmove-right); move to right window
  (global-set-key [M-up] 'windmove-up)      ; move to upper window
  (global-set-key [M-down] 'windmove-down)  ; move to downer window
  )

(progn ;; ST mode
  (require 'stringtemplate-mode)
  (autoload 'stringtemplate-mode "stringtemplate-mode" "StringTemplate editing mode" t)
  )

(if nil ;; doesn't seem to be working: http://unix.stackexchange.com/a/77360/24044
    (eval-after-load 'shell
      '(progn
         (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
         (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
         t)))

(progn ;; see http://melpa.milkbox.net/#/inf-ruby and http://stackoverflow.com/a/6385982/274677
  (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
  (global-set-key (kbd "C-c C-r") 'ruby-send-region)
  (if nil
      (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode) ;; do not enable the enh-ruby-mode (I guess that means 'enhanced')
    (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)))

(global-set-key "\C-x\C-k" 'compile)

(progn;; http://unix.stackexchange.com/a/154154/24044
  (defun last-message (&optional num)
    (or num (setq num 1))
    (if (= num 0)
        (current-message)
      (save-excursion
        (set-buffer "*Messages*")
        (save-excursion
          (forward-line (- 1 num))
          (backward-char)
          (let ((end (point)))
            (forward-line 0)
            (buffer-substring-no-properties (point) end))))))
  (defun insert-last-message (&optional num)
    (interactive "*p")
    (insert (last-message num)))
  (global-set-key "\C-cm" 'insert-last-message)
  )

(progn ;; http://unix.stackexchange.com/a/45381/24044
  (defun insert-buffer-name ()
    "Insert the full path file name into the current buffer."
    (interactive) 
    (insert (buffer-name (window-buffer (minibuffer-selected-window))))
    )
  (global-set-key "\C-x\C-i" 'insert-buffer-name)
  )

(setq Buffer-menu-buffer+size-width 60) ;; http://stackoverflow.com/a/26062716/274677

(progn
  (load "json-mode")
  (add-to-list 'auto-mode-alist '("\\.json" . json-mode))
  )
(put 'downcase-region 'disabled nil)


(setq tramp-mode nil) ;; disable tramp-mode, see: https://groups.google.com/forum/#!topic/gnu.emacs.help/OLRkGgJqgu8

(progn ;; http://emacs.stackexchange.com/a/5750
  (defun my-shell-hook ()
    (define-key shell-mode-map (kbd "C-c SPC") 'ace-jump-mode))
  (add-hook 'shell-mode-hook 'my-shell-hook)
  )

(put 'upcase-region 'disabled nil)

(global-set-key "\C-x%" 'shrink-window)


(progn ;; http://stackoverflow.com/a/2173393/274677
  (defun vi-open-line-above ()
    "Insert a newline above the current line and put point at beginning."
    (interactive)
    (unless (bolp)
      (beginning-of-line))
    (newline)
    (forward-line -1)
    (indent-according-to-mode))

  (defun vi-open-line-below ()
    "Insert a newline below the current line and put point at beginning."
    (interactive)
    (unless (eolp)
      (end-of-line))
    (newline-and-indent))

  (defun vi-open-line (&optional abovep)
    "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
    (interactive "P")
    (if abovep
        (vi-open-line-above)
      (vi-open-line-below)))
  (if nil ;; this was the original binding but the [insert] key is too far away
      (define-key global-map [(meta insert)] 'vi-open-line)
    )
  (if t ;; I prefer this binding instead
      (define-key global-map (kbd "M-o") 'vi-open-line)
    )
  )

(progn;; http://www.emacswiki.org/emacs/ColumnMarker
  (require 'column-marker)
  (global-set-key [?\C-c ?c] 'column-marker-1)
  (add-hook 'java-mode-hook (lambda () (interactive) (column-marker-1 120)))
  )

(progn;; instructed to add the below lines from [http://orgmode.org/orgguide.pdf], section 1.3
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ; not needed since Emacs 22.2
  (add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb))

(progn
  (setq whichSolutionToUseForOrgModePDF 2) ;; set that to either 1 or 2
  (if (= whichSolutionToUseForOrgModePDF 1)
      (print "1st option for loading PDF files in org-mode in effect")
    (print "2nd option for loading PDF files in org-mode in effect")))

(progn
  (defun my-org-mode-hook ()
    "My hook for the web-mode"
    (setq org-hide-emphasis-markers nil)
    (if (= whichSolutionToUseForOrgModePDF 1) ;; http://stackoverflow.com/a/8836108
        (progn
          (delete '("\\.pdf\\'" . default) org-file-apps)
          (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))
          )
      ))
  (add-hook 'org-mode-hook 'my-org-mode-hook))

(if (= whichSolutionToUseForOrgModePDF 2) ;; http://stackoverflow.com/a/9116029/274677
    (eval-after-load "org"
      '(progn
         (if nil ;; this is messing up with my opening of txt files
             ;; .txt files aren't in the list initially, but in case that changes
             ;; in a future version of org, use if to avoid errors
             (if (assoc "\\.txt\\'" org-file-apps)
                 (setcdr (assoc "\\.txt\\'" org-file-apps) "emacs %s")
               (add-to-list 'org-file-apps '("\\.txt\\'" . "emacs %s") t)))
         ;; Change .pdf association directly within the alist
         (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s"))))

(progn ;; https://jpace.wordpress.com/2015/01/09/adding-groovy-emacs-mode/
    ;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
  (add-to-list 'load-path ".emacs.d/groovy-emacs-modes")
  (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
  (add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
  (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
  
    ;;; make Groovy mode electric by default.
  (add-hook 'groovy-mode-hook
            '(lambda ()
               (require 'groovy-electric)
               (groovy-electric-mode)))
  )

(setq nxml-child-indent 4)

(if nil
    (fset 'html-mode 'nxml-mode) ;; http://stackoverflow.com/a/144938 and remember that C-j enters a new line with proper indentation in nXML mode
  ;; see also: http://stackoverflow.com/a/6247579
  (progn ;; http://web-mode.org/
    (add-to-list 'load-path "~/.emacs.d/web-mode/")
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-current-column-highlight t)
    (load "web-mode")
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (defun my-web-mode-hook ()
      "Hooks for Web mode."
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-style-padding 1)
      (setq web-mode-script-padding 1)
      (setq web-mode-block-padding 0)
      )
    (add-hook 'web-mode-hook  'my-web-mode-hook)
    (eval-after-load "web-mode"
      '(set-face-background 'web-mode-current-element-highlight-face "color-88"))
    )
  )

;;(progn
;;    (require 'openwith)
;;    (openwith-mode t)
;;    (setq openwith-associations '(("\\.pdf\\'" "evince" (file))))
;;    (setq openwith-associations '(("\\.jpg\\'" "eog" (file))))
;;    (setq openwith-associations '(("\\.png\\'" "eog" (file))))
;;)

(progn
  (require 'openwith)
  (openwith-mode t)
  (setq openwith-associations '(
                                ("\\.pdf\\'" "evince" (file))
                                ("\\.mp3\\'" "xmms" (file))
                                ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
                                ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file))
                                ("\\.png\\'" "eog" (file)))))

(add-hook 'org-mode-hook ;; http://stackoverflow.com/a/1775652/274677
          (lambda ()
            (org-indent-mode t))
          t)
(add-hook 'org-mode-hook #'visual-line-mode)

(setq org-agenda-files (list "~/esac-rawdar/rawdar.org"))

(add-hook 'java-mode-hook #'hs-minor-mode)

(progn ;; http://stackoverflow.com/a/19625063/274677
  ;; requires that you also run "sudo apt-get install xsel"
  (defun copy-to-clipboard ()
    (interactive)
    (if (display-graphic-p)
        (progn
          (message "Yanked region to x-clipboard!")
          (call-interactively 'clipboard-kill-ring-save)
          )
      (if (region-active-p)
          (progn
            (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
            (message "Yanked region to clipboard!")
            (deactivate-mark))
        (message "No region active; can't yank to clipboard!")))
    )

  (defun paste-from-clipboard ()
    (interactive)
    (if (display-graphic-p)
        (progn
          (clipboard-yank)
          (message "graphics active")
          )
      (insert (shell-command-to-string "xsel -o -b"))
      )
    )

  (global-set-key [f8] 'copy-to-clipboard)
  (global-set-key [f9] 'paste-from-clipboard)
  )


(if nil                        ;; in the end switched it off because of some issues
    (add-hook 'LaTeX-mode-hook ;; http://emacs.stackexchange.com/q/17954/4003
          (lambda ()
            (progn
              (outline-minor-mode 1)
              (local-set-key "\C-c\C-c"
                             outline-mode-prefix-map)
              (local-set-key (kbd "TAB")
                             (lambda ()
                               (interactive)
                               (outline-toggle-children)))))))

  ;; see: http://stackoverflow.com/a/21065066/274677
  ;;      http://stackoverflow.com/a/21342883/274677
  (progn ; install melpa and tss package; TODO: move more packages to the MELPA install format
    ;; https://stable.melpa.org/#/getting-started

    (require 'package)
    (setq package-enable-at-startup nil)
    (mapc (lambda(p) (add-to-list 'package-archives p t))
          '(("marmalade" . "http://marmalade-repo.org/packages/")
            ("melpa" . "http://melpa.org/packages/")))
    ;;(package-refresh-contents) ;; uncomment this line every couple of months or so ...
    (package-initialize)

    ;; Bootstrap `use-package' ;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)

    (use-package typescript
      :ensure t)

    (use-package auto-complete
      :ensure t) 

    (use-package ac-js2
      :ensure t) ;; NB: I also had to do this:
                 ;;        just replace line 285 of ac-js2.el with this:
                 ;;               (eval '(ac-define-source "js2"
                 ;; see: https://github.com/ScottyB/ac-js2/issues/18#issuecomment-74518558
                 ;;      http://stackoverflow.com/q/26812853/274677

    (use-package js2-mode
      :mode "\\.js\\'"
      :init
      (setq js2-highlight-level 3) 
;;      (add-hook 'js2-mode-hook 'ac-js2-mode)
      (add-hook 'js2-mode-hook (lambda () (progn (auto-revert-mode)
                                             (custom-set-variables
                                              '(auto-revert-interval 0.1)))))
;;      (add-hook 'js2-mode-hook 'ac-js2-setup-auto-complete-mode)
      (message "js2-mode-config")
      :ensure t)

)


