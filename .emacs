;; set this to the appropriate value in conjunction with the [~/environment/emax] script
;; (setq server-use-tcp t) ; listen on TCP sockets instead of on UNIX domain sockets
(menu-bar-mode 0)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/"))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "d6a00ef5e53adf9b6fe417d2b4404895f26210c52bb8716971be106550cea257" default)))
 '(inhibit-startup-screen t)
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lazy-highlight ((t (:foreground "white" :background "SteelBlue"))))
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




(setq initial-frame-alist '((top . 200) (left . 500)))


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


;; the following presumable enables clipboard sharing between the
;; Emacs buffer and other applications but strangely I have found
;; that to work even with the following line absent; still, it
;; doesn't hurt to have it.
(setq x-select-enable-clipboard t)


(add-to-list 'auto-mode-alist '("\\.xhtml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$"   . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.wsdl$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xjb$"   . nxml-mode))



(if
    (= emacs-major-version 23)
    (load "package") ;; NB: for Emacs 23 that means you need package.el on your load path.
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


(progn
  ;; see: http://stackoverflow.com/questions/1128927/how-to-scroll-line-by-line-in-gnu-emacs
  (setq scroll-step            1
        scroll-conservatively  10000)
  )


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
    "My hook for the org-mode"
    (setq org-hide-emphasis-markers nil)
    (if (= whichSolutionToUseForOrgModePDF 1) ;; http://stackoverflow.com/a/8836108
        (progn
          (delete '("\\.pdf\\'" . default) org-file-apps)
          (add-to-list 'org-file-apps '("\\.pdf\\'" . "okular %s"))
          (add-to-list 'org-file-apps '("\\.xls\\'" . "libreoffice %s"))          
          )
      ))
  (add-hook 'org-mode-hook 'my-org-mode-hook))

(if (= whichSolutionToUseForOrgModePDF 2) ;; http://stackoverflow.com/a/9116029/274677
    (eval-after-load "org"
      '(progn
         (add-to-list 'org-file-apps '("\\.xls\\'" . "libreoffice %s"))
         (add-to-list 'org-file-apps '("\\.pptx\\'" . "libreoffice %s"))
         (if nil ;; this is messing up with my opening of txt files
             ;; .txt files aren't in the list initially, but in case that changes
             ;; in a future version of org, use if to avoid errors
             (if (assoc "\\.txt\\'" org-file-apps)
                 (setcdr (assoc "\\.txt\\'" org-file-apps) "emacs %s")
               (add-to-list 'org-file-apps '("\\.txt\\'" . "emacs %s") t)))
         ;; Change .pdf association directly within the alist
         (setcdr (assoc "\\.pdf\\'" org-file-apps) "okular %s"))))



(setq nxml-child-indent 4)



(add-hook 'org-mode-hook ;; http://stackoverflow.com/a/1775652/274677
          (lambda ()
            (org-indent-mode t)
            )
          t)
(add-hook 'org-mode-hook #'visual-line-mode)

(setq org-agenda-files (list "~/esac-rawdar/rawdar.org"))

(add-hook 'java-mode-hook #'hs-minor-mode)



(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(progn ;; ibuffer stuff
  (setq ibuffer-formats                     ;; http://emacs.stackexchange.com/a/623/4003
        '((mark modified read-only " "
                (name 50 50 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))



(defun server-shutdown () ;; define function to shutdown emacs server instance
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )



(add-hook 'org-mode-hook ;; http://stackoverflow.com/a/1775652/274677
          (lambda ()
            (define-key org-mode-map "\M-q" 'toggle-truncate-lines) ;; http://superuser.com/q/299886/138891
            ))


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

  (setq ad-redefinition-action 'accept) ;; this is to silence: "`tramp-read-passwd' got redefined" according to: https://github.com/syl20bnr/spacemacs/issues/192

  ;; see: http://stackoverflow.com/a/21065066/274677
  ;;      http://stackoverflow.com/a/21342883/274677
  (progn ; install melpa and tss package; TODO: move more packages to the MELPA install format
    ;; https://stable.melpa.org/#/getting-started

    (require 'package)
    (setq package-enable-at-startup nil)
    (mapc (lambda(p) (add-to-list 'package-archives p t))
          '(("marmalade" . "http://marmalade-repo.org/packages/")
            ("melpa" . "http://melpa.org/packages/")
            ("org" . "http://orgmode.org/elpa/"))) ;; org-mode has its own repo, see: http://stackoverflow.com/a/14838150/274677
    ;;(package-refresh-contents) ;; uncomment this line every couple of months or so ...
    (when (< emacs-major-version 24) ; http://emacs.stackexchange.com/a/15274/4003
      ;; For important compatibility libraries like cl-lib
        (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
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
      :mode "\\.jsx?\\'" 
      :init
      (setq js2-highlight-level 3) 
;;      (add-hook 'js2-mode-hook 'ac-js2-mode)
      (add-hook 'js2-mode-hook (lambda () (progn (auto-revert-mode)
                                             (setq auto-revert-interval 0.1))))
;;      (add-hook 'js2-mode-hook 'ac-js2-setup-auto-complete-mode)
      (message "js2-mode-config")
      :ensure t)
                

    (use-package ace-window
      :init
      (global-set-key (kbd "C-x o") 'ace-window)
      :ensure t)

    (use-package zenburn-theme
      :init
      (load-theme 'zenburn)
      :ensure t)

    (use-package free-keys
      :init
      (global-set-key (kbd "C-h C-k") 'free-keys)
      :ensure t)

    (use-package helm
      :init
      (helm-mode 1)
      (progn ;; http://emacs.stackexchange.com/q/2867/4003
             ;; see also: http://tuhdo.github.io/helm-intro.html
          (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z
          (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
          (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
        )
      :ensure t)

    (use-package web-mode
      :init
      (progn
        (setq web-mode-enable-current-element-highlight t)
        (setq web-mode-enable-current-column-highlight t)
        (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
        (add-to-list 'auto-mode-alist '("\\.jsp\\'"  . web-mode))        
        (defun my-web-mode-hook ()
          "Hooks for Web mode."
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
      :ensure t)

    (use-package clojure-mode :ensure t)
    (use-package cider :ensure t)
    (if nil
        ;; case A: use the bindings that allow me to work with org-mode as well
        (use-package ace-jump-mode
         :ensure t
         :init
         (bind-key "C-c C-SPC" 'ace-jump-mode)
         (bind-key "C-c C-c C-SPC" 'ace-jump-char-mode)
         (bind-key "C-c C-c C-c C-SPC" 'ace-jump-line-mode)
        )
       ;; case B: use the bindings that allow me to maintain the same bindings as used in my RHEL 6 machine @ CfA
       (use-package ace-jump-mode
         :ensure t
         :init
         (bind-key "C-c SPC" 'ace-jump-mode)
         (bind-key "C-c C-c SPC" 'ace-jump-char-mode)
         (bind-key "C-c C-c C-c SPC" 'ace-jump-line-mode)
        )
      )


    (use-package openwith
      :ensure t
      :init
      (openwith-mode t)
      (setq openwith-associations '(
                                ("\\.pdf\\'" "okular" (file))
                                ("\\.mp3\\'" "xmms" (file))
                                ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
                                ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file))
                                ("\\.png\\'" "eog" (file))))
      )

    (use-package json-mode
      :ensure t
      :init
      (put 'downcase-region 'disabled nil))

    (use-package view) ;; http://www.emacswiki.org/emacs/HalfScrolling

    (use-package column-marker ;; http://www.emacswiki.org/emacs/ColumnMarker
      :init
      (add-hook 'java-mode-hook (lambda () (interactive) (column-marker-1 120)))      
      )

    (use-package repeat) ;; to repeat last command: "C-x z"; once pressed, additional 'z's will keep repeating it. (this appears to be on by default)

    (use-package anzu
      :ensure t
      :init
      (global-anzu-mode +1)
      )

    (if nil ;; 2016-12-23 I decided to stop using this package because it messed up with the M-g g goto-line binding and produced unstable results regarding the M-g binding
    (use-package folding ;; http://emacs.stackexchange.com/a/27093/4003
      :ensure t
      :config
      (folding-install-hooks)
      (defun my-always-fold ()
        (setq-local folded-file t))
      (advice-add #'folding-mode-find-file :before #'my-always-fold)
      (folding-add-to-marks-list 'js2-mode  "// {{{" "// }}}")
      (folding-add-to-marks-list 'css-mode  "/* {{{" "}}} /*")
      ;; below are modes for which folding does not really make sense; see this: http://emacs.stackexchange.com/q/28674/4003
      (let ((disabled-modes-for-folding '(json-mode fundamental-mode makefile-gmake-mode
                                          org-mode  web-mode conf-space-mode nxml-mode conf-javaprop-mode
                                          help-mode
           )))
        (dolist (x disabled-modes-for-folding)
          (folding-add-to-marks-list x "non-sensical-begin-mark" "non-sensical-begin-mark")
          ))
      )
    )

    (use-package volatile-highlights
    :config
    (volatile-highlights-mode t))

)




(progn ; organize my global keys and ensure they override others
  (defvar my-keys-minor-mode-map ;; http://stackoverflow.com/a/683575/274677
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-x C-j"  ) 'flash-crosshairs) ;;  C-x C-j overrides dired similar binding but it can also be accessed with with C-x d;
                                        ; (define-key map (kbd "M-g g"    ) 'goto-line)      ; this doesn't appear to be necessary
      (define-key map (kbd "C-x C-l"  ) 'join-line)      ; shadows a disabled command (lowercase region)
;      don't define the below four (4) as they are needed in org-mode for the move item / increase / decrease indentation functionality      
;      (define-key map (kbd "M-<left>" ) 'windmove-left)  ; move to left windnow
;      (define-key map (kbd "M-<right>") 'windmove-right) ; move to right window
;      (define-key map (kbd "M-<up>"   ) 'windmove-up)    ; move to upper window
;      (define-key map (kbd "M-<down>" ) 'windmove-down)  ; move to downer window
      (define-key map (kbd "M-j"      ) 'windmove-left)  ; move to left windnow
      (define-key map (kbd "M-;"      ) 'windmove-right) ; move to right window
      (define-key map (kbd "M-k"      ) 'windmove-up)    ; move to upper window
      (define-key map (kbd "M-l"      )'windmove-down)   ; move to downer window
      (define-key map (kbd "C-x C-k"  ) 'compile)
      (define-key map (kbd "C-x C-b"  ) 'ibuffer)        ; used to be: 'buffer-menu
      (define-key map (kbd "M-s M-s"  ) 'replace-string)
      (define-key map (kbd "C-M-k"    ) 'server-shutdown)
      (define-key map (kbd "C-v"      ) 'View-scroll-half-page-forward)  ;; http://www.emacswiki.org/emacs/HalfScrolling
      (define-key map (kbd "M-v"      ) 'View-scroll-half-page-backward) ;; --||--
      (define-key map (kbd "<f8>"     ) 'copy-to-clipboard)
      (define-key map (kbd "<f9>"     ) 'paste-from-clipboard)
      (define-key map (kbd "C-c c"    ) 'column-marker-1)
      map)
    "my-keys-minor-mode keymap.")


  (define-minor-mode my-keys-minor-mode
    "A minor mode so that my key settings override annoying major modes."
    :init-value t
    :lighter " my-keys")

  (my-keys-minor-mode 1)
  )

;; This is progressively added stuff that I would at some point have to move
;; back above the [use-package] blocks
(progn
  (defun describe-server-name ()
    (interactive)
    (message "%s" (symbol-value 'server-name)))
    (global-set-key  (kbd "C-h C-s") 'describe-server-name))

;; http://emacs.stackexchange.com/q/10414/4003
(defun toggle-maximize-buffer () "Toggle maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (set-window-configuration my-saved-window-configuration)
         (progn
           (setq my-saved-window-configuration (current-window-configuration))
           (delete-other-windows))))

(define-key my-keys-minor-mode-map (kbd "C-M-u") 'toggle-maximize-buffer)

