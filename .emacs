(menu-bar-mode 0)
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(global-linum-mode)
(global-hl-line-mode 1)
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

(progn ;; color-theme
  (require  'color-theme)
  (eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-jsc-dark)))
)



