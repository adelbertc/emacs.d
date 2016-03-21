;;;;;;;;;;;;;;;
;;; Package ;;;
;;;;;;;;;;;;;;;
; (push '("marmalade" . "https://marmalade-repo.org/packages/")
;       package-archives)

(require 'package)
(setq
 use-package-always-ensure t
 package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                    ("org"   . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;
;;; Misc packages ;;;
;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode)

;;;;;;;;;;;;;;
;;; Colors ;;;
;;;;;;;;;;;;;;
(use-package color-theme)
(use-package color-theme-solarized
  :config
  (progn
    (load-theme 'solarized t)
    (set-terminal-parameter nil 'background-mode 'dark)
    (setq frame-background-mode (quote dark))))

(use-package linum
  :config
  (progn
    (global-linum-mode 1)
    (set-face-background 'linum "black")
    (set-face-foreground 'linum "brightgreen")))

;;;;;;;;;;;;;;;;;;;;
;;; company-mode ;;;
;;;;;;;;;;;;;;;;;;;;
(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil))

;;;;;;;;;;;;;
;;; Emacs ;;;
;;;;;;;;;;;;;
(setq
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 backup-directory-alist `((".*" . ,temporary-file-directory))
 column-number-mode t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 linum-format 'format-linum)

(setq-default
 indent-tabs-mode nil)

(define-key global-map (kbd "RET") 'newline-and-indent)
(fset `yes-or-no-p `y-or-n-p)
(menu-bar-mode -1)

; Format linum padding - count # of characters in the # of lines and pad
(defun format-linum (line)
  (propertize
   (format
    (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
      (concat "%" (number-to-string w) "d "))
    line)
   'face 'linum))

; (electric-indent-mode 0)
(electric-pair-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;
;;; evil-mode ;;;
;;;;;;;;;;;;;;;;;
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (progn
    (evil-mode t)
    ; Let electric-pair do it's job
    (define-key evil-insert-state-map [remap newline] nil)
    (define-key evil-insert-state-map [remap newline-and-indent] nil)))

(use-package evil-leader
  :config
  (progn
   (global-evil-leader-mode)
   (evil-leader/set-leader ",")
   ; General
   (evil-leader/set-key "d" (lambda () (interactive) (split-window-right) (other-window 1)))
   (evil-leader/set-key "s" (lambda () (interactive) (split-window-below) (other-window 1)))
   (evil-leader/set-key "w" 'other-window)
   ; Projectile
   (evil-leader/set-key "b" 'projectile-dired)
   (evil-leader/set-key "e" 'projectile-find-file)
   (evil-leader/set-key "g" 'projectile-find-file-in-known-projects)
   (evil-leader/set-key "v" 'projectile-switch-project)
   ; Scala
   (evil-leader/set-key "p" (lambda () (interactive) (sbt-start) (previous-buffer)))
   (evil-leader/set-key "r" 'previous-error)
   (evil-leader/set-key "f" 'next-error)
   (evil-leader/set-key "c" 'toggle-sbt)
   (evil-leader/set-key "q" 'sbt-command)))

;;;;;;;;;;;;;;;;;;
;;; Projectile ;;;
;;;;;;;;;;;;;;;;;;
(use-package projectile
  :config
  (progn
    (projectile-global-mode t)
    (setq projectile-project-root-files-functions '(projectile-root-top-down projectile-root-bottom-up projectile-root-top-down projectile-root-top-down-recurring))))

;;;;;;;;;;;;;
;;; Scala ;;;
;;;;;;;;;;;;;
(use-package scala-mode2
  :config
  (progn
    (setq scala-indent:align-forms t)
    (setq scala-indent:align-parameters t)
    (setq scala-indent:indent-value-expression t)

    (set-face-attribute font-lock-constant-face t        :foreground "red")
    (set-face-attribute font-lock-doc-face t             :foreground "brightgreen" :slant 'italic)
    (set-face-attribute scala-font-lock:implicit-face t  :foreground "magenta")
    (set-face-attribute scala-font-lock:override-face t  :foreground "color-52")
    (setq modifier-color "brightred")
    (set-face-attribute scala-font-lock:final-face t     :foreground modifier-color)
    (set-face-attribute scala-font-lock:lazy-face t      :foreground modifier-color)
    (set-face-attribute scala-font-lock:private-face t   :foreground modifier-color)
    (set-face-attribute scala-font-lock:protected-face t :foreground modifier-color)
    (set-face-attribute scala-font-lock:sealed-face t    :foreground modifier-color)))


(use-package sbt-mode
  :config
  (progn
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)

    (add-hook 'sbt-mode-hook 'sbt-prev-on-save)))

(defun sbt-buffer () (get-buffer (sbt:buffer-name)))

(defun sbt-prev-on-save ()
  (add-hook 'after-save-hook 'sbt-run-previous-command))

(defun switch-sbt-project (proj)
  (interactive (list (read-string "Switch to project: ")))
  (sbt-command (concat "project " proj))
  (message (concat "Switched to project " proj)))

(defun toggle-sbt ()
  (interactive)
  (cond ((eq (sbt-buffer) (window-buffer (selected-window)))  ; sbt is focused
         (delete-window))
        ((get-buffer-window (sbt-buffer))                     ; visible but not focused
         (switch-to-buffer-other-window (sbt-buffer))
         (delete-window))
        (t                                                    ; not visible
          (split-window-below)
          (other-window 1)
          (switch-to-buffer (sbt-buffer))
          (shrink-window 5)
          (other-window -1))
        ))

