;;;;;;;;;;;;;;;
;;; Package ;;;
;;;;;;;;;;;;;;;
(require 'package)
(package-initialize)
(push '("marmalade" . "https://marmalade-repo.org/packages/")
      package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)
(push '("gnu" . "http://elpa.gnu.org/packages/")
      package-archives)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun ensure-installed (p)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;
;;; Requires ;;;
;;;;;;;;;;;;;;;;
(ensure-installed 'auto-complete)
(ensure-installed 'color-theme-solarized)
(ensure-installed 'evil)
(ensure-installed 'evil-leader)
(ensure-installed 'linum)
(ensure-installed 'projectile)
(ensure-installed 'sbt-mode)
(ensure-installed 'scala-mode2)

;;;;;;;;;;;;;;
;;; Colors ;;;
;;;;;;;;;;;;;;
(global-linum-mode 1)
(set-terminal-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

(setq frame-background-mode (quote dark))
(set-face-background 'linum "black")
(set-face-foreground 'linum "brightgreen")

;;;;;;;;;;;;;
;;; Emacs ;;;
;;;;;;;;;;;;;
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)  
(menu-bar-mode -1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; Indent on newline
(define-key global-map (kbd "RET") 'newline-and-indent)

; Spaces not tabs
(setq-default indent-tabs-mode nil)

(setq linum-format 'format-linum)

; Format linum padding - count # of characters in the # of lines and pad
(defun format-linum (line)
  (propertize (format
                (let
                  ((w (length (number-to-string (count-lines (point-min) (point-max))))))
                  (concat "%" (number-to-string w) "d "))
                line)
              'face 'linum))

(electric-pair-mode 1)
(show-paren-mode 1)

; Turn off shell echo
(require 'comint)
(defun no-shell-echo () (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'no-shell-echo)

(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete ;;;
;;;;;;;;;;;;;;;;;;;;;
(ac-config-default)

;;;;;;;;;;;;;;;;;
;;; evil-mode ;;;
;;;;;;;;;;;;;;;;;
(setq evil-want-C-u-scroll t)
(ensure-installed 'evil)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

; General
(evil-leader/set-key "d" (lambda () (interactive) (split-window-right) (other-window 1)))
(evil-leader/set-key "i" 'eval-expression)
(evil-leader/set-key "s" (lambda () (interactive) (split-window-below) (other-window 1)))
(evil-leader/set-key "v" 'shell)
(evil-leader/set-key "w" 'other-window)
(evil-leader/set-key "x" 'kill-this-buffer)

; Projectile
(evil-leader/set-key "b" 'projectile-dired)
(evil-leader/set-key "e" 'projectile-find-file)
(evil-leader/set-key "g" 'projectile-find-file-in-known-projects)
; (evil-leader/set-key "r" 'projectile-switch-project)
(evil-leader/set-key "t" 'projectile-find-file-other-window)
(evil-leader/set-key "m" 'add-projectile-project)
(evil-leader/set-key "n" 'projectile-remove-known-project)

; Scala
; Start SBT in a buffer and switch back to original
(evil-leader/set-key "p" (lambda () (interactive) (sbt-start) (previous-buffer)))
; Hack to stop current SBT command in evil-mode
(evil-leader/set-key "l" (lambda () (interactive) (sbt-command "") (other-window 1)))
(evil-leader/set-key "r" 'previous-error)
(evil-leader/set-key "f" 'next-error)
(evil-leader/set-key "c" 'toggle-sbt)
(evil-leader/set-key "q" 'sbt-command)
; (evil-leader/set-key "v" (lambda () (interactive) (sbt-command "compile")))
; (evil-leader/set-key "q" 'switch-sbt-project)

(evil-mode t)

; Let electric-pair do it's job
(define-key evil-insert-state-map [remap newline] nil)
(define-key evil-insert-state-map [remap newline-and-indent] nil)

;;;;;;;;;;;;;;;;;;
;;; Projectile ;;;
;;;;;;;;;;;;;;;;;;
(defun add-projectile-project (dir)
  (interactive (list (read-file-name "Add project root:")))
  (projectile-add-known-project dir)
  (projectile-merge-known-projects)
  (message "Project %s added to the list of known projects." dir))

;;;;;;;;;;;;;
;;; Scala ;;;
;;;;;;;;;;;;;
(require 'scala-mode2)
(require 'sbt-mode)

(add-hook 'sbt-mode-hook'(lambda ()
    (add-hook 'after-save-hook 'sbt-run-previous-command)
))

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

(defun sbt-buffer () (get-buffer (sbt:buffer-name)))

(defun switch-sbt-project (proj)
  (interactive (list (read-string "Switch to project: ")))
  (sbt-command (concat "project " proj))
  (message (concat "Switched to project " proj)))

(set-face-attribute font-lock-constant-face t        :foreground "red")
(set-face-attribute font-lock-doc-face t             :foreground "brightgreen" :slant 'italic)
(set-face-attribute scala-font-lock:implicit-face t  :foreground "magenta")
(set-face-attribute scala-font-lock:override-face t  :foreground "color-52")

(setq modifier-color "brightred")
(set-face-attribute scala-font-lock:final-face t     :foreground modifier-color)
(set-face-attribute scala-font-lock:lazy-face t      :foreground modifier-color)
(set-face-attribute scala-font-lock:private-face t   :foreground modifier-color)
(set-face-attribute scala-font-lock:protected-face t :foreground modifier-color)
(set-face-attribute scala-font-lock:sealed-face t    :foreground modifier-color)