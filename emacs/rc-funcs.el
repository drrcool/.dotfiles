;; -*- lexical-binding: t; -*-

;; Window functions
(defun rc/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))
(defun rc/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))
;; Package enhancements
(defun rc/rg ()
  "Allows you to select a fold to ripgrep"
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively `counsel-rg)))

(defun rc/prettify-symbols-setup ()
  ;; checkboxes
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  ;; (push '("[X]" . "☒" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)

  ;; org-babel
  (push '("#+BEGIN_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+END_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+begin_src" . ?≫) prettify-symbols-alist)
  (push '("#+end_src" . ?≫) prettify-symbols-alist)

  (push '("#+BEGIN_QUOTE" . ?❝) prettify-symbols-alist)
  (push '("#+END_QUOTE" . ?❞) prettify-symbols-alist)

  ;; (push '("#+BEGIN_SRC python" . ) prettify-symbols-alist) ;; This is the Python symbol. Comes up weird for some reason
  (push '("#+RESULTS:" . ?≚ ) prettify-symbols-alist)

  ;; drawers
  (push '(":PROPERTIES:" . ?) prettify-symbols-alist)

  ;; tags
  ;; (push '(":Misc:" . "" ) prettify-symbols-alist)

  (prettify-symbols-mode))


(defun rc/save-and-close-this-buffer (buffer)
  "Saves and closes given buffer."
  (if (get-buffer buffer)
	  (let ((b (get-buffer buffer)))
		(save-buffer b)
		(kill-buffer b))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun rc/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun rc/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

