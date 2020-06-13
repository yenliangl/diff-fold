;;; diff-fold.el --- Show/hide hunks and file in diff-mode.  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmail.com>
;; Maintainer: Stephane Zermatten <szermatt@gmail.com>
;; Version: 0.1
;; Keywords: diff vc
;; URL: http://github.com/szermatt/diff-fold
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;;
;; A minor mode that allows hiding and showing hunks and files in a
;; diff-mode buffer.
;;
;; To install add it to your elisp path,
;;   (require diff-fold)
;; then turn it on for all diff buffers with:
;;   (add-hook 'diff-mode-hook 'turn-on-diff-fold)
;;
;; By default, the minor mode binds hiding/showing behavior to TAB
;; and shift-TAB in `diff-fold-mode-map`.
;;
;; If you use diff-mode in read-only mode, with
;; `diff-default-read-only` set to t, you'll want to add your own
;; shortcuts directly to `diff-mode-shared-map`, for
;;
;; Under Emacs 27, diff-fold plays well with `diff-font-lock-prettify`.
;;
;; Setup example, with use-package and straight:
;;
;; (use-package diff-mode
;;   :hook ((diff-mode . turn-on-diff-fold)))
;;
;; (use-package diff-fold
;;   :straight (:type git :repo "https://github.com/szermatt/diff-fold.git")
;;   :bind (:map diff-mode-shared-map
;;               ("H" . diff-fold-hide-all-files)
;;               ("h" . diff-fold-toggle)
;;               ("S" . diff-fold-show-all)
;;               ("TAB" . diff-fold-toggle)
;;               ("S-TAB" . diff-fold-toggle-file)
;;               ("<backtab>" . diff-fold-toggle-file)))


(require 'diff-mode)

;;; Code:

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'let-alist))

(defvar diff-fold-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'diff-fold-toggle)
    (define-key map (kbd "S-TAB") 'diff-fold-toggle-file)
    (define-key map (kbd "<backtab>") 'diff-fold-toggle-file)
    map))

(define-minor-mode diff-fold-mode
  "Minor mode that adds support for folding to `diff-mode`."
  :group diff-mode
  :keymap diff-fold-mode-map
  (if diff-fold-mode
      (progn  ;; turn on
        (unless (eq major-mode 'diff-mode)
          (error "Diff-fold-mode only works in Diff Mode buffers"))
        (add-to-invisibility-spec 'diff-fold)
        (advice-add 'diff-hunk-next :after 'diff-fold--after-hunk-move)
        (advice-add 'diff-hunk-prev :after 'diff-fold--after-hunk-move)
        (advice-add 'diff-file-kill :around 'diff-fold--around-kill-file))
    ;; turn off
    (advice-remove 'diff-hunk-next 'diff-fold--after-hunk-move)
    (advice-remove 'diff-hunk-prev 'diff-fold--after-hunk-move)
    (advice-remove 'diff-file-kill 'diff-fold--around-kill-file)
    (remove-from-invisibility-spec 'diff-fold)
    (diff-fold-show-all)))

;;;###autoload
(defun turn-on-diff-fold ()
  "Turn on diff fold mode in the current buffer."
  (diff-fold-mode 1))

;;;###autoload
(defun diff-fold-has-multiple-files ()
  "Return t if the diff has multiple files.

Does nothing if `diff-fold-mode` is not enabled."
  (save-excursion
    (goto-char 1)
    (diff-end-of-file)
    (< (point) (1- (buffer-size)))))

;;;###autoload
(defun diff-fold-hide-all-files ()
  "Hide all files in the current diff.

Does nothing if `diff-fold-mode` is not enabled."
  (interactive)
  (when diff-fold-mode
    (let ((files-bounds))
      (save-excursion
        (goto-char 1)
        (while (< (point) (1- (buffer-size)))
          (let ((file-bounds (diff-fold-file)))
            (unless (diff-fold--invisible-p
                     (diff-fold--overlay file-bounds))
              (push file-bounds files-bounds))
            (goto-char (1+ (cdr (assq 'end file-bounds)))))))
      (dolist (files-bounds files-bounds)
        (diff-fold--hide files-bounds)))))

;;;###autoload
(defun diff-fold-hide-all-hunks ()
  "Hide all hunks in the current file."
  (interactive)
  (let-alist (diff-fold-file)
    (let ((hunks-bounds))
      (save-excursion
        (goto-char .start)
        (while (< (point) (1- .end))
          (let ((hunk-bounds (diff-fold-hunk)))
            (if (null hunk-bounds)
                (goto-char (line-beginning-position 2))
              (unless (diff-fold--invisible-p
                       (diff-fold--overlay hunk-bounds))
                (push hunk-bounds hunks-bounds))
              (goto-char (1+ (cdr (assq 'end hunk-bounds))))))))
      (dolist (hunk-bound hunks-bounds)
        (diff-fold--hide hunk-bound)))))

;;;###autoload
(defun diff-fold-show-all ()
  "Show all files and hunks that were hidden."
  (interactive)
  (diff-fold--delete-overlays 1 (1+ (buffer-size))))

;;;###autoload
(defun diff-fold-hide-hunk ()
  "Hide the hunk at point.

Fails if not within a hunk."
  (interactive)
  (if-let ((hunk (diff-fold-hunk)))
      (diff-fold--hide hunk)
    (error "No hunk at point")))

;;;###autoload
(defun diff-fold-show-hunk ()
  "Show the hunk at point.

Does nothing if not at a hidden hunk."
  (interactive)
  (when-let ((hunk (diff-fold-hunk)))
    (diff-fold--show hunk)))

;;;###autoload
(defun diff-fold-hide-file ()
  "Hide the file at point.

Fails if not within a file"
  (interactive)
  (if-let ((file (diff-fold-file)))
      (diff-fold--hide file)
    (error "No file at point")))

;;;###autoload
(defun diff-fold-show-file ()
  "Show the file at point.

Does nothing if not at a hidden file."
  (interactive)
  (when-let ((file (diff-fold-file)))
    (diff-fold--show file)))

;;;###autoload
(defun diff-fold-toggle ()
  "Toggle folding of the file or hunk at point."
  (interactive)
  (diff-fold--toggle (diff-fold-at-point)))

;;;###autoload
(defun diff-fold-toggle-file ()
  "Toggle folding of the file at point."
  (interactive)
  (diff-fold--toggle (diff-fold-file)))

(defun diff-fold-at-point ()
  "Return the bounds of the file or hunk at point."
  (or (diff-fold-hunk) (diff-fold-file)))

;;;###autoload
(defun diff-fold-hide ()
  "Hide the file or hunk at point."
  (interactive)
  (diff-fold--hide (diff-fold-at-point)))

;;;###autoload
(defun diff-fold-show ()
  "Show the file or hunk at point."
  (interactive)
  (diff-fold--show (diff-fold-at-point)))

(defun diff-fold--toggle (bounds)
  "Toggle BOUNDS."
  (let ((overlay (diff-fold--overlay bounds)))
    (if (diff-fold--invisible-p overlay)
        (delete-overlay overlay)
      (diff-fold--hide bounds overlay))))

(defun diff-fold--invisible-p (overlay)
  "Check whether OVERLAY is currently invisible."
  (and overlay (overlay-get overlay 'invisible)))

(defun diff-fold--hide (bounds &optional overlay)
  "Hide section within BOUNDS, 'file or 'hunk.

If available, OVERLAY should be the overlay found for BOUNDS."
  (let-alist bounds
    (let ((header-end (diff-fold--header-end .type .start)))
      (when (and (>= (point) header-end) (< (point) .end))
        (goto-char .start))
      (if-let ((hunk-ov (or overlay (diff-fold--overlay bounds))))
          ;; hide it again if it was shown temporarily
          (overlay-put hunk-ov 'invisible 'diff-fold)
        (let ((hunk-ov (make-overlay (1+ header-end) .end)))
          (overlay-put hunk-ov 'invisible 'diff-fold)
          (overlay-put hunk-ov 'diff-fold .type)
          (overlay-put hunk-ov 'evaporate t)
          (overlay-put hunk-ov 'isearch-open-invisible
                       'delete-overlay))))))

(defun diff-fold--header-end (type start)
  "Return the end position of the header of the section TYPE at START."
  (save-excursion
    (goto-char start)
    (if (and (symbol-value 'diff-font-lock-prettify) (eq 'file type))
        (progn
          (diff-beginning-of-hunk 2)
          (- (point) 1))
      (line-end-position))))

(defun diff-fold--show (bounds)
  "Show section within BOUNDS."
  (when-let ((overlay (diff-fold--overlay bounds)))
    (delete-overlay overlay)))

(defun diff-fold--overlay (bounds)
  "Return the overlay for section in BOUNDS."
  (let-alist bounds
    (diff-fold--overlay-at (1- .end) .type)))

(defun diff-fold--overlay-at (pos type)
  "Find an overlay at POS of the given TYPE."
  (let ((overlays (overlays-at pos))
        found)
    (while (and
            overlays
            (not
             (setq found
                   (let ((overlay (car overlays)))
                     (when (eq type (overlay-get overlay 'diff-fold))
                       overlay)))))
      (setq overlays (cdr overlays)))
    found))

(defun diff-fold--delete-overlays (from to)
  "Delete all overlays installed by diff-fold between FROM and TO."
  (dolist (ov (overlays-in from to))
    (when (overlay-get ov 'diff-fold)
      (delete-overlay ov))))

(defun diff-fold-hunk ()
  "Return the bounds of a hunk at point, if any."
  (save-excursion
    (let ((pos (point)) (hunk-start) (hunk-end))
      (setq hunk-start (condition-case nil
                           (diff-beginning-of-hunk t)
                         (error nil)))
      (when (and hunk-start (<= hunk-start pos))
        (diff-end-of-hunk)
        (setq hunk-end (point))
        (when (> hunk-end pos)
          `((start . ,hunk-start)
            (end . ,hunk-end)
            (type . hunk)))))))

(defun diff-fold-file ()
  "Return the bounds of a file at point, if any."
  (save-excursion
    (let ((file-start))
      (diff-beginning-of-file-and-junk)
      (setq file-start (point))
      (diff-end-of-file)
      `((start . ,file-start)
        (end . ,(point))
        (type . file)))))

(defun diff-fold--after-hunk-move (&rest _)
  "When navigating to a hunk in a hidden file, temporarily show it.

This function meant to be used as an after advice"
  (when (memq this-command '(diff-hunk-next diff-hunk-prev))
    (when-let ((overlay (diff-fold--overlay-at (point) 'file)))
      (when (eq 'diff-fold (overlay-get overlay 'invisible))
        (overlay-put overlay 'invisible nil)
        ;; redisplay once the point has left the overlay
        (let ((redisplay-overlay-f))
          (setq redisplay-overlay-f
                (lambda ()
                  (unless (memq overlay (overlays-at (point)))
                    (overlay-put overlay 'invisible 'diff-fold)
                    (remove-hook 'post-command-hook redisplay-overlay-f))))
          (add-hook 'post-command-hook redisplay-overlay-f nil 'local))))))

(defun diff-fold--around-kill-file (func &rest args)
  "Go back to the beginning of the current file after killing it.

This function calls \(FUNC ARGS) to kill the file, so it can be
used as an around advice."
  (let-alist (diff-fold-file)
    (apply func args)
    (goto-char .start)))

(provide 'diff-fold)
;;; diff-fold.el ends here
