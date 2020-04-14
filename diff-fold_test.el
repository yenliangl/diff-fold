;;; Tests diff-fold.el -*- lexical-binding: t -*-
;;

(require 'diff)
(require 'diff-fold)
(require 'ert)
(require 'ert-x)

(defun test-diff-visible-buffer (&optional point-mark)
  "Returns the visible portion of the buffer.

If POINT-MARK is true, the point is indicated with []."
  (save-excursion
    (let ((start-pos (point)))
      (when point-mark
        (when (test-diff-invisible-p (point))
          (error "point at %s is invisible" (point)))
        (insert "[]"))
      (goto-char (point-min))
      (let ((invisible) (last 1) (sections))
        (while (< (point) (point-max))
          (when (not invisible)
          (push (buffer-substring-no-properties (point) last) sections))
          (setq invisible (test-diff-invisible-p (point)))
          (setq last (point))
          (goto-char (next-overlay-change (point))))
        (unless invisible
          (push (buffer-substring-no-properties (point-max) last) sections))
        (when point-mark (delete-region start-pos (+ start-pos 2)))
        (apply 'concat (nreverse sections))))))

(defun test-diff-invisible-p (pos)
  (memq 'diff-fold (mapcar
                    (lambda (o) (overlay-get o 'invisible))
                    (overlays-at pos))))

(defmacro test-diff-fold-with-buffer (args text &rest body)
  (declare (indent 1))
  `(ert-with-test-buffer ()
     (let ,(append '((diff-font-lock-prettify nil)
                     (diff-default-read-only nil)
                     (diff-mode-hook nil)
                     (diff-fold-mode-hook nil))
                   args)
       (insert ,text)
       (goto-char (point-min))
       (diff-mode)
       (diff-fold-mode 1)
       ,@body)))

(defconst test-diff-fold-single-file
  "--- words.txt 2020-03-22 10:33:52.000000000 +0100
+++ /dev/fd/63 2020-03-22 10:47:13.000000000 +0100
@@ -3,5 +3,5 @@
 access
 license
-beat
+audience
 deprive
 tick
@@ -41,5 +41,5 @@
 preference
 see
-spirit
+zinc
 huge
 articulate
")

(defconst test-diff-fold-multiple-files
  "diff --git a/more-words.txt b/more-words.txt
index 5c7b07f..1650f75 100644
--- a/more-words.txt
+++ b/more-words.txt
@@ -22,8 +22,8 @@ personality
 precede
 timber
 study
-social
-extort
+exhorting
+panda
 disability
 beard
 seem
@@ -33,8 +33,8 @@ paragraph
 like
 dawn
 plan
-reptile
+laptop
 explode
 girlfriend
-elaborate
+cotton
 day
diff --git a/words.txt b/words.txt
index 4507658..6431f8d 100644
--- a/words.txt
+++ b/words.txt
@@ -18,9 +18,9 @@ tape
 combine
 dash
 theater
-weather
+wheat
 censorship
-kitty
+reproduce
 serve
 tasty
 swallow
@@ -31,7 +31,7 @@ credit
 tradition
 joystick
 patience
-burn
+complex
 grant
 related
 poison
")

(ert-deftest test-diff-fold-hide-hunk ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-single-file
    (diff-hunk-next)
    (diff-fold-hide-hunk)
    (should
     (equal
      (test-diff-visible-buffer)
      "--- words.txt 2020-03-22 10:33:52.000000000 +0100
+++ /dev/fd/63 2020-03-22 10:47:13.000000000 +0100
@@ -3,5 +3,5 @@
@@ -41,5 +41,5 @@
 preference
 see
-spirit
+zinc
 huge
 articulate
"))))

(ert-deftest test-diff-fold-hide-last-hunk ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-single-file
    (diff-hunk-next 2)
    (diff-fold-hide-hunk)
    (should
     (equal
      (test-diff-visible-buffer)
      "--- words.txt 2020-03-22 10:33:52.000000000 +0100
+++ /dev/fd/63 2020-03-22 10:47:13.000000000 +0100
@@ -3,5 +3,5 @@
 access
 license
-beat
+audience
 deprive
 tick
@@ -41,5 +41,5 @@
"))))

(ert-deftest test-diff-fold-hide-all-hunks ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-single-file
    (diff-hunk-next 2)
    (diff-fold-hide-all-hunks)
    (should
     (equal
      (test-diff-visible-buffer)
      "--- words.txt 2020-03-22 10:33:52.000000000 +0100
+++ /dev/fd/63 2020-03-22 10:47:13.000000000 +0100
@@ -3,5 +3,5 @@
@@ -41,5 +41,5 @@
"))))

(ert-deftest test-diff-fold-hide-hunk-adjust-point ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-single-file
    (diff-hunk-next)
    (forward-line 2)
    ;; Point is inside the portion of the hunk that will be hidden. It
    ;; should be moved.
    (diff-fold-hide-all-hunks)
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "--- words.txt 2020-03-22 10:33:52.000000000 +0100
+++ /dev/fd/63 2020-03-22 10:47:13.000000000 +0100
[]@@ -3,5 +3,5 @@
@@ -41,5 +41,5 @@
"))))

(ert-deftest test-diff-fold-hide-hunk-adjust-point ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-single-file
    (diff-hunk-next)
    (forward-char 5)
    ;; Point is not inside the portion of the hunk that will be
    ;; hidden. It should be left alone.
    (diff-fold-hide-all-hunks)
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "--- words.txt 2020-03-22 10:33:52.000000000 +0100
+++ /dev/fd/63 2020-03-22 10:47:13.000000000 +0100
@@ -3[],5 +3,5 @@
@@ -41,5 +41,5 @@
"))))

(ert-deftest test-diff-fold-show-hunk ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-single-file
    (diff-hunk-next)
    (diff-fold-hide-hunk)
    (diff-hunk-next)
    (diff-fold-hide-hunk)
    (should
     (equal
      (test-diff-visible-buffer)
      "--- words.txt 2020-03-22 10:33:52.000000000 +0100
+++ /dev/fd/63 2020-03-22 10:47:13.000000000 +0100
@@ -3,5 +3,5 @@
@@ -41,5 +41,5 @@
"))
    (diff-fold-show-hunk)
    (should
     (equal
      (test-diff-visible-buffer)
      "--- words.txt 2020-03-22 10:33:52.000000000 +0100
+++ /dev/fd/63 2020-03-22 10:47:13.000000000 +0100
@@ -3,5 +3,5 @@
@@ -41,5 +41,5 @@
 preference
 see
-spirit
+zinc
 huge
 articulate
"))))

(ert-deftest test-diff-fold-hide-hunk-twice ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-single-file
    (diff-hunk-next)
    (diff-fold-hide-hunk)
    (diff-fold-hide-hunk)
    (diff-fold-show-hunk)
    (should
     (equal
      (test-diff-visible-buffer)
      test-diff-fold-single-file))))

(ert-deftest test-diff-fold-hide-file ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-single-file
    (diff-hunk-next 2)
    (diff-fold-hide-file)
    (should
     (equal
      (test-diff-visible-buffer)
      "--- words.txt 2020-03-22 10:33:52.000000000 +0100
"))))

(ert-deftest test-diff-fold-hide-second-file ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-multiple-files
    (diff-file-next 2)
    (diff-fold-hide-file)
    (should
     (equal
      (test-diff-visible-buffer)
      "diff --git a/more-words.txt b/more-words.txt
index 5c7b07f..1650f75 100644
--- a/more-words.txt
+++ b/more-words.txt
@@ -22,8 +22,8 @@ personality
 precede
 timber
 study
-social
-extort
+exhorting
+panda
 disability
 beard
 seem
@@ -33,8 +33,8 @@ paragraph
 like
 dawn
 plan
-reptile
+laptop
 explode
 girlfriend
-elaborate
+cotton
 day
diff --git a/words.txt b/words.txt
"))))

(ert-deftest test-diff-fold-hide-all-files ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-multiple-files
    (diff-hunk-next 2)
    (diff-fold-hide-all-files)
    (should
     (equal
      (test-diff-visible-buffer)
      "diff --git a/more-words.txt b/more-words.txt
diff --git a/words.txt b/words.txt
"))))

(ert-deftest test-diff-fold-hide-all-files-with-prettify ()
  (test-diff-fold-with-buffer ((diff-font-lock-prettify t))
      test-diff-fold-multiple-files
    (diff-fold-hide-all-files)
    (should
     (equal
      (test-diff-visible-buffer)
      "diff --git a/more-words.txt b/more-words.txt
index 5c7b07f..1650f75 100644
--- a/more-words.txt
+++ b/more-words.txt
diff --git a/words.txt b/words.txt
index 4507658..6431f8d 100644
--- a/words.txt
+++ b/words.txt
"))))


(ert-deftest test-diff-fold-hide-file-adjust-point ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-multiple-files
    (diff-hunk-next 2)
    (diff-fold-hide-all-files)
    (should
     (equal
      (test-diff-visible-buffer 'point-mark)
      "[]diff --git a/more-words.txt b/more-words.txt
diff --git a/words.txt b/words.txt
"))))


(ert-deftest test-diff-fold-temporarily-show-file ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-multiple-files
    (diff-fold-hide-all-hunks)
    (diff-file-next)
    (diff-fold-hide-all-hunks)
    (diff-fold-hide-all-files)
    (goto-char (point-min))
    (ert-simulate-command '(diff-hunk-next . nil))        
    (should
     (equal
      (test-diff-visible-buffer)
      "diff --git a/more-words.txt b/more-words.txt
index 5c7b07f..1650f75 100644
--- a/more-words.txt
+++ b/more-words.txt
@@ -22,8 +22,8 @@ personality
@@ -33,8 +33,8 @@ paragraph
diff --git a/words.txt b/words.txt
"))))

(ert-deftest test-diff-fold-diff-hunk-next-temporarily-shows-file ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-multiple-files
    (diff-fold-hide-all-hunks)
    (goto-char (point-max))
    (diff-fold-hide-all-hunks)
    (diff-fold-hide-all-files)
    (goto-char (point-min))

    (ert-simulate-command '(diff-hunk-next . nil))    
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "diff --git a/more-words.txt b/more-words.txt
index 5c7b07f..1650f75 100644
--- a/more-words.txt
+++ b/more-words.txt
[]@@ -22,8 +22,8 @@ personality
@@ -33,8 +33,8 @@ paragraph
diff --git a/words.txt b/words.txt
"))

    (ert-simulate-command '(diff-hunk-next . nil))
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "diff --git a/more-words.txt b/more-words.txt
index 5c7b07f..1650f75 100644
--- a/more-words.txt
+++ b/more-words.txt
@@ -22,8 +22,8 @@ personality
[]@@ -33,8 +33,8 @@ paragraph
diff --git a/words.txt b/words.txt
"))

    (ert-simulate-command '(diff-hunk-next . nil))
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
"diff --git a/more-words.txt b/more-words.txt
diff --git a/words.txt b/words.txt
index 4507658..6431f8d 100644
--- a/words.txt
+++ b/words.txt
[]@@ -18,9 +18,9 @@ tape
@@ -31,7 +31,7 @@ credit
"))

    (ert-simulate-command '(diff-hunk-prev . nil))
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
"diff --git a/more-words.txt b/more-words.txt
index 5c7b07f..1650f75 100644
--- a/more-words.txt
+++ b/more-words.txt
@@ -22,8 +22,8 @@ personality
[]@@ -33,8 +33,8 @@ paragraph
diff --git a/words.txt b/words.txt
"))))

(ert-deftest test-diff-fold-force-hide-temporarily-shown-file ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-multiple-files
    (diff-fold-hide-all-hunks)
    (goto-char (point-max))
    (diff-fold-hide-all-hunks)
    (diff-fold-hide-all-files)
    (goto-char (point-min))

    (ert-simulate-command '(diff-hunk-next . nil))    
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "diff --git a/more-words.txt b/more-words.txt
index 5c7b07f..1650f75 100644
--- a/more-words.txt
+++ b/more-words.txt
[]@@ -22,8 +22,8 @@ personality
@@ -33,8 +33,8 @@ paragraph
diff --git a/words.txt b/words.txt
"))

    (diff-fold-hide-all-files)
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "[]diff --git a/more-words.txt b/more-words.txt
diff --git a/words.txt b/words.txt
"))))

(ert-deftest test-diff-fold-kill-file ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-multiple-files
    (diff-hunk-next 2)
    (diff-fold-hide-all-files)
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "[]diff --git a/more-words.txt b/more-words.txt
diff --git a/words.txt b/words.txt
"))

    (ert-simulate-command '(diff-file-kill . nil))

    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "[]diff --git a/words.txt b/words.txt
"))))


(ert-deftest test-diff-fold-kill-hunk ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-single-file
    (diff-fold-hide-all-hunks)
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "--- words.txt 2020-03-22 10:33:52.000000000 +0100
+++ /dev/fd/63 2020-03-22 10:47:13.000000000 +0100
[]@@ -3,5 +3,5 @@
@@ -41,5 +41,5 @@
"))
    (ert-simulate-command '(diff-file-hunk . nil))
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "--- words.txt 2020-03-22 10:33:52.000000000 +0100
+++ /dev/fd/63 2020-03-22 10:47:13.000000000 +0100
[]@@ -41,5 +41,5 @@
"))))

(ert-deftest test-diff-fold-kill-hunk ()
  (test-diff-fold-with-buffer ()
      test-diff-fold-multiple-files
    (diff-hunk-next 2)
    (diff-fold-hide-all-files)
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "[]diff --git a/more-words.txt b/more-words.txt
diff --git a/words.txt b/words.txt
"))

    (ert-simulate-command '(diff-file-kill . nil))
    
    (should
     (equal
      (test-diff-visible-buffer 'mark-point)
      "[]diff --git a/words.txt b/words.txt
"))))


