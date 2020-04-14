# diff-fold

An Emacs minor mode for diff-mode that allows hiding and showing
(folding) files and hunks. The resulting behavior is quite similar to
magit.

## Prerequisites

Diff-fold requires at least Emacs 26.

## Installation

To install, turn it on for all diff buffers with:

```elisp
(add-hook 'diff-mode-hook 'turn-on-diff-fold)
```

By default, the minor mode binds hiding/showing behavior to TAB
and shift-TAB in `diff-fold-mode-map`.

diff-fold plays well with `diff-font-lock-prettify`, available under
Emacs 27.

## Diff Mode in read-only mode

If use diff-mode in read-only mode, with `diff-default-read-only` set
to t, you'll want to add your own shortcuts directly to
`diff-mode-shared-map`.

Setup example, with use-package and `diff-default-read-only`:

```elisp
(use-package diff-mode
  :bind (:map diff-mode-shared-map
         ("H" . diff-fold-hide-all-files)
         ("h" . diff-fold-toggle)
         ("S" . diff-fold-show-all)
         ("TAB" . diff-fold-toggle)
         ("S-TAB" . diff-fold-toggle-file)
         ("<backtab>" . diff-fold-toggle-file))
 :hook ((diff-mode . turn-on-diff-fold))
 :config
 (setq diff-font-lock-prettify t
       diff-default-read-only t))
```

## Testing

Run `diff-fold_test.el` with ERT.

## License

This project is licensed under the GPLv2 - see the [license](license) file for details
