# diff-fold  [![test](https://github.com/szermatt/diff-fold/workflows/test/badge.svg)](https://github.com/szermatt/diff-fold/actions)

An Emacs minor mode for diff-mode that allows hiding and showing
(folding) files and hunks. The resulting behavior is quite similar to
magit.

## Prerequisites

Diff-fold requires at least Emacs 26.

## Installation

To install add it to your elisp path,
```
(require diff-fold)
```
then turn it on for all diff buffers with:
```
(add-hook 'diff-mode-hook 'turn-on-diff-fold)
```

By default, the minor mode binds hiding/showing behavior to TAB
and shift-TAB in `diff-fold-mode-map`.

diff-fold plays well with `diff-font-lock-prettify`, available under
Emacs 27.

## Diff Mode in read-only mode

If you use diff-mode in read-only mode, with `diff-default-read-only`
set to t, you'll want to add your own shortcuts directly to
`diff-mode-shared-map`.

## Setup Example

Full setup, with use-package and straight:

```elisp
(use-package diff-mode
  :hook ((diff-mode . turn-on-diff-fold)))

(use-package diff-fold
  :straight (:type git :repo "https://github.com/szermatt/diff-fold.git")
  :bind (:map diff-mode-shared-map
              ("H" . diff-fold-hide-all-files)
              ("h" . diff-fold-toggle)
              ("S" . diff-fold-show-all)
              ("TAB" . diff-fold-toggle)
              ("S-TAB" . diff-fold-toggle-file)
              ("<backtab>" . diff-fold-toggle-file)))
```

## Testing

Run `diff-fold_test.el` with ERT.

## License

This project is licensed under the GPLv2 - see the [license](license) file for details
