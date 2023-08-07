# Caser.el #

This package provides functions which change the case of text -- from `camelCase` to `dash-case` to `snake_case` and back.

## Bindings ##

We recommend binding these functions to relevant keys:

```emacs-lisp
(bind-key "M-C" #'camelcase-dwim)
(bind-key "M-S" #'snakecase-dwim)
(bind-key "M-D" #'dashcase-dwim)
```

## Non-ASCII Support ##

Because this package uses standard Emacs [regexp character classes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Char-Classes.html) and [case conversion functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Case-Conversion.html), it fully supports changing the case of languages that don't use only ASCII letters. Try `orð á íslensku` (Icelandic), or `Deutsch ist ähnlich` (German).

Although languages with no capitalization can't be camelCased, they can still be changed from dash-case to snake_case: try `한국어-단어` `한글-조선글` (Korean).

## Functions ##

There are two sets of functions. If you don't know which you want, prefer the `*-dwim` functions.

1. `*-dwim` functions are the most user-friendly. If the region is active, they act on the region. If given a prefix argument, they act on that many words forward (or, if negative, backward). Otherwise, they change the case of the single next word. These functions are `camelcase-dwim`, `dashcase-dwim`, and `snakecase-dwim`.
2. `*-word` functions don't act on the region. They take an optional prefix argument, and act on that many words. These functions are `camelcase-word`, `dashcase-word`, and `snakecase-word`.
