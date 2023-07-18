# Caser.el #

This package provides functions which change the case of text -- from `camelCase` to `dash-case` to `snake_case` and back.

## Functions ##

There are two sets of functions:

1. `*-dwim` functions are the most user-friendly. If the region is active, they act on the region. If given a prefix argument, they act on that many words forward (or, if negative, backward). Otherwise, they change the case of the single next word. These functions are `camelcase-dwim`, `dashcase-dwim`, and `snakecase-dwim`.
2. `*-word` functions don't act on the region. They take an optional prefix argument, and act on that many words. These functions are `camelcase-word`, `dashcase-word`, and `snakecase-word`.

## Bindings ##

We recommend binding these functions to relevant keys:

```emacs-lisp
(bind-key "M-C" #'camelcase-dwim)
(bind-key "M-S" #'snakecase-dwim)
(bind-key "M-D" #'dashcase-dwim)
```
