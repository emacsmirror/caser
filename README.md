# Caser.el #

This package provides functions which change the case of text -- from `camelCase` to `UpperCamelCase` to `dash-case` to `snake_case` and back.

[![builds.sr.ht status](https://builds.sr.ht/~zck/caser.el.svg)](https://builds.sr.ht/~zck/caser.el?)

## Bindings ##

We recommend binding these functions to relevant keys:

```emacs-lisp
(bind-key "M-C" #'caser-camelcase-dwim)
(bind-key "C-M-C" #'caser-upper-camelcase-dwim)
(bind-key "M-S" #'caser-snakecase-dwim)
(bind-key "M-D" #'caser-dashcase-dwim)
```

## Basic usage ##

Put some camelcase, snakecase, or dashcase text into a buffer. Then call one of the functions below.

* `caser-camelcase-dwim` changes text to camelCase
* `caser-upper-camelcase-dwim` changes text to UpperCamelCase
* `caser-snakecase-dwim` changes text to snake_case
* `caser-dashcase-dwim` changes text to dash-case.

## Dwim?

The functions Do What I Mean. Hopefully, that is.

1. When some text is selected, the `-dwim` functions act on the region.
1. When no text is selected, the `-dwim` functions act on the next word.
1. When there's a prefix argument, the `-dwim` functions act on that many words forward.

## Repeat case changes ##

Sometimes, it's easier to call a `dwim` function multiple times in a row, rather than select a region first. To make this easier, this mode supports [repeat-mode](https://karthinks.com/software/it-bears-repeating/). Repeat-mode is a way of easily repeating related commands.

To enable it, run this line:

```emacs-lisp
(repeat-mode 1)
```

Then, each function is able to be repeated by pressing a single letter. Once you run caser-snakecase-dwim, it can be ran gain with `s`. Similarly, caser-camelcase-dwim can be repeated with `c`; caser-upper-camelcase-dwim can be repeated with `c`; caser-dashcase-dwim can be repeated with `d`.

These keys can be customized; see variables `caser-snakecase-repeat-map`, `caser-camelcase-repeat-map`, and `caser-dashcase-repeat-map`.

## Non-ASCII Support ##

Because this package uses standard Emacs [regexp character classes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Char-Classes.html) and [case conversion functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Case-Conversion.html), it fully supports changing the case of languages that don't use only ASCII letters. Try `orð á íslensku` (Icelandic), or `Deutsch ist ähnlich` (German).

Although languages with no capitalization can't be camelCased, they can still be changed from dash-case to snake_case: try `한국어-단어` (Korean).

## Alternatives ##

This is not the first casing-related Emacs package. However, it works slightly differently than these existing packages.

* **[string-inflection.el](https://github.com/akicho8/string-inflection)** -- this package only supports ASCII characters.
* **[electric-case.el](https://github.com/zk-phi/electric-case)** -- this package converts what is typed in =dash-case= to =camelCase= or =snake_case=. It dosen't permit ad-hoc case changes.

## CI

CI is set up at builds.sr.ht. Hopefully [everything is passing](https://builds.sr.ht/~zck/caser.el).
