# play-code.el

Play code with online playgrounds.

Currently support:

- [Go playground](https://play.golang.org/)     (offical)
- [Rust playground](https://play.rust-lang.org) (offical)
- [rextester](https://rextester.com)            (third-party)
- [labstack](https://code.labstack.com)         (third-party)

The priority of offical playground is higher than the third-party, it can be changed by modifying `play-code-ground-alist`.

## Installation

Clone this repository, then add the following to your `.emacs`:

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/play-code"))
(require 'play-code)
```

## Usage

```
M-x play-code
```

This function can be applied to:

- buffer
- region
- block (or region in block) ;; require org-mode / markdown
