[![Build Status](https://travis-ci.com/twlz0ne/playonline.el.svg?branch=master)](https://travis-ci.com/twlz0ne/playonline.el)

# playonline.el

Play code with online playgrounds.

Currently support:

- [Go playground](https://play.golang.org/)     (official)
- [Rust playground](https://play.rust-lang.org) (official)
- [rextester](https://rextester.com)            (third-party)
- [mycompiler](https://www.mycompiler.io)       (third-party)
- [labstack](https://code.labstack.com)         (third-party)

The priority of official playground is higher than the third-party, it can be changed by modifying `playonline-ground-alist`.

## Installation

Clone this repository, then add the following to your `.emacs`:

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/playonline"))
(require 'playonline)
```

## Usage

```
M-x playonline
```

This function can be applied to:

- buffer
- region
- block (or region in block) ;; requires org / markdown mode
