# play-code.el

Play code with online playgrounds.

Currently support:

- [rextester.com](https://rextester.com) (priority)
- [labstack.com](https://code.labstack.com)

The priority can be changed by modifying `play-code-ground-alist`.

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
