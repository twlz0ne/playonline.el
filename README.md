# play-code.el

Play code with online playgrounds.

Currently support:

- [rextester.com](rextester.com) (priority)
- [labstack.com](code.labstack.com)

The priority can be changed by modifying `play-code-ground-alist`.

## Installation

Clone this repository, then add the following to your `.emacs`:

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/play-code"))
(require 'play-code)
```

## Functions

- `play-code-region`
- `play-code-buffer`
- `play-code-block` (require org-mode / markdown)
