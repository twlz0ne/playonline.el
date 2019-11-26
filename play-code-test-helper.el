;;; play-code-test.el --- Helper for testing play-code.el -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Redefine `play-code' to apply proxy.
;;
;; Example of `proxy-conf.el':
;;
;;   (setq with-proxy-http-server "127.0.0.1:7890")
;;   (setq with-proxy-senders '(play-code-send-to-go-playground))
;;
(when (load "proxy-conf.el" t)
  (require 'with-proxy)
  (with-eval-after-load 'play-code
    (defun play-code (&optional beg end)
      "Play code online.

This function can be applied to:
- buffer
- region
- block (or region in block) ;; require org-mode / markdown
"
      (interactive "r")
      (pcase-let*
          ((`(,mode ,code ,bounds)
            (pcase major-mode
              (`org-mode (play-code-orgmode-src-block beg end))
              (`markdown-mode (play-code-markdown-src-block beg end))
              (_ (list major-mode
                       (if (region-active-p)
                           (buffer-substring-no-properties beg end)
                         (buffer-substring-no-properties (point-min) (point-max)))
                       nil))))
           (`(,lang ,sender ,wrapper)
            (play-code--get-lang-and-function
             (save-restriction
               (when bounds
                 (apply 'narrow-to-region bounds))
               (play-code--get-mode-alias mode)))))
        ;; {{{+++
        (when (memq sender (bound-and-true-p with-proxy-senders))
          (setq sender (let ((orig-sender sender))
                         (lambda (&rest args)
                           (with-proxy
                             (apply orig-sender args))))))
        ;; }}}+++
        (funcall sender lang (if wrapper
                                 (funcall wrapper code)
                               code))))))

(provide 'play-code-test-helper)

;;; play-code-test-helper.el ends here
