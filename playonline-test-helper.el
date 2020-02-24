;;; playonline-test.el --- Helper for testing playonline.el -*- lexical-binding: t; -*-

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

;; Redefine `playonline' to apply proxy.
;;
;; Example of `proxy-conf.el':
;;
;;   (setq with-proxy-http-server "127.0.0.1:7890")
;;   (setq with-proxy-senders '(playonline-send-to-go-playground))
;;
(when (load "proxy-conf.el" t)
  (require 'with-proxy)
  (with-eval-after-load 'playonline
    (defun playonline (&optional beg end)
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
              (`org-mode (playonline-orgmode-src-block beg end))
              (`markdown-mode (playonline-markdown-src-block beg end))
              (_ (list major-mode
                       (if (region-active-p)
                           (buffer-substring-no-properties beg end)
                         (buffer-substring-no-properties (point-min) (point-max)))
                       nil))))
           (`(,lang ,sender ,wrapper)
            (playonline--get-lang-and-function
             (save-restriction
               (when bounds
                 (apply 'narrow-to-region bounds))
               (playonline--get-mode-alias mode)))))
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

(provide 'playonline-test-helper)

;;; playonline-test-helper.el ends here
