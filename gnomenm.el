;;; gnomenm.el --- Emacs interface to Gnome nmcli command

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes, hardware
;; Version: 0.0.1

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

;;; Commentary:

;; nmcli is a pain to use so here's a simple Emacs interface.

;;; Code:

(defvar gnomenm/enabled nil
  "Whether gnomenm is enabled or not.")

(defun gnomenm/enable ()
  (shell-command-to-string "nmcli -t -f net-enabled nm enable true")
  (message "gnomenm network enabled")
  (setq nm/enabled t))

(defun gnomenm/disable ()
  (shell-command-to-string "nmcli -t -f net-enabled nm enable false")
  (message "gnomenm network disabled")
  (setq gnomenm/enabled nil))

(defun gnomenm-status ()
  "What's the network status?"
  (interactive)
  (message "gnomenm network is %s"
           (if gnomenm/enabled "on" "off")))

(defun gnomenm-toggle-enabled (&optional status)
  "Toggle whether networking is enabled or not."
  (interactive "p")
  (cond
    ((> status 0)
     (gnomenm/enable))
    ((< status 1)
     (gnomenm/disable))
    ((eq status nil)
     (if gnomenm/enabled (gnomenm/disable) (gnomenm/enable)))))

(defun gnomenm/connected ()
  (car
   (split-string
    (shell-command-to-string "nmcli -t -f name con status")
    "\n")))

(defun gnomenm/list-aps ()
  (split-string
   (shell-command-to-string "nmcli -t -f name con list")
   "\n"))

(defun gnomenm/disconnect (ap)
  (car
   (split-string
    (shell-command-to-string
     (format "nmcli -t -f name con down id \"%s\"" ap))
    "\n")))

(defun gnomenm/connect (ap)
  (car
   (split-string
    (shell-command-to-string
     (format "nmcli -t -f name con up id \"%s\"" ap))
    "\n")))

(defun gnomenm-disconnect ()
  "Disconnect from the current Access Point."
  (interactive)
  (let ((current-ap (gnomenm/connected)))
    (gnomenm/disconnect current-ap)))

(defun gnomenm-connect (ap)
  "Connect to a specific AP."
  (interactive
   (list
    (let ((ap-list (gnomenm/list-aps)))
      (completing-read "What access point? " ap-list nil t))))
  (let ((current-ap (gnomenm/connected)))
    (if (equal ap current-ap)
        (message "nm: already connected to %s" ap)
        ;; Else let's try and connect to it
        (unwind-protect
             (gnomenm/disconnect current-ap)
          (gnomenm/connect ap)))))

(provide 'gnomenm)

;;; gnomenm.el ends here
