;;; nm.el --- Emacs interface to Gnome nmcli command

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes, hardware

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

(defvar nm/enabled nil
  "Whether nm is enabled or not.")

(defun nm/enable ()
  (shell-command-to-string "nmcli -t -f net-enabled nm enable true")
  (setq nm/enabled t))

(defun nm/disable ()
  (shell-command-to-string "nmcli -t -f net-enabled nm enable false")
  (setq nm/enabled nil))

(defun nm-toggle-enabled (&optional status)
  "Toggle whether networking is enabled or not."
  (interactive "p")
  (cond
    ((> status 0)
     (nm/enable))
    ((< status 1)
     (nm/disable))
    ((eq status nil)
     (if nm/enabled (nm/disable) (nm/enable)))))

(defun nm/connected ()
  (car
   (split-string
    (shell-command-to-string "nmcli -t -f name con status")
    "\n")))

(defun nm/list-aps ()
  (split-string
   (shell-command-to-string "nmcli -t -f name con list")
   "\n"))

(defun nm/disconnect (ap)
  (car
   (split-string
    (shell-command-to-string
     (format "nmcli -t -f name con down id \"%s\"" ap))
    "\n")))

(defun nm/connect (ap)
  (car
   (split-string
    (shell-command-to-string
     (format "nmcli -t -f name con up id \"%s\"" ap))
    "\n")))

(defun nm-disconnect ()
  "Disconnect from the current Access Point."
  (interactive)
  (let ((current-ap (nm/connected)))
    (nm/disconnect current-ap)))

(defun nm-connect (ap)
  "Connect to a specific AP."
  (interactive
   (list
    (let ((ap-list (nm/list-aps)))
      (completing-read "What access point? " ap-list nil t))))
  (let ((current-ap (nm/connected)))
    (if (equal ap current-ap)
        (message "nm: already connected to %s" ap)
        ;; Else let's try and connect to it
        (unwind-protect
             (nm/disconnect current-ap)
          (nm/connect ap)))))

(provide 'nm)
;;; nm.el ends here
