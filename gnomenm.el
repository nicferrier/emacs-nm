;;; gnomenm.el --- Emacs interface to Gnome nmcli command

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes, hardware
;; URL: http://github.com/nicferrier/emacs-nm
;; Version: 0.0.4

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
  "Turn on WIFI."
  (shell-command-to-string "nmcli -t -f net-enabled nm wifi on")
  (message "gnomenm wifi enabled")
  (setq gnomenm/enabled t))

(defun gnomenm/disable ()
  "Turn off WIFI."
  (shell-command-to-string "nmcli -t -f net-enabled nm wifi off")
  (message "gnomenm wifi disabled")
  (setq gnomenm/enabled nil))

(defun gnomenm-status ()
  "What's the network status?"
  (interactive)
  (message "gnomenm network is %s"
           (if gnomenm/enabled "on" "off")))

;;;###autoload
(defun gnomenm-toggle-enabled (&optional status)
  "Toggle whether WIFI is enabled or not."
  (interactive "P")
  (cond
    ((null status)
     (if gnomenm/enabled (gnomenm/disable) (gnomenm/enable)))
    ((> (prefix-numeric-value status) 0)
     (gnomenm/enable))
    ((< (prefix-numeric-value status) 1)
     (gnomenm/disable))))

(defun gnomenm/connected ()
  "What AP are we currently connected to?"
  (car
   (split-string
    (shell-command-to-string "nmcli -t -f name con status")
    "\n")))

(defun gnomenm/list-aps ()
  "Make a list of all APs."
  (split-string
   (shell-command-to-string "nmcli -t -f name con list")
   "\n"))

(defun gnomenm/disconnect (ap)
  "Disconnect from the specified AP."
  (car
   (split-string
    (shell-command-to-string
     (format "nmcli -t -f name con down id \"%s\"" ap))
    "\n")))


(defvar gnomenm/connect-history '()
  "History of all you have connected to.")

(defun gnomenm/connect (ap)
  "Connect to the specified AP."
  (car
   (split-string
    (shell-command-to-string
     (format "nmcli -t -f name con up id \"%s\"" ap))
    "\n"))
  (add-to-list 'gnomenm/connect-history ap))

;;;###autoload
(defun gnomenm-disconnect ()
  "Disconnect from the current Access Point."
  (interactive)
  (let ((current-ap (gnomenm/connected)))
    (gnomenm/disconnect current-ap)))

(defvar gnomenm-connect-history nil
  "The history of APs you've connected to.")

;;;###autoload
(defun gnomenm-connect (ap)
  "Connect to a specific AP."
  (interactive
   (list
    (let ((ap-list (gnomenm/list-aps)))
      (completing-read
       "What access point? " ap-list nil t
       (if gnomenm-connect-history
           (car gnomenm-connect-history)
           nil)
       'gnomenm-connect-history))))
  (let ((current-ap (gnomenm/connected)))
    (if (equal ap current-ap)
        (message "nm: already connected to %s" ap)
        ;; Else let's try and connect to it
        (unwind-protect
             (gnomenm/disconnect current-ap)
          (gnomenm/connect ap)))))

;;;###autoload
(defun gnomenm-flip ()
  "Flip the AP to the last but one connected to.

If you don't have two APs in the history it does nothing.

This is really useful if you switch between a pair of APs like I
do.  I recommend using a keychord like:

 (key-chord-define-global \"90\"  'gnomenm-flip)

See http://www.emacswiki.org/KeyChord for details on KeyChord."
  (interactive)
  (let ((ap (cadr gnomenm/connect-history)))
    (when ap
      (gnomenm-connect ap))))

(provide 'gnomenm)

;;; gnomenm.el ends here
