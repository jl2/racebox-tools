;; dbus.lisp

;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :racebox-tools)

(defun first-racebox-device ()
  "Return the name of the first RaceBox Mini device found."
  (dbt:managed-object-name (first (list-racebox-devices))))

(defun is-racebox-device (object)
  "Check if a bluetooth object is a RaceBox Mini device."
  (and (bluetooth:device-p object)
       (let* ((attributes (dbt:managed-object-value object))
              (properties (dbt:find-value attributes "org.bluez.Device1"))
              (name (dbt:find-value properties "Name")))
         (cl-ppcre:scan "^RaceBox Mini [0-9]+" name))))

(defun list-racebox-devices ()
  "Return a list of all known RaceBox Mini devices."
  (remove-if-not #'is-racebox-device
                 (bluetooth:list-objects)))

(defun connect (&key (device-name (first-racebox-device)))
  "Connect to a RaceBox."
  (dbt:invoke-method-simple :system
                            "org.bluez"
                            device-name
                            "org.bluez.Device1"
                            "Connect"))

(defun disconnect (&key (device-name (first-racebox-device)))
  "Disconnect "
  (dbt:invoke-method-simple :system
                            "org.bluez"
                            device-name
                            "org.bluez.Device1"
                            "Disconnect"))

(defun to-string (buffer)
  "Convert an octet buffer into a string."
  (declare (type vector buffer))
  ;; Chop the trailing 0 bytes
  (babel:octets-to-string (subseq buffer
                                  0 (search #(0)
                                            buffer))))

(defun read-metadata (&key (device-name (first-racebox-device)))
  "Return the type, serial number, firmware version, hardware version, and manufacturer."
  (loop :for key :in '(:type :serial :firmware-version :hardware-version :manufacturer)
        :for uuid :in '("00002a24-0000-1000-8000-00805f9b34fb"
                        "00002a25-0000-1000-8000-00805f9b34fb"
                        "00002a26-0000-1000-8000-00805f9b34fb"
                        "00002a27-0000-1000-8000-00805f9b34fb"
                        "00002a29-0000-1000-8000-00805f9b34fb")
        :collecting (cons key
                          (to-string
                           (bluetooth:read-gatt-characteristic-by-uuid device-name
                                                                       uuid)))))
(defun read-raw-value (&key (device-name (first-racebox-device)))
  "Read an octet buffer containing the most recent reading from specified device."
  (bluetooth:read-gatt-characteristic-by-uuid device-name
                                        "6e400003-b5a3-f393-e0a9-e50e24dcca9e"))



;; (dbus:define-dbus-object racebox-service
;;   (:path "/org/bluez/hci0/dev_D2_D6_A3_84_35_29/service000b/char000e"))
;; (dbus:define-dbus-signal-handler (racebox-service (wat))
;;     wat)

;; TODO Get notifications of incoming messages.
;; (dbus:define-dbus-object racebox-listener-service
;;   (:path "/org/jl2/RaceBoxService"))

;; (dbus:define-dbus-method (racebox-service ) () (:list)
;;   (:interface "org.jl2.RaceBoxService")
;;   (read-current-value))

;; (dbus:define-dbus-signal-handler (my-service on-signal) ((s :string))
;;   (:interface "org.adeht.MyService")
;;   (format t "Got signal with arg ~S~%" s))

;; (defun publish-example ()
;;   (handler-case
;;       (with-open-bus (bus (session-server-addresses))
;;         (format t "Bus connection name: ~A~%" (bus-name bus))
;;         (publish-objects bus))
;;     (end-of-file ()
;;       :disconnected-by-bus)))
