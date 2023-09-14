
# Table of Contents

1.  [racebox-tools](#org53ade79)
    1.  [About](#org11d6cd9)
    2.  [TODOs](#orgfa96cdc)
        1.  [Replace dbus-tools with more efficient dbus equivalents](#org8b16672)
    3.  [REPL Examples](#orgc1898b8)
    4.  [Running as a Service](#orgc2a1218)
    5.  [Requirements](#org2639198)
        1.  [D-Bus](#org4381db0)
        2.  [Bluez](#org7bc67e7)
        3.  [A Bluetooth Lower Energy host](#org5c76d9d)
    6.  [Shell Scripts](#org3097b5c)
    7.  [License](#org1246a4e)
2.  [Plan](#orge7ade79)
    1.  [Fill in a TODO list.](#orga6989a5)


<a id="org53ade79"></a>

# racebox-tools


<a id="org11d6cd9"></a>

## About

A Common Lisp package and systemd service for logging Racebox Mini sensor data.


<a id="orgfa96cdc"></a>

## TODOs


<a id="org8b16672"></a>

### Replace dbus-tools with more efficient dbus equivalents

Many functions in the current implementation repeatedly use (dbus:with-open-bus () &#x2026;)
in the same call.  Change it to only opens the bus once and put all calls in a single open.


<a id="orgc1898b8"></a>

## REPL Examples

    (ql:quickload :racebox-tools)
    
    ;; Return a list of name/object pairs for each RaceBox Mini nearby.
    (racebox-tools:list-racebox-devices)
    
    ;; By default these all use the first RaceBox Mini in the list returned
    ;; by (racebox-tools:list-racebox-devices), but there's an optional parameter.
    
    ;; Disconnect first so connect won't error out.
    (racebox-tools:disconnect)
    (racebox-tools:connect)
    
    ;; Read metadata UUIDs for manufacturer, serial, hardware/firmware versions.
    (racebox-tools:read-metadata)
    
    ;; Get a racebox-mini-data-message structure holding the raw integer values
    ;; described in the RaceBox Mini Data Message
    (let ((racebox-value (racebox-tools:read-current-value)))
      (swank:inspect-in-emacs racebox-value))
    
    (racebox-tools:disconnect)

Connect to the first seen RaceBox:

    (ql:quickload :racebox-tools)
    (let ((my-racebox (first ;; First name/object pair
                       (first  ;; Get the name
                        (racebox-tools:list-racebox-devices)))))
      (racebox-tools:connect :device-name my-racebox)
      (racebox-tools:read-metadata :device-name my-racebox)
      (let ((result (racebox-tools:read-current-value :device-name my-racebox)))
        (swank:inspect-in-emacs result))
      (racebox-tools:disconnect :device-name my-racebox))


<a id="orgc2a1218"></a>

## Running as a Service

Make sure <file:///home/jeremiah/src/lisp/> is in <file:///home/jeremiah/.config/common-lisp/source-registry.conf.d/projects.conf>
with this code:

    (:tree (:home "src/lisp/"))

Clone racebox-tools and dbus-tools into <file:///home/jeremiah/src/lisp/>

    mkdir -p $HOME/src/lisp/
    cd $HOME/src/lisp/
    git clone git@github.com:jl2/racebox-tools.git
    git clone git@github.com:jl2/dbus-tools.git

Edit <./racebox-recorder.service> and replace 'jeremiah' with your username.

    
    sudo cp racebox-tools/racebox-recorder.service /etc/systemd/services/
    systemctl start racebox-recorder

After a second the RaceBox LED should shine blue and a new .db file should show up
in <file:///home/jeremiah/src/lisp/racebox-tools/databases>.


<a id="org2639198"></a>

## Requirements


<a id="org4381db0"></a>

### D-Bus


<a id="org7bc67e7"></a>

### Bluez


<a id="org5c76d9d"></a>

### A Bluetooth Lower Energy host


<a id="org3097b5c"></a>

## Shell Scripts

    # Connect interactively to RaceBox device
    sudo gatttool -t random -b <RaceboxAddress> -I
    # Type this in to connect:
    connect
    # Type this to read data:
    char-read-hnd f


<a id="org1246a4e"></a>

## License

ISC

Copyright (c) 2023 Jeremiah LaRocco <jeremiah<sub>larocco</sub>@fastmail.com>


<a id="orge7ade79"></a>

# Plan


<a id="orga6989a5"></a>

## TODO Fill in a TODO list.

