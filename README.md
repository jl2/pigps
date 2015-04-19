# pigps

pigps is a Common Lisp library for using Adafruit's "Ultimate GPS", using my Common Lisp binding to wiringPi.

# Using

First, hook up the Ultimate GPS to your Raspberry Pi as explained [on the Adafruit website](https://learn.adafruit.com/adafruit-ultimate-gps-on-the-raspberry-pi/using-uart-instead-of-usb).


Next, get the code.  The easiest way is to clone the library into your ASDF/QuickLisp search path and load it with QuickLisp.

```bash
cd ~/src/lisp
git clone https://github.com/jl2/pigps
```

Next, start Slime (or just a REPL) as root:

```bash
sudo emacs --user $USER -f slime
```

Finally, load the library and get your current location:

```commonlisp
(ql:quickload 'pigps)

(pigps:current-location)
```

If everything is configured and working correctly, the result, a pigps:gps-datapoint object, should be printed.

# Notes
For the time being, this library ignores many of the NMEA fields returned by the Ultimate GPS, such as the VTG, GSA, and GSV fields.
