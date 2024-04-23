<!---
  Copyright 2024 Winford (Uncle Grumpy) <winford@object.stream>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# atomvm_dotstar

Welcome to the atomvm_dotstar AtomVM AP102 library.

This is an AtomVM native spi driver for the AP102 RGB LED written is pure Erlang.  This driver has been tested on the ESP32 platform, but should work on other microcontrollers, if an AtomVM SPI driver interface that supports `spi:write/3` transactions is added in the future.

Currently the driver only supports a single dot.  This is all I have available to test on, but supoport is planned for strips and matrix in the near future.

The AP102 uses just the SPI `sclk` and `mosi` pins on the SPI bus, and does not use a `cs` pin.  This should not pose a problem to other peripherals on the same bus, as long as they do use a `cs` pin, but it does mean that it is remotely possible that data transmissions intended for other peripherals _could_ cause a color change in the AP102 LED(s).  This is fairly unlikely, but not impossible.  The AP102 looks for a sequence of 32 (or more) `0` bits in a row, followed by 3 bits in a row of `1` (the next five bits set the brightness of the first LED), the next 24 bits are the color value of the first LED, additionally, any number of 32 bit LED (brightness + color) data packets may be included, followed by 32 bits of `1` to signal the end of the transmission.  If you are using a peripheral that might reqire similarly constructed SPI transmissions you should consider putting it on a different SPI bus, or accept that the LED may occasionally exhibit unexpected behavior.

See the examples folder for how use the driver use the driver.
