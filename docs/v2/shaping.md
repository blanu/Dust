# Shaping layer for Dust 2.5

## Overview

The shaping layer mediates between a raw stream connection which is expected to transmit statistically uniform
bytes in both directions and a shaped stream connection conforming to the properties of a model.  In the Dust
stack, it mediates between uniform streams and visible streams.

Properties modeled include packet inter-arrival times, fixed byte strings, and probability distributions of
transmitted bytes.

[XXX]