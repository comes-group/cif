# COMES Image Format

This is an image format designed to be as bad and inefficient as possible.
It is uncompressed, similar to BMP, and uses Polish words instead of binary
data. Even for storing numbers.

## Why?

No reason at all. This project exists solely for having fun, and learning parser
optimization techniques. For you, there's no reason at all to use CIF, unless
you want to rival my implementation :)

## Installing

CIF needs Nim 1.4.2.

```
nimble install cif
```

This will install two executables, the encoder `encif`, and decoder `decif`.
Usage:

```
encif [options] <input file> <output file>
decif [options] <input file> [output file]
```

Pass `--help` or `-h` to any one of those executables for more info.

## Using CIF as a library

I mean, come on. There are better image formats out there. Go use `stb_image`,
`nimPNG`, anything else, really.

…

Alright, fine, I'll show you how to use cif.nim in your program.

```nim
import std/streams

import cif

# To encode:
var output = openFileStream("out.cif", fmWrite)
output.encodeCif(width = 1920, height = 1080, channels = 4, flags = {cifPolish},
                 metadata = {"source": "my program"}, input = pixels)
# `channels` is the number of channels, and can be either 3 for RGB or
# 4 for RGBA.
# `metadata` is any sort of key-value metadata you want to embed in the file.
# Note that at the moment any flags other than cifPolish are unsupported.

# To decode:
var
  pixels: seq[uint8]
  width, height: Natural
  channels: 3..4
  flags: set[CifFlag]
  metadata: seq[(string, string)]

  input = openFileStream("in.cif", fmRead)

decodeCif(pixels, width, height, channels, flags, metadata, input)
```
