import std/os
import std/parseopt
import std/streams
import std/strformat
import std/strutils

when defined(cifBenchmark):
  import std/monotimes
  import std/times

import stb_image/write as stbiw

import cif

type
  CliError = object of ValueError

proc cliError(message: string) =
  raise newException(CliError, message)

let helpText = fmt"""
decif – Reference COMES Image Format™ Decoder

Usage: {getAppFilename().splitPath.tail} [options] <input file> <output file>

Options:
  -h --help                   show this
  -i --inputFile:<file>       set input file (alias for positional)
  -o --outputFile:<file>      set output file (alias for positional)
  -q --quality:<1..100>       set JPEG quality (only affects JPEG output)
  --dryRun                    don't export, only decode. used for benchmarking

The output format is inferred from the output file's extension, and can be
one of:
  .bmp, .png, .jpg, .tga
If an unknown extension is specified, decif will fall back to PNG.
""".strip

var
  inputFilename = ""
  outputFilename = ""
  jpegQuality: 1..100 = 80
  dry = false

for kind, key, value in getopt(commandLineParams()):
  case kind
  of cmdArgument:
    if inputFilename.len == 0: inputFilename = key
    elif outputFilename.len == 0: outputFilename = key
    else: cliError "expected 2 positional arguments"
  of cmdShortOption, cmdLongOption:
    case key
    of "h", "help": echo helpText; quit(0)
    of "i", "inputFile": inputFilename = value
    of "o", "outputFile": outputFilename = value
    of "q", "quality": jpegQuality = parseInt(value)
    of "dryRun": dry = true
    else: cliError fmt"invalid option: '{key}'"
  of cmdEnd: break

if inputFilename.len == 0:
  echo helpText
  quit(0)

if not dry and outputFilename.len == 0:
  outputFilename = inputFilename.changeFileExt(".png")
  stderr.writeLine "No output filename given. Outputting to ", outputFilename
  if fileExists(outputFilename):
    cliError fmt"no output filename given, and {outputFilename} already exists"

var
  inputFile: FileStream

  pixels: seq[uint8]
  width, height: Natural
  channels: 3..4
  flags: set[CifFlag]
  metadata: seq[(string, string)]

inputFile =
  if inputFilename.len > 0: openFileStream(inputFilename, fmRead)
  else: newFileStream(stdin)

stdout.writeLine "Decoding CIF. This might take a while."

decodeCif(pixels, width, height, channels, flags, metadata, inputFile)

assert pixels.len == width * height * channels

stdout.writeLine "Flags: ", flags
stdout.writeLine "Size: ", width, " × ", height, ", ", channels * 8, " bpp"
stdout.writeLine "Metadata: "
for (key, value) in metadata:
  stdout.writeLine " - ", key, ": ", value

if not dry:

  stdout.writeLine "Encoding user-specified format…"

  case outputFilename.splitFile.ext
  of ".bmp": writeBmp(outputFilename, width, height, channels, pixels)
  of ".png": writePng(outputFilename, width, height, channels, pixels)
  of ".jpg", ".jpeg":
    writeJpg(outputFilename, width, height, channels, pixels, jpegQuality)
  of ".tga": writeTga(outputFilename, width, height, channels, pixels)

  stdout.writeLine "Encoding done."

