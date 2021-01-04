import std/os
import std/parseopt
import std/streams
import std/strformat
import std/strutils

import stb_image/read as stbi

import cif

type
  CliError = object of ValueError

proc cliError(message: string) =
  raise newException(CliError, message)

let helpText = fmt"""
encif - Reference COMES Image Format™ Encoder

Usage: {getAppFilename().splitPath.tail} [options] <input file> [output file]

Input file defaults to stdin, output file defaults to stdout.

Options:
  -h --help                   show this
  -i --inputFile:<file>       set input file (alias for positional)
  -o --outputFile:<file>      set output file (alias for positional)
  -f --format:<flags>         set output CIF flags (see below)
  -m --meta:key=value         add metadata to the resulting file

CIF flags:
  Flags have to be comma separated, eg. `polish,compact,quadtree`.
  The default set of flags is `polish`. At least the language must be specified.

  polish                      use Polish words
  english                     use English words
  compact                     use arabic numbers instead of words
  quadtree                    quadtree mode (pixels are compressed)
""".strip

var
  inputFilename = ""
  outputFilename = ""
  flags = {cifPolish}
  metadata = @{
    "encoder": "cif.nim",
  }

proc parseFlags[T: enum](s: TaintedString): set[T] =
  for flag in s.split(','):
    result.incl(parseEnum[T](flag))

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
    of "f", "format": flags = parseFlags[CifFlag](value)
    of "m", "meta":
      let kv = value.split('=', 1)
      metadata.add((kv[0], kv[1]))
    else: cliError fmt"invalid option: '{key}'"
  of cmdEnd: break

var
  inputFile: File
  outputFile: FileStream
  width, height, channels: int

inputFile =
  if inputFilename.len > 0: open(inputFilename, fmRead)
  else: stdin
outputFile =
  if outputFilename.len > 0: openFileStream(outputFilename, fmWrite)
  else: newFileStream(stdout)

var pixels = stbi.loadFromFile(inputFile, width, height, channels, 0)
if channels notin 3..4:
  cliError fmt"Channel count in '{inputFilename}' unsupported"

outputFile.encodeCif(width, height, channels, flags, metadata, pixels)

close inputFile
close outputFile
