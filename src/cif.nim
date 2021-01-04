## COMES Image Format™ reference en/decoder.
##
## The format
## ----------
##
## CIF is a text-based image format designed to be as shitty and verbose as
## possible.
##
## All CIF images are either 24bpp or 32bpp, as indicated in the header.
## Colors have 8 bits per channel, so RGB8 and RGBA8 formats are supported.
## CIF images may have any arbitrary resolution.
##
## Common rules
## ============
##
## .. code-block:
##
##  ws <- +' '
##  nl <- +'\n'
##  comma <- ',' * ws
##  semicolon <- ';' * ws
##  colon <- ':' * ws
##  magic <- "CIF"
##
## Language-specific keywords
## ==========================
##
## Depending on the language chosen, certain keywords change in the format.
##
## Polish
## ******
##
## .. code-block::
##
##  version <- "WERSJA"
##  size <- "ROZMIAR"
##  width <- "szerokość"
##  height <- "wysokość"
##  bpp <- "bitów_na_piksel"
##  metadata <- "METADANE"
##  node <- "WĘZEŁ"
##  branch <- "GAŁĄŹ"
##  leaf <- "LIŚĆ"
##
## English
## *******
##
## .. code-block::
##
##  version <- "VERSION"
##  size <- "SIZE"
##  width <- "width"
##  height <- "height"
##  bpp <- "bits_per_pixel"
##  metadata <- "METADATA"
##  node <- "NODE"
##  branch <- "BRANCH"
##  leaf <- "LEAF"
##
## Number encoding
## ===============
##
## Number encoding changes depending on flags passed to the coder.
##
## compact
## *******
##
## Compact should be able to parse any arbitrary 64-bit number.
##
## .. code-block::
##
##  digit <- {'0'..'9'}
##  int <- +digit
##
## polish
## ******
##
## Polish must be able to parse/reproduce numbers at least in the range 0..9999.
##
## *PEG code for the ``int`` rule is not included because it quickly gets really
## verbose. Implementation is an exercise for the reader :)*
##
## english
## *******
##
## English should be able to parse/reproduce numbers at least in the range
## 0..9999. `fourty` is preferred over `forty`, but both can be implemented for
## completeness (though `forty` is optional).
##
## Image files
## ===========
##
## Common header
## *************
##
## Every image file has a header which signifies metadata about the image.
## This header must appear at the very top of the file.
##
## .. code-block::
##
##  separatedList(sep, rule) <- ?(rule * *(sep * rule))
##
##  # Magic + flags must appear at the very first line of the file
##  flag <- "polish" | "english" | "compact" | "quadtree"
##  magicFlags <- magic * colon * separatedList(comma, flag) * nl
##
##  # Then the version
##  versiondef <- ver * ws * int * nl
##
##  # Then the image size
##  sizedef <-
##    size * ws *
##    width * colon * int * comma *
##    height * colon * int * comma *
##    bpp * colon * int *
##    nl
##
##  # Then any number of metadata definitions
##  metakey <- +{'a'..'z', 'A'..'Z', '0'..'9', '_'}
##  metavalue <- *(1 - nl)
##  metadatadef <- metadata * metakey * colon * metavalue * nl
##  metadatadefs <- *metadatadef
##
##  # This is the rule for the full header:
##  header <- magicFlags * versiondef * sizedef * metadatadefs * nl
##
## Pixel format
## ************
##
## Depending on whether the image is 24bpp or 32bpp, pixels may be encoded in
## one of two ways:
##
## .. code-block::
##
##  if bpp == 24:
##    pixel <- int * semicolon * int * semicolon * int
##  elif bpp == 32:
##    pixel <- int * semicolon * int * semicolon * int * semicolon * int
##
## Image data
## **********
##
## Depending on whether quadtree mode is used or not, pixel data may be encoded
## in one of the following ways:
##
## Stream mode
## ~~~~~~~~~~~
##
## Pixels in stream mode are arranged in top to bottom, left to right order.
##
## .. code-block::
##
##  imageData <- *(pixel * nl)
##
## Quadtree mode
## ~~~~~~~~~~~~~
##
## When quadtree mode is enabled, while coding the image the read/write buffer
## must be enlarged to the nearest power of two. Anything out of bounds of the
## image size is expected to be solid black (0, 0, 0) in 24bpp mode or
## transparent (0, 0, 0, 0) in 32bpp mode.
##
## .. code-block::
##
##  # Quadtrees are built out of branch nodes and leaf nodes.
##  # Each branch node has 4 children nodes that may be other branch nodes or
##  # leaf nodes.
##  # Each leaf node encodes a solid pixel color.
##
##  branchNode <-
##    node * ws * branch * nl *
##    anyNode * nl *
##    anyNode * nl *
##    anyNode * nl *
##    anyNode * nl
##
##  leafNode <- node * ws * leaf * ws * pixel
##
##  anyNode <- leafNode | branchNode
##
##  imageData <- anyNode * nl
##
## Tying it all together
## *********************
##
## Given the previously defined parsing rules, this is how a CIF file should be
## read:
##
## .. code-block::
##
##   cif <- header * anyNode

import std/algorithm
import std/macros
import std/streams
import std/strformat
import std/strutils
import std/tables

type
  CifFlag* = enum
    cifPolish = "polish"
    cifEnglish = "english"
    cifCompact = "compact"
    cifQuadtree = "quadtree"

  CifError* = object of ValueError

type
  Keyword = enum
    kwVersion
    kwSize
    kwWidth
    kwHeight
    kwBpp
    kwMetadata
    kwNode
    kwBranch
    kwLeaf

  Dictionary = array[Keyword, string]

const
  CifFormatVersion* = 1

  Magic = "CIF"
  PolishKeywords = [
    kwVersion: "WERSJA",
    kwSize: "ROZMIAR",
    kwWidth: "szerokość",
    kwHeight: "wysokość",
    kwBpp: "bitów_na_piksel",
    kwMetadata: "METADANE",
    kwNode: "WĘZEŁ",
    kwBranch: "GAŁĄŹ",
    kwLeaf: "LIŚĆ",
  ]
  EnglishKeywords = [
    kwVersion: "VERSION",
    kwSize: "SIZE",
    kwWidth: "width",
    kwHeight: "height",
    kwBpp: "bits_per_pixels",
    kwMetadata: "METADATA",
    kwNode: "NODE",
    kwBranch: "BRANCH",
    kwLeaf: "LEAF",
  ]

  MetadataKeyChars = {'a'..'z', 'A'..'Z', '0'..'9', '_'}
  MetadataValueChars = {'\x00'..'\xff'} - {'\n'}



# encoder

type
  ToLocInt = proc (i: int): string {.nimcall.}

proc cifAssert(cond: bool, message: string) =
  if not cond:
    raise newException(CifError, message)

proc `$`(flags: set[CifFlag]): string =
  for flag in flags:
    result.addSep(",", 1)
    result.add($flag)

const
  plOnes = [
    0: "zero",
    1: "jeden",
    2: "dwa",
    3: "trzy",
    4: "cztery",
    5: "pięć",
    6: "sześć",
    7: "siedem",
    8: "osiem",
    9: "dziewięć",
  ]
  plTeens = [
    10: "dziesięć",
    11: "jedenaście",
    12: "dwanaście",
    13: "trzynaście",
    14: "czternaście",
    15: "piętnaście",
    16: "szesnaście",
    17: "siedemnaście",
    18: "osiemnaście",
    19: "dziewiętnaście",
  ]
  plIrregularTens = [
    2: "dwadzieścia",
    3: "trzydzieści",
    4: "czterdzieści",
  ]
  plIrregularHundreds = [
    1: "sto",
    2: "dwieście",
    3: "trzysta",
    4: "czterysta",
  ]
  plOneThousand = "tysiąc"
  plThousands1 = "tysiące"
  plThousands2 = "tysięcy"

proc compactInt(i: int): string = $i

proc polishInt(i: int): string {.locks: 0.} =

  # sweet jesus

  assert i in 0..999999

  let
    onesAndTens = i mod 100
    ones = i mod 10
    tens = i div 10 mod 10
    hundreds = i div 100 mod 10
    thousands = i div 1000

  proc tensStr(tens, ones: int): string {.inline.} =
    result =
      case tens
      of 2..4: plIrregularTens[tens]
      else: plOnes[tens] & "dziesiąt"
    if ones != 0:
      result.add(' ')
      result.add(plOnes[ones])

  proc hundredsStr(hundreds: int): string {.inline.} =
    case hundreds
    of 1..4: plIrregularHundreds[hundreds]
    else: plOnes[hundreds] & "set"

  proc thousandsStr(thousands: int): string {.inline.} =
    case thousands
    of 1: plOneThousand
    of 2..4: polishInt(thousands) & ' ' & plThousands1
    else: polishInt(thousands) & ' ' & plThousands2

  result =
    case onesAndTens
    of 0..9: plOnes[onesAndTens]
    of 10..19: plTeens[onesAndTens]
    else: tensStr(tens, ones)

  if (hundreds > 0 or thousands > 0) and onesAndTens == 0:
    result.setLen(0)

  if hundreds > 0:
    let oldResult = result
    result = hundredsStr(hundreds)
    if oldResult.len > 0:
      result.add(' ')
      result.add(oldResult)
  if thousands > 0:
    let oldResult = result
    result = thousandsStr(thousands)
    if oldResult.len > 0:
      result.add(' ')
      result.add(oldResult)

proc englishInt(i: int): string =

  assert i in 0..999999
  cifAssert(false, "English mode is not implemented")

proc encodeBitmap(output: Stream, channels: range[3..4],
                  input: openArray[uint8], locInt: ToLocInt) =
  for i in countup(0, input.len - 1, channels):
    for c in 0..<channels:
      if c > 0:
        output.write("; ")
      output.write(locInt(input[i + c].int))
    output.write('\n')

proc encodeQuadtree(output: Stream, width, height: Natural,
                    channels: range[3..4], input: openArray[uint8],
                    locInt: ToLocInt) =
  cifAssert(false, "Quadtree encoding is not implemented")  # TODO

proc hasOneLanguage(flags: set[CifFlag]): bool =
  not (cifPolish in flags and cifEnglish in flags)

proc languageSpecified(flags: set[CifFlag]): bool =
  cifPolish in flags or cifEnglish in flags

proc encodeCif*(output: Stream, width, height: Natural, channels: range[3..4],
                flags: set[CifFlag], metadata: openArray[(string, string)],
                input: openArray[uint8]) =
  ## Encodes a CIF image. Pixels are interpreted as RGB or RGBA depending on the
  ## ``channels`` parameter.

  # validate provided data

  cifAssert(flags.hasOneLanguage,
            "Language cannot be polish and english at the same time")
  cifAssert(flags.languageSpecified,
            "Language must be specified")
  cifAssert(width <= 999999 and height <= 999999,
            "Image is too big; only sizes up to 999999×999999 are supported")
  cifAssert(input.len == width * height * channels,
            "Input data length must match width * height * channels")

  for (key, value) in metadata:
    cifAssert(key.allCharsInSet(MetadataKeyChars),
              fmt"Metadata key '{key}' contains invalid characters")
    cifAssert(value.allCharsInSet(MetadataValueChars),
              fmt"Metadata value '{value}' contains invalid characters")

  # choose language-dependent stuff

  let
    dict =
      if cifEnglish in flags: EnglishKeywords
      else: PolishKeywords
    locInt: ToLocInt =
      if cifCompact in flags: compactInt
      elif cifEnglish in flags: englishInt
      else: polishInt

  # output header

  output.writeLine(fmt"{Magic}: {flags}")
  output.writeLine(fmt"{dict[kwVersion]} {locInt(CifFormatVersion)}")
  output.writeLine(
    fmt"{dict[kwSize]} " &
    fmt"{dict[kwWidth]}: {locInt(width)}, " &
    fmt"{dict[kwHeight]}: {locInt(height)}, " &
    fmt"{dict[kwBpp]}: {locInt(channels * 8)}"
  )
  for (key, value) in metadata:
    output.writeLine(fmt"{dict[kwMetadata]} {key} {value}")

  if cifQuadtree in flags:
    output.encodeQuadtree(width, height, channels, input, locInt)
  else:
    output.encodeBitmap(channels, input, locInt)



# decoder

type
  Parser = object
    data {.requiresInit.}: ptr UncheckedArray[char]
    len {.requiresInit.}, position: int
    lineCount: int

template cifError(input: Parser, message: string): untyped =
  raise newException(CifError, fmt"in input at {input.position}: " & message)

{.push inline.}

proc initParser(data: openArray[char]): Parser =
  Parser(
    data: cast[ptr UncheckedArray[char]](data[0].unsafeAddr),
    len: data.len,
    position: 0,
    lineCount: 1,
  )

proc atEnd(input: Parser): bool =
  input.position >= input.len

proc peekChar(input: Parser): char =
  if not input.atEnd: input.data[input.position]
  else: '\0'

proc readChar(input: var Parser): char =
  result = input.peekChar()
  inc input.position

proc exactMatch(input: var Parser, word: openArray[char]): bool =

  if input.position + word.len > input.len:
    return false

  for i in 0..<word.len:
    if input.data[input.position + i] != word[i]:
      return false

  input.position += word.len
  result = true

proc readUntil(input: var Parser, charset: set[char]): string =

  while input.peekChar() notin charset:
    result.add(input.readChar())

template trueOrReturn(expr: untyped): untyped =

  if not expr:
    return

template trueOrError(input: Parser, expr: untyped,
                     message: string): untyped =

  if not expr:
    input.cifError(message)

proc ws(input: var Parser): bool =

  while input.peekChar() == ' ':
    result = true
    discard input.readChar()

proc nl(input: var Parser): bool =

  while input.peekChar() == '\n':
    result = true
    inc input.lineCount
    discard input.readChar()

proc comma(input: var Parser): bool =

  trueOrReturn input.exactMatch(",")
  trueOrReturn input.ws()
  result = true

proc semicolon(input: var Parser): bool =

  trueOrReturn input.exactMatch(";")
  trueOrReturn input.ws()
  result = true

proc colon(input: var Parser): bool =

  trueOrReturn input.exactMatch(":")
  trueOrReturn input.ws()
  result = true

proc parseEnum[T: enum](input: var Parser, dest: var T): bool =

  for x in T:
    if input.exactMatch($x):
      dest = x
      return true

proc parseEnumSet[T: enum](input: var Parser, set: var set[T]): bool =

  while true:
    var x: T
    trueOrReturn input.parseEnum[:T](x)
    set.incl(x)
    if not input.comma():
      return true

{.pop.}

macro unrolledExactMatch(input: var Parser, str: static string): bool =

  result = quote do:
    `input`.position + `str`.len <= `input`.len

  var charConds: seq[NimNode]
  for i, c in str:
    let lit = newLit(c)
    charConds.add quote do:
      `input`.data[`input`.position + `i`] == `lit`

  for cond in charConds:
    result = infix(result, "and", cond)

  result = quote do:
    let cond = `result`
    `input`.position += `str`.len * ord(cond)
    cond

type
  TrieCase = tuple[body, indexVar: NimNode, index: int]

var trieCases {.compileTime.}: Table[string, TrieCase]

macro trieAddCase(str: static string, body: untyped) =
  trieCases[str] = (body: body, indexVar: nil, index: -1)

macro trieAddCase(
  # gotta use openArray because the compiler doesn't like normal arrays
  arr: static openArray[string],
  arrMin: static int,
  indexVar, body: untyped
) =
  for i, str in arr:
    trieCases[str] = (body, indexVar, i + arrMin)

macro trieFinish(input: var Parser): untyped =

  type Cell = tuple[str: string, tc: TrieCase]

  proc unrolledMatchBranch(cell: Cell, depth: int): NimNode =

    let
      str = cell.str[depth..^1]
      cond = quote do:
        input.unrolledExactMatch(`str`)
    var action = newStmtList()
    if cell.tc.indexVar != nil:
      let
        ivar = cell.tc.indexVar
        index = cell.tc.index
      action.add quote do:
        let `ivar` = `index`
    action.add(cell.tc.body)
    result = newTree(nnkElifBranch, cond, action)

  proc traverseCells(input: NimNode, sourceCells: seq[Cell]): NimNode =

    var cells: array[char.low..char.high, seq[Cell]]

    for (str, tc) in sourceCells:
      cells[str[0]].add (str, tc)

    result = newTree(nnkIfStmt)
    for ch, list in cells:
      if list.len == 1:
        result.add(unrolledMatchBranch(list[0], 0))
      elif list.len > 1:
        var list = list
        list.sort(
          order = SortOrder.Descending,
          cmp = proc (a, b: Cell): int =
            cmp(a.str, b.str)
        )
        let
          charLit = newLit(ch)
          cond = quote do:
            `input`.peekChar() == `charLit`
        var cases = newTree(nnkIfStmt)
        for cell in list:
          cases.add(unrolledMatchBranch(cell, 1))
        let body = quote do:
          inc `input`.position
          `cases`
        result.add(newTree(nnkElifBranch, cond, body))

  var rootCells: seq[Cell]
  for str, tc in trieCases:
    rootCells.add (str, tc)

  result = traverseCells(input, rootCells)

  trieCases.clear()

macro trie(input: var Parser, body: untyped): untyped =

  result = newStmtList()

  let
    addCase = bindSym"trieAddCase"
    finish = bindSym"trieFinish"

  for stmt in body:
    if stmt.kind == nnkCall and stmt[0].kind == nnkIdent:
      result.add newCall(addCase, stmt[0], stmt[1])
    elif stmt.kind == nnkInfix:
      stmt[0].expectIdent("|")
      result.add newCall(
        addCase,
        stmt[1],  # arr
        newCall(bindSym"low", stmt[1]),  # arrMin
        stmt[2],  # indexVar
        stmt[3],  # body
      )

  result.add(newCall(finish, input))

proc parsePolishInt(input: ptr Parser): int =

  {.push experimental: "implicitDeref".}

  const regularTensAndHundreds = {5..9}

  var word = 0
  while true:

    input.trie:

      plOneThousand:
        word = 1000

      [plThousands1, plThousands2] | _:
        result *= 1000

      plIrregularHundreds | i:
        word = i * 100

      plIrregularTens | i:
        word = i * 10

      plTeens | i:
        word = i

      plOnes | i:
        word = i
        if i in regularTensAndHundreds:
          if input.unrolledExactMatch("dziesiąt"):
            word *= 10
          elif input.unrolledExactMatch("set"):
            word *= 100

    result += word
    word = 0
    if not input.ws():
      break

  {.pop.}

{.push inline.}

proc magicAndFlags(input: var Parser, flags: var set[CifFlag]) =

  input.trueOrError input.unrolledExactMatch(Magic), "Magic CIF expected"
  input.trueOrError input.colon(), "Colon after magic CIF expected"
  input.trueOrError input.parseEnumSet[:CifFlag](flags), "CIF flags expected"

proc version(input: var Parser, version: var int, dict: static Dictionary) =

  input.trueOrError input.unrolledExactMatch(dict[kwVersion]),
                    "Version keyword expected"
  input.trueOrError input.ws(), "Whitespace after version keyword expected"
  version = input.addr.parsePolishInt()

proc sizeAndBpp(input: var Parser, width, height: var Natural,
                channels: var range[3..4], dict: static Dictionary) =

  input.trueOrError input.unrolledExactMatch(dict[kwSize]),
                    "Size keyword expected"
  input.trueOrError input.ws(), "Whitespace after size keyword expected"

  # width
  input.trueOrError input.unrolledExactMatch(dict[kwWidth]),
                    "Width keyword expected"
  input.trueOrError input.colon(), "Colon after width keyword expected"
  width = input.addr.parsePolishInt()
  input.trueOrError input.comma(), "Comma after width expected"

  # height
  input.trueOrError input.unrolledExactMatch(dict[kwHeight]),
                    "Height keyword expected"
  input.trueOrError input.colon(), "Colon after height keyword expected"
  height = input.addr.parsePolishInt()
  input.trueOrError input.comma(), "Comma after height expected"

  # bits per pixel
  var bpp: int
  input.trueOrError input.unrolledExactMatch(dict[kwBpp]),
                    "BPP keyword expected"
  input.trueOrError input.colon(), "Colon after BPP keyword expected"
  bpp = input.addr.parsePolishInt()
  input.trueOrError bpp in [24, 32], "Invalid bits per pixel: " & $bpp
  channels =
    if bpp == 24: 3
    else: 4

proc metadata(input: var Parser, metadata: var seq[(string, string)],
              dict: static Dictionary) =

  while input.unrolledExactMatch(dict[kwMetadata]):
    input.trueOrError input.ws(), "Whitespace expected after metadata keyword"
    let key = input.readUntil({' '})
    discard input.ws()
    let value = input.readUntil({'\l'})
    metadata.add((key, value))
    input.trueOrError input.nl(), "Line break expected to end metadata string"

proc decodeBitmap(input: var Parser, output: var seq[uint8],
                  width, height: Natural, channels: var range[3..4]) =

  output.setLen(width * height * channels)

  var x, y = 0
  while true:
    for i in 0..<channels:
      let val = input.addr.parsePolishInt()
      input.trueOrError val in 0..255, "Channel out of range 0..255: " & $val
      output[i + channels * (x + y * width)] = val.uint8
      if i < channels - 1:
        input.trueOrError input.semicolon(),
                          "Semicolon expected to separate channels"
    input.trueOrError input.nl(), "Line break expected after pixel"
    if input.atEnd:
      break
    inc x
    if x >= width:
      x = 0
      inc y

{.pop.}

proc decodeCif*(output: var seq[uint8], width, height: var Natural,
                channels: var range[3..4], flags: var set[CifFlag],
                metadata: var seq[(string, string)], inputStream: Stream) =
  ## Decodes a CIF image to ``output``, saves metadata read from the file
  ## into the provided parameters.

  var
    inputString = inputStream.readAll()  # we store it in a var to keep it alive
    input = initParser(inputString)

  input.magicAndFlags(flags)
  input.trueOrError flags.hasOneLanguage,
                    "Flags have both english and polish at the same time"
  input.trueOrError flags.languageSpecified,
                    "Flags do not have a language specified"
  input.trueOrError input.nl(), "Line break expected after flags"

  var version: int
  input.version(version, PolishKeywords)
  input.trueOrError input.nl(), "Line break expected after version"

  input.sizeAndBpp(width, height, channels, PolishKeywords)
  input.trueOrError input.nl(), "Line break expected after size information"

  input.metadata(metadata, PolishKeywords)
  # line break is implied by metadata

  input.decodeBitmap(output, width, height, channels)

  let expectBytes = width * height * channels
  input.trueOrError expectBytes == output.len,
    "Image size doesn't match output byte count. Expected: " & $expectBytes &
    ", got: " & $output.len

when isMainModule:
  echo "LICZBOTESTER 9999"
  echo "tests numbers."

  for i in 0..999999:
    var
      origString = polishInt(i)
      input = initParser(origString)
      outI = parsePolishInt(addr input)
    if i != outI:
      echo "mismatch: ", i, " -> ", origString, " -> ", outI

  echo "LICZBOTESTER 9999 done.  Thank you."
