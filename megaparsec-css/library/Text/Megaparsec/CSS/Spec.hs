{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# LANGUAGE TypeFamilies, RankNTypes #-}

-- | https://www.w3.org/TR/CSS21/grammar.html#scanner

module Text.Megaparsec.CSS.Spec
  (
  -- * Identifiers
    ident, name, nmstart, nmchar

  -- * Strings
  , string, string1, string2
  , badstring, badstring1, badstring2

  -- * URLs
  , url, baduri, baduri1, baduri2, baduri3

  -- * Numbers
  , num

  -- * Comments
  , comment, badcomment, badcomment1, badcomment2

  -- * Whitespace
  , s, w, nl

  -- * Representations of single characters
  , h, nonascii, unicode, escape
  , letterA, letterC, letterD, letterE, letterG
  , letterH, letterI, letterK, letterL, letterM
  , letterN, letterO, letterP, letterR, letterS
  , letterT, letterU, letterX, letterZ

  ) where

import Data.Functor
import Data.Functor.Identity
import Data.Void
import Text.Megaparsec ((<?>), (<|>))

import qualified Data.Char as Char
import qualified Numeric
import qualified Text.Megaparsec as P

type Parser e s m a =
    (Ord e, P.MonadParsec e s m, P.Token s ~ Char) =>
    P.ParsecT e s m a

match :: P.MonadParsec e s m => m a -> m (P.Tokens s)
match p = fst <$> P.match p

-- |
-- > [0-9a-f]

h :: Parser e s m Char
h =
  let
    f x = (x >= '0') && (x <= '9')
       || (x >= 'a') && (x <= 'f')
       || (x >= 'A') && (x <= 'F')  -- a-f includes A-F because
                                    -- it is case-insensitive
  in
    P.satisfy f <?> "h"

-- |
-- > [\240-\377]
--
-- The "\377" represents the highest character number that
-- current versions of Flex can deal with (decimal 255).
-- It should be read as "\4177777" (decimal 1114111), which
-- is the highest possible code point in Unicode/ISO-10646.

nonascii :: Parser e s m Char
nonascii =
  let
    f x = (x >= '\160')       -- octal 240     = decimal 160
       && (x <= '\1114111')   -- octal 4177777 = decimal 1114111
  in
    P.satisfy f <?> "nonascii"

-- |
-- > \r\n|[ \t\r\n\f]
--
-- This isn't named in the grammar, but it's duplicated a lot,
-- so we've given it a name here. It appears optionally at the
-- end of escape sequences.

escapeTerminator :: Parser e s m ()
escapeTerminator =
    void (P.single '\r' *> P.single '\n') <|>
    void (P.satisfy (`elem` " \t\r\n\f"))

-- |
-- > \\{h}{1,6}(\r\n|[ \t\r\n\f])?

unicode :: Parser e s m Char
unicode =
  do
    _ <- P.single '\\'
    x <- P.count' 1 6 h
    _ <- P.optional escapeTerminator
    return (readHexChar x)
  <?> "unicode"

readHexChar :: String -> Char
readHexChar s =
  case Numeric.readHex s of
    [(n, "")] -> Char.chr n
    _ -> error ("Invalid hex char: " ++ s)

-- |
-- > {unicode}|\\[^\r\n\f0-9a-f]

escape = _

-- |
-- > [_a-z]|{nonascii}|{escape}

nmstart = _

-- |
-- > [_a-z0-9-]|{nonascii}|{escape}
nmchar = _

-- |
-- > \"([^\n\r\f\\"]|\\{nl}|{escape})*\"

string1 = _

-- |
-- > \'([^\n\r\f\\']|\\{nl}|{escape})*\'

string2 = _

-- |
-- > \"([^\n\r\f\\"]|\\{nl}|{escape})*\\?

badstring1 = _

-- |
-- > \'([^\n\r\f\\']|\\{nl}|{escape})*\\?

badstring2 = _

-- |
-- > \/\*[^*]*\*+([^/*][^*]*\*+)*

badcomment1 = _

-- |
-- > \/\*[^*]*(\*+[^/*][^*]*)*

badcomment2 = _

-- |
-- > url\({w}([!#$%&*-\[\]-~]|{nonascii}|{escape})*{w}

baduri1 = _

-- |
-- > url\({w}{string}{w}

baduri2 = _

-- |
-- > url\({w}{badstring}

baduri3 = _

-- |
-- > \/\*[^*]*\*+([^/*][^*]*\*+)*\/

comment = _

-- |
-- > -?{nmstart}{nmchar}*

ident = _

-- |
-- > {nmchar}+

name = _

-- |
-- > [0-9]+|[0-9]*"."[0-9]+

num = _

-- |
-- > {string1}|{string2}

string = _

-- |
-- > {badstring1}|{badstring2}

badstring = _

-- |
-- > {badcomment1}|{badcomment2}

badcomment = _

-- |
-- > {baduri1}|{baduri2}|{baduri3}

baduri = _

-- |
-- > ([!#$%&*-~]|{nonascii}|{escape})*

url = _

-- |
-- > [ \t\r\n\f]+

s = _

-- |
-- > {s}?

w = _

-- |
-- > \n|\r\n|\r|\f

nl = _

-- |
-- > a|\\0{0,4}(41|61)(\r\n|[ \t\r\n\f])?

letterA = _

-- |
-- > c|\\0{0,4}(43|63)(\r\n|[ \t\r\n\f])?

letterC = _

-- |
-- > d|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?

letterD = _

-- |
-- > e|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?

letterE = _

-- |
-- > g|\\0{0,4}(47|67)(\r\n|[ \t\r\n\f])?|\\g

letterG = _

-- |
-- > h|\\0{0,4}(48|68)(\r\n|[ \t\r\n\f])?|\\h

letterH = _

-- |
-- > i|\\0{0,4}(49|69)(\r\n|[ \t\r\n\f])?|\\i

letterI = _

-- |
-- > k|\\0{0,4}(4b|6b)(\r\n|[ \t\r\n\f])?|\\k

letterK = _

-- |
-- > l|\\0{0,4}(4c|6c)(\r\n|[ \t\r\n\f])?|\\l

letterL = _

-- |
-- > m|\\0{0,4}(4d|6d)(\r\n|[ \t\r\n\f])?|\\m

letterM = _

-- |
-- > n|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n

letterN = _

-- |
-- > o|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\o

letterO = _

-- |
-- > p|\\0{0,4}(50|70)(\r\n|[ \t\r\n\f])?|\\p

letterP = _

-- |
-- > r|\\0{0,4}(52|72)(\r\n|[ \t\r\n\f])?|\\r

letterR = _

-- |
-- > s|\\0{0,4}(53|73)(\r\n|[ \t\r\n\f])?|\\s

letterS = _

-- |
-- > t|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t

letterT = _

-- |
-- > u|\\0{0,4}(55|75)(\r\n|[ \t\r\n\f])?|\\u

letterU = _

-- |
-- > x|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\x

letterX = _

-- |
-- > z|\\0{0,4}(5a|7a)(\r\n|[ \t\r\n\f])?|\\z

letterZ = _
