module SyntaxTest
( biblicalPi -- deprecated
, indianaPi -- required for backwards compatibility
, Record(..)
, OpaqueAlias(Opaque)) where

import qualified Prelude as P
import Prelude hiding (sin, ($), Maybe(..)) -- comment after import

-- Multiple imports per line
import System.Exit; import Control.Monad -- legal if semicolon is present
import Control.Applicative import Data.List -- illegal!
import Data.Char -- comment should eat this semicolon; import Bar

-- This is a line comment
{- This is a block comment -}
{-# This is a compiler pragma #-}

-- Top-level declarations
main :: IO ()
main = exitWith nested
  -- Nested declarations
  where nested :: ExitCode
        nested = ExitSuccess

-- Basic type definitions
data Rating = Abysmal | Boring | Average | Decent | Phenomenal
  deriving (Eq, Read, Show, Ord, Enum)

-- Record syntax
data Record = Rec
  { name :: String
  , rating :: Rating
  } deriving (Eq, Read, Show)

-- Typedefs
type TransparentAlias = String
newtype OpaqueAlias = Opaque { toString :: String }

-- Integral vs. floating
biblicalPi = 3
indianaPi = 3.2

-- Type vs. value
noOp :: IO ()
noOp = return ()

-- Multiple type declarations
elem, notElem :: (Eq a) => a -> [a] -> Bool
(\\), (//) :: (Eq a) => [a] -> [a] -> [a]

-- Infix function declaration
_ `elem` [] = False
x `elem` (y:ys) = x == y || x `elem` ys
infix 4 `elem` -- not associative

-- Simultaneous type declaration and definition
answer :: Int = 42

-- Operators that resemble comments and vice versa
foo --^ bar = "This should not be highlighted as a comment"
foo -- ^ bar = "...but this should! (cf. Haddock syntax)"

-- Argument highlighting
_ `safeDiv` 0 = Nothing
safeDiv 0 _ = Just 0
safeDiv x y = Just (x / y)
1 / 0 = undefined

-- Unusual but legal operator definition syntax
(g / h) x = g x / h x

-- Named pattern matches
[] ++ rhs@(x:_) = rhs
lhs@(x:_) ++ [] = lhs

-- Debug functions
assert "sure" = error "Insufficient formality"
assert "nope" = trace "Received \"nope\"" undefined

-- Various constants
bools = [True, False]
ords = [LT, EQ, GT]
unit = ()
nil = []

#ifndef PREPROC
#include <bbcode.hs>
#define PREPROC putStrLn "Testing C preprocessor"
#endif

-- Unboxed primitives (-XMagicHash)
addPrimitive :: (# Int#, Int# #) -> Int#
addPrimitive (# x, y #) = x +# y

-- Various language extensions
data GADT a where
  Foo :: GADT a

data ExistentialTypes = forall a. a

templateHaskell = $(splice [$perl| ^*@$*@$&@$ |])

newQuasiQuote = [perl| sdf asdf |]

$(top) level splice

another top level splice

class FunDeps a b | a -> b where
  implicitParameters :: (?implicit :: a) => [a]

data family TypeFamilies a
data instance TypeFamilies () = TypeFamilies ()
class TF a where
  data Bar a :: * -> *
instance TF Int where
  data Bar Int = Whatever

patternGuards a b c
  | Just x <- lookup a b
  , Just y <- lookup a c
  = Just (x + y)

-- | These are Haddock comments.
-- The rest of this paragraph is also part of the same comment.

bar = 0 -- ^ This is a Haddock comment for a preceding declaration.
        -- These often appear inline, but not always.

-- | Module-level documentation comment (might be a block comment)
module ExportList (
  -- * Section Name
  foo -- ** Inline description of 'foo'
) where

{- |
  $section_name
  Treating Haddock comments as regular comments is fine, because
  supporting them is a lot of work. Here's a whole bunch of markup:

  @
    blockOfCode :: normal -> haskell -> code
  @

  >>> exampleCode
  output

  'Lexically.Valid.Haskell.identifier' would be formatted specially,
  but apostrophes are not special unless they surround an identifier.
  Double quotes yield hyperlinks to modules, such as "Control.Monad"

  (1) Enumerated list
  2. Enumerated list

  * Unordered list

  [@definitionList@] Description text for @definitionList@.

  Inline link to the manual: <http://www.haskell.org/haddock/doc/html/>
-}

foo = [| asdf + 1 "asdf" $( [| 1 |] ) |]


abc = fold' adf

abc' x y = 123

