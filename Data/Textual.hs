{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Define a general interface to a string type, to allow us to defer the
--   decision about the precise string representation as late as possible.
--   There is an unfortunate interaction with overloaded strings in programs
--   that use this class, which requires clients to add type annotations in
--   places where they didn't previously need to, but this is a relatively
--   minor cost.

module Data.Textual(Textual(..)) where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

-- | General interface to textual values -- that is, a value that contains
--   some text and behaves generally like a string.  The set of operations
--   supported is somewhat ad hoc.
class Textual a where
  -- | Convert a string to the textual type
  fromString :: String -> a
  -- | Convert a textual type to the equivalent string
  toString :: a -> String

  -- | An empty textual value
  empty :: a

  -- | Construct a textual value that contains a single character
  singleton :: Char -> a

  -- | Add a character to the front of a textual value
  cons :: Char -> a -> a

  -- | Tests whether a textual value is empty or not
  null :: a -> Bool

  -- | Returns the first character of a textual value, which must not be empty.
  head :: a -> Char

  -- | Returns all characters after the head of a textual value, which must not
  --   be empty.
  tail :: a -> a

  -- | Concatenates two textual values.
  append :: a -> a -> a

  -- | Concatenates a sequence of textual values.
  concat :: [a] -> a

  -- | Concatenates the list, inserting the first argument between successive
  --   elements of the list.
  intercalate :: a -> [a] -> a

  -- | @span p t@ returns a pair of textual values.  The first is the longest
  --   prefix of @t@ that contains only characters that satisfy @p@, and the
  --   second is the remainder of the original textual value.
  span :: (Char -> Bool) -> a -> (a, a)

instance Textual String where
  fromString = id
  toString = id

  empty = []
  singleton x = [x]
  cons = (:)
  null = Prelude.null
  head = Prelude.head
  tail = Prelude.tail

  append = (++)
  concat = Prelude.concat
  intercalate = Prelude.intercalate

  span = Prelude.span

instance Textual Text.Text where
  fromString = Text.pack
  toString = Text.unpack

  empty = Text.empty
  singleton = Text.singleton
  cons = Text.cons
  null = Text.null
  head = Text.head
  tail = Text.tail

  append = Text.append
  concat = Text.concat
  intercalate = Text.intercalate

  span = Text.span

instance Textual Lazy.Text where
  fromString = Lazy.pack
  toString = Lazy.unpack

  empty = Lazy.empty
  singleton = Lazy.singleton
  cons = Lazy.cons
  null = Lazy.null
  head = Lazy.head
  tail = Lazy.tail

  append = Lazy.append
  concat = Lazy.concat
  intercalate = Lazy.intercalate

  span = Lazy.span


