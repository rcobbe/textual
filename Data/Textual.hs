{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-
Copyright 2013-2017 Richard Cobbe

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

-- | Define a general interface to a string type, to allow us to defer the
--   decision about the precise string representation as late as possible.
--   There is an unfortunate interaction with overloaded strings in programs
--   that use this class, which requires clients to add type annotations in
--   places where they didn't previously need to, but this is a relatively
--   minor cost.

module Data.Textual(Textual(..), View(..), view) where

import qualified Data.List as List
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
  intercalate = List.intercalate

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

-- | View of a Textual value, for pattern matching.  The type variable @a@ is
--   the actual type of the Textual value.  The decision to leave the rest of
--   the textual value unconverted is intentional, as there are some
--   applications where this is more convenient than converting the entire
--   textual value to a form suitable for pattern matching.  Clients who
--   wish to convert the entire value can use 'toString' above.
data View a = Empty
              -- ^ empty textual value
            | Char :|: a
              -- ^ first character and the rest of the textual value

-- | Construct the View of a Textual value.
view :: Textual a => a -> View a
view txt =
  if Data.Textual.null txt
  then Empty
  else (Data.Textual.head txt) :|: (Data.Textual.tail txt)
