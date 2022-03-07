module Test.Utils where

import StringParser.Common (Parser)

newtype AnyParser = AnyParser (forall r. (forall a. Parser a -> r) -> r)

mkAnyParser :: forall a. Parser a -> AnyParser
mkAnyParser p = AnyParser \f -> f p
