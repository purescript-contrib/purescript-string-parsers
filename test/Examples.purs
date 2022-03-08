module StringParser.Examples where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold, foldl, sum)
import Data.List.Types (NonEmptyList)
import Effect (Effect)
import Effect.Console (log, logShow)
import StringParser (Parser, anyChar, between, char, endBy1, eof, fail, lookAhead, many, many1, regex, runParser, sepBy1, skipSpaces, string, unParser, (<?>))

-- Serves only to make this file runnable
main :: Effect Unit
main = printResults

printResults :: Effect Unit
printResults = do
  log "" -- empty blank line to separate output from function call

  log "### Example Content 1 ###"
  doBoth "fail" ((fail "example failure message") :: Parser Unit) exampleContent1
  doBoth "numberOfAs" numberOfAs exampleContent1
  doBoth "removePunctuation" removePunctuation exampleContent1
  doBoth "replaceVowelsWithUnderscore" replaceVowelsWithUnderscore exampleContent1
  doBoth "tokenizeContentBySpaceChars" tokenizeContentBySpaceChars exampleContent1
  doBoth "extractWords" extractWords exampleContent1
  doBoth "badExtractWords" badExtractWords exampleContent1
  doBoth "quotedLetterExists" quotedLetterExists exampleContent1

  log
    "\n\
    \### Example Content 2 ###"
  doBoth "parseCSV" parseCSV exampleContent2

-- Example Content 1

exampleContent1 :: String
exampleContent1 =
  "How many 'a's are in this sentence, you ask? Not that many."

numberOfAs :: Parser Int
numberOfAs = do
  {-
  let
    oneIfA = 1 <$ string "a" <?> "Letter was 'a'"
    zeroIfNotA = 0 <$ regex "[^a]" <?> "Letter was not 'a'"
    letterIsOneOrZero = oneIfA <|> zeroIfNotA <?>
                            "The impossible happened: \
                            \a letter was not 'a', and was not not-'a'."
    convertLettersToList = many1 letterIsOneOrZero
  list <- convertLettersToList                                          -}
  list <- many1
    ( (1 <$ string "a")
        <|> (0 <$ regex "[^a]")
    )
  -- calculate total number by adding Ints in list together
  pure $ sum list

removePunctuation :: Parser String
removePunctuation =
  do {-
  let
    charsAndSpaces = regex "[a-zA-Z ]+"
    everythingElse = regex "[^a-zA-Z ]+"
    ignoreEverythingElse = "" <$ everythingElse
    zeroOrMoreFragments = many1 $ charsAndSpaces <|> ignoreEverythingElse   -}
    list <- many1
      ( regex "[a-zA-Z ]+"
          <|> ("" <$ regex "[^a-zA-Z ]+")
      )

    -- combine the list's contents together via '<>'
    pure $ foldl (<>) "" list

replaceVowelsWithUnderscore :: Parser String
replaceVowelsWithUnderscore = do
  list <- many1 $
    ( ("_" <$ regex "[aeiou]")
        <|> regex "[^aeiou]+"
    )

  pure $ foldl (<>) "" list

tokenizeContentBySpaceChars :: Parser (NonEmptyList String)
tokenizeContentBySpaceChars = do
  (regex "[^ ]+") `sepBy1` (string " ")

extractWords :: Parser (NonEmptyList String)
extractWords = do
  endBy1 (regex "[a-zA-Z]+")
    -- try commenting out one of the "<|> string ..." lines and see what happens
    ( many1
        ( string " " <?> "Failed to match space as a separator"
            <|> string "'"
              <?> "Failed to match single-quote char as a separator"
            <|> string ","
              <?> "Failed to match comma as a separator"
            <|> string "?"
              <?> "Failed to match question mark as a separator"
            <|> string "."
              <?> "Failed to match period as a separator"
              <?> "Could not find a character that separated the content..."
        )
    )

badExtractWords :: Parser (NonEmptyList String)
badExtractWords = do
  list <- endBy1 (regex "[a-zA-Z]+")
    -- try commenting out the below "<|> string ..." lines
    ( many1
        ( string " " <?> "Failed to match space as a separator"
            <|> string "'"
              <?> "Failed to match single-quote char as a separator"
            <|> string ","
              <?> "Failed to match comma as a separator"
              -- <|> string "?" <?> "Failed to match question mark as a separator"
              -- <|> string "." <?> "Failed to match period as a separator"
              <?> "Could not find a character that separated the content..."
        )
    )
  -- short for 'end of file' or 'end of content'
  eof <?> "Entire content should have been parsed but wasn't."
  pure list

-- there are better ways of doing this using `whileM`, but this explains
-- the basic idea:
quotedLetterExists :: Parser Boolean
quotedLetterExists = do
  let
    singleQuoteChar = string "'"
    betweenSingleQuotes parser =
      between singleQuoteChar singleQuoteChar parser

  list <- many
    ( true <$ (betweenSingleQuotes (char 'a') <?> "No 'a' found.")
        <|> false <$ anyChar
    )
  pure $ foldl (||) false list

-- Example Content 2

-- CSV sample with some inconsistent spacing
exampleContent2 :: String
exampleContent2 =
  "ID, FirstName, LastName,             Age, Email\n\
  \523,     Mark,   Kenderson, 24, my.name.is.mark@mark.mark.com\n"

type CsvContent =
  { idNumber :: String
  , firstName :: String
  , lastName :: String
  , age :: String
  , originalEmail :: String
  , modifiedEmail :: String
  }

parseCSV :: Parser CsvContent
parseCSV = do
  let
    commaThenSpaces = string "," *> skipSpaces
    idNumber_ = string "ID"
    firstName_ = string "FirstName"
    lastName_ = string "LastName"
    age_ = string "Age"
    email_ = string "Email"
    newline = string "\n"
    csvColumn = regex "[^,]+"

  -- parse headers but don't produce output
  void $ idNumber_ *> commaThenSpaces
    *> firstName_
    *> commaThenSpaces
    *> lastName_
    *> commaThenSpaces
    *> age_
    *> commaThenSpaces
    *>
      email_

  void newline

  -- now we're on line 2
  idNumber <- csvColumn <* commaThenSpaces
  firstName <- csvColumn <* commaThenSpaces
  lastName <- csvColumn <* commaThenSpaces
  age <- csvColumn <* commaThenSpaces

  -- lookAhead will parse the content ahead of us,
  -- then reset the position of the string
  -- to what it was before it.
  originalEmail <- lookAhead $ regex "[^\n]+"

  let
    parseAlphanumericChars = regex "[a-zA-Z0-9]+"
    parsePeriodsAndPlusesAsEmptyStrings =
      "" <$ ((string ".") <|> (string "+"))
    parseListOfParts =
      many1
        ( parseAlphanumericChars
            <|> parsePeriodsAndPlusesAsEmptyStrings
        )

  usernameWithoutPeriodsOrPluses <- fold <$> parseListOfParts
  void $ string "@"
  domainName <- fold <$> (many1 ((regex "[a-zA-Z0-9]+") <|> (string ".")))
  void $ string "\n"

  -- Ensure we hit the end of the string content via 'end-of-file'
  void eof

  -- now return the parsed content
  pure
    { idNumber
    , firstName
    , lastName
    , age
    , originalEmail
    , modifiedEmail: usernameWithoutPeriodsOrPluses <> "@" <> domainName
    }

-- Helper functions

doBoth :: forall a. Show a => String -> Parser a -> String -> Effect Unit
doBoth parserName parser content = do
  doRunParser parserName parser content
  doUnParser parserName parser content

-- | Shows the results of calling `unParser`. You typically want to use
-- | this function when writing a parser because it includes other info
-- | to help you debug your code.
doUnParser :: forall a. Show a => String -> Parser a -> String -> Effect Unit
doUnParser parserName parser content = do
  log $ "(unParser) Parsing content with '" <> parserName <> "'"
  case unParser parser { substring: content, position: 0 } of
    Left rec -> log $ "Position: " <> show rec.pos
      <>
        "\n\
        \Error: "
      <> show rec.error
    Right rec -> log $ "Result was: " <> show rec.result
      <>
        "\n\
        \Suffix was: "
      <> show rec.suffix
  log "-----"

-- | Shows the results of calling `runParser`. You typically don't want to use
-- | this function when writing a parser because it doesn't help you debug
-- | your code when you write it incorrectly.
doRunParser :: forall a. Show a => String -> Parser a -> String -> Effect Unit
doRunParser parserName parser content = do
  log $ "(runParser) Parsing content with '" <> parserName <> "'"
  case runParser parser content of
    Left error -> logShow error
    Right result -> log $ "Result was: " <> show result
  log "-----"
