{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ID.Parse (parseChanges) where

import Brassica.SoundChange.Parse (parseRule)
import Control.Monad.State
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import ID.Schemata

type Parser = ParsecT Void Text (State [Edge])

-- space consumer which does not match newlines
sc :: Parser ()
sc = L.space space1' (L.skipLineComment ";") empty
  where
    -- adapted from megaparsec source: like 'space1', but does not
    -- consume newlines (which are important for rule separation)
    space1' = void $ takeWhile1P (Just "white space") ((&&) <$> isSpace <*> (/='\n'))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

code :: Parser Text
code = lexeme $ T.pack <$> many alphaNumChar

nextLine :: Parser ()
nextLine = void $ some $ symbol "\n"

-- NB: this cancels out comments in the rest of the line
restOfLine :: Parser Text
restOfLine = trim <$> takeWhile1P (Just "sound change rule") (/='\n') <* nextLine
  where
    trim = T.dropWhile isSpace . T.dropWhileEnd isSpace

parseNote :: Parser Text
parseNote = symbol "+" *> restOfLine

parseOverrides :: Parser Overrides
parseOverrides = go NoOverride
  where
    go :: Overrides -> Parser Overrides
    go o = (<|> pure o) $ do
        o' <- choice
            [ WholeRule              <$> (symbol "&"  *> restOfLine)
            , WholeEnvironment       <$> (symbol "/&" *> restOfLine)
            , ExtraConditions . pure <$> (symbol "/+" *> restOfLine)
            ]
        go =<< combine o o'

    combine :: Overrides -> Overrides -> Parser Overrides
    combine NoOverride o = pure o
    combine o NoOverride = pure o
    combine (ExtraConditions cs) (ExtraConditions ds)
        = pure $ ExtraConditions (cs++ds)
    combine _ _ = fail "Incompatible overrides"

parseChange :: Parser Change
parseChange = do
    _ <- symbol "-"
    rule' <- restOfLine
    case parseRule (T.unpack rule') of
        Left err -> rethrow err
        Right rule -> Change rule <$> parseOverrides <*> many parseNote
  where
    rethrow :: ParseErrorBundle String Void -> Parser a
    rethrow ParseErrorBundle{bundleErrors=e:|_} = parseError $ toText e

    toText :: ParseError String a -> ParseError Text a
    toText (TrivialError o u e) = TrivialError o u e
    toText (FancyError o e) = FancyError o e

parseSource :: Parser (Text, [Change])
parseSource = (,) <$> (symbol "@" *> restOfLine) <*> many parseChange

parseEdge :: Parser ()
parseEdge = do
    _ <- symbol "#"
    from <- code
    to <- code
    nextLine
    ss <- many parseSource

    modify $ fmap $ \case
        e | e_from e == to -> e { e_root = False }
          | otherwise -> e

    modify $ \es ->
        let r = all (\e -> e_to e /= from) es
        in Edge from to r ss : es

parseChanges :: String -> Text -> Either (ParseErrorBundle Text Void) SoundChanges
parseChanges n i = flip evalState [] $ runParserT (sc *> go <* eof) n i
  where
    go = do
        title <- restOfLine
        _ <- many parseEdge
        es <- get
        pure $ SoundChanges title es
