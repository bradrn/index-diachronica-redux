{-# LANGUAGE TypeApplications #-}

module Main where

import Brassica.SoundChange.Parse (errorBundlePretty)
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (runState, runStateT)
import Citeproc (citeproc, parseStyle)
import Citeproc.Types (CiteprocOptions(..), Result (resultBibliography))
import Data.Csv (decodeByName)
import Data.Text (pack)
import Data.Time
import Data.Traversable (for)
import Data.Yaml (decodeFileEither, decodeFileThrow, prettyPrintParseException)
import Lucid (renderBS)
import Text.Pandoc (def, unPandocPure)
import Text.Pandoc.Citeproc (getReferences)
import Text.Pandoc.Readers.BibTeX (readBibLaTeX)
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), (-<.>))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as TIO

import ID.Generate
import ID.Parse
import ID.Schemata

main :: IO ()
main = do
    Right (_, ls) <- decodeByName @Languoid <$> BL.readFile "data/languoids.csv"
    lis' <- decodeFileEither "data/langinfo.yaml"
    lis <- case lis' of
            Left err -> do
                putStrLn "Error in langinfo.yaml:"
                putStrLn $ prettyPrintParseException err
                exitFailure
            Right val -> pure val

    bibfile <- TIO.readFile "data/references.bib"
    let Right refs = fst $ fst $
            flip runState def $ flip runStateT def $ runExceptT $ unPandocPure $ do
                pandoc <- readBibLaTeX def bibfile
                getReferences Nothing pandoc

    stylesheet <- TIO.readFile "chicago-author-date.csl"
    Right style <- parseStyle (const $ pure mempty) stylesheet

    rds' <- decodeFileEither @[ReferenceData] "data/references-data.yaml"
    rds <- case rds' of
            Left err -> do
                putStrLn "Error in references-data.yaml:"
                putStrLn $ prettyPrintParseException err
                exitFailure
            Right val -> pure val

    changefiles <- listDirectory "data/changes"
    let subdir = "site"
    files <- for changefiles $ \file -> do
        parsed <- parseChanges file <$> TIO.readFile ("data/changes" </> file)
        case parsed of
            Right scs -> do
                let page = genPage ls lis refs style rds scs
                    filename = file -<.> "html"
                BL.writeFile (subdir </> filename) $ renderBS page
                pure (sc_title scs, pack filename)
            Left err -> do
                putStrLn $ errorBundlePretty err
                exitFailure
    time <- getCurrentTime
    BL.writeFile (subdir </> "index.html") $ renderBS $ genIndex time files
