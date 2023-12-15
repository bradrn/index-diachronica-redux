{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (runState, runStateT)
import Citeproc (citeproc, parseStyle)
import Citeproc.Types (CiteprocOptions(..), Result (resultBibliography))
import Data.Csv (decodeByName)
import Data.Text (pack)
import Data.Time
import Data.Traversable (for)
import Data.Yaml (decodeFileEither, decodeFileThrow)
import Lucid (renderBS)
import Text.Pandoc (def, unPandocPure)
import Text.Pandoc.Citeproc (getReferences)
import Text.Pandoc.Readers.BibTeX (readBibLaTeX)
import System.Directory (listDirectory)
import System.FilePath ((</>), (-<.>))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as TIO

import ID.Generate
import ID.Parse
import ID.Schemata

main :: IO ()
main = do
    Right (_, ls) <- decodeByName @Languoid <$> BL.readFile "data/languoids.csv"
    lis <- decodeFileThrow "data/langinfo.yaml"

    bibfile <- TIO.readFile "data/references.bib"
    let Right refs = fst $ fst $
            flip runState def $ flip runStateT def $ runExceptT $ unPandocPure $ do
                pandoc <- readBibLaTeX def bibfile
                getReferences Nothing pandoc

        os = CiteprocOptions
            { linkCitations = False
            , linkBibliography = True
            }

    stylesheet <- TIO.readFile "chicago-author-date.csl"
    Right style <- parseStyle (const $ pure mempty) stylesheet
    let bib = resultBibliography $ citeproc os style Nothing refs []

    rds' <- decodeFileEither @[ReferenceData] "data/references-data.yaml"
    let rds = case rds' of
            Left err -> error $ show err
            Right val -> val

    changefiles <- listDirectory "data/changes"
    let subdir = "site"
    files <- for changefiles $ \file -> do
        Right scs <- parseChanges file <$> TIO.readFile ("data/changes" </> file)
        let page = genPage ls lis bib rds scs
            filename = file -<.> "html"
        BL.writeFile (subdir </> filename) $ renderBS page
        pure (sc_title scs, pack filename)
    time <- getCurrentTime
    BL.writeFile (subdir </> "index.html") $ renderBS $ genIndex time files
