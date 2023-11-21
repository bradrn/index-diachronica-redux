{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (runState, runStateT)
import Data.Csv (decodeByName)
import Data.Foldable (for_)
import Data.Yaml (decodeFileEither)
import Text.Pandoc (def, unPandocPure)
import Text.Pandoc.Citeproc (getReferences)
import Text.Pandoc.Readers.BibTeX (readBibLaTeX)
import System.Directory (listDirectory)
import System.FilePath ((</>))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as TIO

import ID.Schemata
import ID.Parse (parseChanges)

main :: IO ()
main = do
    putStrLn "Language list:"
    print . decodeByName @Languoid =<< BL.readFile "data/languoids.csv"

    putStrLn "\nLanguage info:"
    print =<< decodeFileEither @[LangInfo] "data/langinfo.yaml"

    putStrLn "\nReferences:"
    bibfile <- TIO.readFile "data/references.bib"
    let refs = fst $ fst $
            flip runState def $ flip runStateT def $ runExceptT $ unPandocPure $ do
                pandoc <- readBibLaTeX def bibfile
                getReferences Nothing pandoc
    print refs

    putStrLn "\nReference data:"
    print =<< decodeFileEither @[ReferenceData] "data/references-data.yaml"

    putStrLn "\nSound changes:"
    changefiles <- listDirectory "data/changes"
    for_ changefiles $ \file ->
        print . parseChanges file =<< TIO.readFile ("data/changes" </> file)
