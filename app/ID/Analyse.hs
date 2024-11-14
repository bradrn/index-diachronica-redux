{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ID.Analyse (genTransitions) where

import Brassica.SoundChange.Types
       ( Rule(..)
       , CategorySpec(..)
       , Lexeme(..)
       , Flags(..)
       , Direction(RTL)
       , CategoryModification (Union)
       , LexemeType (..)
       )
import Data.List (find)
import Data.Text (Text, pack)
import Data.Traversable (for)

import ID.Schemata
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad (zipWithM)
import Debug.Trace

(==&) :: Eq b => (a -> b) -> b -> (a -> Bool)
(==&) f v = (==v) . f

fromConvention :: [Convention] -> String -> [Text]
fromConvention convs cs
    | Just Convention{c_ipa} <- find (c_sym ==& pack cs) convs
    = c_ipa
    | '*':cs' <- cs = [pack cs']
    | otherwise = [pack cs]

transitions :: [Convention] -> Rule CategorySpec -> [(Text, Text)]
transitions cs r
    | null (replacement r) = filter deduplicate $ fromMaybe [] $
          go (target r) (repeat (Grapheme "∅"))
    | length (target r) /= length (replacement r) = []
    | otherwise =
    let rtl = applyDirection (flags r) == RTL
    in filter deduplicate $ fromMaybe [] $ go
        (if rtl then reverse (target r) else target r)
        (if rtl then reverse (replacement r) else replacement r)
  where
    deduplicate = uncurry (/=)

    go :: [Lexeme CategorySpec 'Matched] -> [Lexeme CategorySpec 'Replacement] -> Maybe [(Text, Text)]
    go rTarget rReplacement = concat <$> zipWithM matchLexemes rTarget rReplacement

    matchLexemes :: Lexeme CategorySpec 'Matched -> Lexeme CategorySpec 'Replacement -> Maybe [(Text, Text)]
    matchLexemes (Grapheme g) (Grapheme g') = Just $
        (,) <$> fromConvention cs g <*> fromConvention cs g'
    matchLexemes (Category (CategorySpec cmes)) (Category (CategorySpec cmes'))
        | (cms, ces) <- unzip cmes
        , (cms', ces') <- unzip cmes'
        , length cmes == length cmes'
        = if all (==Union) cms && all (==Union) cms'
            then concat <$> zipWithM go ces ces'
            else Just []
    matchLexemes (GreedyCategory (CategorySpec cmes)) (Category (CategorySpec cmes'))
        | (cms, ces) <- unzip cmes
        , (cms', ces') <- unzip cmes'
        , length cmes == length cmes'
        = if all (==Union) cms && all (==Union) cms'
            then concat <$> zipWithM go ces ces'
            else Just []
    matchLexemes (Category (CategorySpec cmes)) l
        | (cms, ces) <- unzip cmes
        = if all (==Union) cms
            then fmap concat $ for ces $ \case [l'] -> matchLexemes l' l; _ -> Nothing
            else Just []
    matchLexemes (GreedyCategory (CategorySpec cmes)) l
        | (cms, ces) <- unzip cmes
        = if all (==Union) cms
            then fmap concat $ for ces $ \case [l'] -> matchLexemes l' l; _ -> Nothing
            else Just []
    matchLexemes l (Category (CategorySpec cmes))
        | (cms, ces) <- unzip cmes
        = if all (==Union) cms
            then fmap concat $ for ces $ \case [l'] -> matchLexemes l l'; _ -> Nothing
            else Just []
    matchLexemes (Category _) Discard = Just []
    matchLexemes (GreedyCategory _) Discard = Just []
    matchLexemes (Optional       ls) (Optional ls') = go ls ls'
    matchLexemes (GreedyOptional ls) (Optional ls') = go ls ls'
    matchLexemes _ _ = Nothing

genTransitions :: [ReferenceData] -> SoundChanges -> Text
genTransitions rds sc = foldMap genEdge $ sc_contents sc
  where
    genEdge :: Edge -> Text
    genEdge e = flip foldMap (e_changes e) $ \(source, changes) ->
        let convs = r_conventions $ fromJust $ find (r_source ==& source) rds
            ts = transitions convs . ch_rule =<< changes
        in foldMap toCsvRow ts

    toCsvRow :: (Text, Text) -> Text
    toCsvRow (from, to) = from <> "," <> to <> "\n"
