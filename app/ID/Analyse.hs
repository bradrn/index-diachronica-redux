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
       )
import Control.Applicative (liftA2)
import Data.List (find)
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Traversable (for)

import qualified Data.Set as Set

import ID.Schemata
import Data.Maybe (fromJust)
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
transitions cs r =
    let rtl = applyDirection (flags r) == RTL
        rTarget      = if rtl then reverse (target r) else target r
        rReplacement = if rtl then reverse (replacement r) else replacement r
    in case (traverse getGrapheme rTarget, traverse getGrapheme rReplacement) of
        (Just rTarget', Just rReplacement') ->
            zip rTarget' rReplacement' >>= many2many
        _ -> []
  where
    getGrapheme :: Lexeme CategorySpec a -> Maybe [[Text]]
    getGrapheme (Grapheme g) = Just [fromConvention cs g]
    getGrapheme (Category (MustInline c)) = Just [[pack c]]
    getGrapheme (Category (CategorySpec cmes))
        | (cms, ces) <- unzip cmes
        , all (==Union) cms
        = for ces $ \case [Grapheme g] -> Just (fromConvention cs g); _ -> Nothing
    getGrapheme _ = Nothing

    many2many :: ([[a]], [[a]]) -> [(a, a)]
    -- map all values in 'as' to all values in 'a'
    many2many (as@(_:_), [a]) = (,) <$> concat as <*> a
    many2many ([a], as@(_:_)) = (,) <$> a <*> concat as
    many2many (as, as') = concat $ zipWith (liftA2 (,)) as as'

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
