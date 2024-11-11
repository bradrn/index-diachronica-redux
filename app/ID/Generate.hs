{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ID.Generate (genPage, genIndex) where

import Brassica.SoundChange.Types
    ( Lexeme(..)
    , Grapheme
    , Rule(..)
    , Flags(..)
    , CategorySpec(..)
    , CategoryModification (..), Sporadicity (ApplyAlways), Environment
    )
import Control.Monad.State.Strict (State, evalState, gets, modify)
import Citeproc (Val(TextVal), lookupVariable, Reference(..), CiteprocOutput, resultBibliography, Style, citeproc, CiteprocOptions (..), unItemId)
import Lucid hiding (for_)
import Lucid.Base (commuteHtmlT2)
import Data.Foldable (for_)
import Data.List (intersperse, find, sortOn)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.Text (Text, unpack, pack)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (ISO8601(iso8601Format), formatShow)
import Text.Pandoc ()
import Text.Pandoc.Builder (Inline(..), Inlines, QuoteType(..))

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as V

import ID.Schemata

unlinesHtml :: [Html ()] -> Html ()
unlinesHtml = mconcat . intersperse (br_ [])

(==&) :: Eq b => (a -> b) -> b -> (a -> Bool)
(==&) f v = (==v) . f

genIPA :: [Text] -> Text
genIPA is = "[" <> mconcat (intersperse "~" is) <> "]"

formatted :: Text -> Html ()
formatted = mconcat . intersperse " " . fmap go . Text.words
  where
    go x
        | Just ('\'', x' ) <- Text.uncons x
        , Just (x'', '\'') <- Text.unsnoc x'
        = span_ [class_ "phoneme"] $ toHtml x''
        | otherwise = toHtml x

genConventions :: [Convention] -> Html ()
genConventions cs = table_ [class_ "transcription"] $ header <> body
  where
    header = tr_ $ th_ "Symbol" <> th_ "Likely IPA" <> th_ "Notes"

    body = flip foldMap cs $ \Convention{c_sym,c_ipa,c_explicit,c_notes} -> tr_ $ mconcat
      [ td_ $ toHtml c_sym
      , td_ $ toHtml $ genIPA c_ipa
      , td_ $ unlinesHtml $
          genMessage c_explicit ++ fmap toHtml c_notes
      ]

    genMessage True = []
    genMessage False = ["(" <> i_ "unclear from source" <> ")"]

renderInlines :: Foldable t => t Inline -> Html ()
renderInlines = foldMap renderInline
  where
    renderInline :: Inline -> Html ()
    renderInline (Str t) = toHtml t
    renderInline (Emph is) = i_ $ renderInlines is
    renderInline (Underline is) = ul_ $ renderInlines is
    renderInline (Strong is) = b_ $ renderInlines is
    renderInline (Strikeout is) = term "s" $ renderInlines is
    renderInline (Superscript is) = sup_ $ renderInlines is
    renderInline (Subscript is) = sub_ $ renderInlines is
    renderInline (SmallCaps is) = span_ [style_ "font-variant-caps: small-caps;"] $ renderInlines is
    renderInline (Quoted SingleQuote is) = "‘" <> renderInlines is <> "’"
    renderInline (Quoted DoubleQuote is) = "“" <> renderInlines is <> "”"
    renderInline (Cite _ _) = error "renderInlines: did not expect citation in bibliography!"
    renderInline (Code _ t) = pre_ $ toHtml t
    renderInline Space = " "
    renderInline SoftBreak = mempty
    renderInline LineBreak = br_ []
    renderInline (Math _ _) = error "renderInlines: Math not yet supported"
    renderInline (RawInline _ t) = toHtmlRaw t
    renderInline (Link _ is (link, _)) = a_ [href_ link] $ renderInlines is
    renderInline (Image _ _ _) = error "renderInlines: did not expect image in bibliography!"
    renderInline (Note _) = error "renderInlines: did not expect image in bibliography!"
    renderInline (Span _ is) = renderInlines is

section :: Text -> Html () -> Html ()
section name html = p_ $ b_ (toHtml $ name <> ":") <> " " <> html

genReference :: Text -> Inlines -> ReferenceData -> Html ()
genReference doi ref rd = div_ [class_ "box reference", id_ (r_source rd)] $ mconcat
    [ h3_ $ toHtml $ "[" <> r_source rd <> "]"
    , section "Reference" $ renderInlines ref
    , section "DOI" $ toHtml doi
    , section "Transcribed by" $ toHtml $ r_transcriber rd
    , section "Reviewed by" $ maybe mempty toHtml $ r_reviewer rd
    , section "Ordering" $ stars $ r_ordering rd
    , section "Exhaustivity" $ stars $ r_exhaustivity rd
    , section "Detail" $ stars $ r_detail rd
    , section "Consensus" $ stars $ r_consensus rd
    , section "Transcription" $ stars $ r_transcription rd
    , section "Transcription conventions" $
        i_ "(where different to IPA)" <> genConventions (r_conventions rd)
    ]
  where
    stars :: Int -> Html ()
    stars 3 = "★★★"
    stars 2 = "★★☆"
    stars 1 = "★☆☆"
    stars _ = "☆☆☆"

genComments :: [Text] -> Html ()
genComments [] = mempty
genComments cs = ul_ [class_ "comment"] $ foldMap (li_ . formatted) cs

genSuprasegmentals :: [Suprasegmental] -> Html ()
genSuprasegmentals ss = table_ [class_ "suprasegmentals"] $ header <> body
  where
    header = tr_ $ th_ "Features" <> th_ "Notes"

    body = flip foldMap ss $ tr_ . mconcat . \case
        Binary val ns ->
            [ td_ $ toHtml $ "[±" <> val <> "]"
            , td_ $ unlinesHtml $ toHtml <$> ns
            ]
        Multi vals ns ->
            [ td_ $ unlinesHtml $ toHtml <$> vals
            , td_ $ unlinesHtml $ toHtml <$> ns
            ]

sourceLink :: Text -> Html ()
sourceLink s = a_ [href_ $ "#" <> s] $ toHtml $ "[" <> s <> "]"

genLangInfo :: [ReferenceData] -> V.Vector Languoid -> LangInfo -> Html ()
genLangInfo rds ls l = foldMap go (l_inventory l)
  where
    go :: Inventory -> Html ()
    go i =
        let convs = r_conventions $ fromJust $ find (r_source ==& i_source i) rds
        in div_ [class_ "box lang", id_ (l_root l)] $ mconcat
            [ h3_ $ toHtml $ lookupByID ls (l_root l)
            , section "Source" $ sourceLink $ i_source i
            , section "Consonant inventory" $ toTable convs (i_consonants i) <> genComments (i_cnotes i)
            , section "Vowel inventory" $ toTable convs (i_vowels i) <> genComments (i_cnotes i)
            , section "Suprasegmentals" $ genSuprasegmentals (i_supras i)
            ]

    toTable :: [Convention] -> [[Text]] -> Html ()
    toTable convs = table_ [class_ "phonemes"] . foldMap (tr_ . foldMap (td_ . fromConvention convs))

    fromConvention :: [Convention] -> Text -> Html ()
    fromConvention convs cs
        | Just Convention{c_ipa} <- find (c_sym ==& cs) convs
        = abbr_ [title_ $ genIPA c_ipa] $ toHtml cs
        | otherwise = toHtml cs

data Spacing a = Both a | After a | Empty
    deriving (Show, Functor)

applySpacing :: Monoid m => m -> [Spacing m] -> m
applySpacing __ = extract . go
  where
    extract (Both a) = a
    extract (After a) = a
    extract Empty = mempty

    go [] = Empty
    go [s] = s
    go (Both a:Both b:ss) = go $ Both (a<>__<>b):ss
    go (Both a:After b:ss) = go $ Both (a<>b):ss
    go (After a:Both b:ss) = go $ After (a<>__<>b):ss
    go (After a:After b:ss) = go $ After (a<>b):ss
    go (s:Empty:ss) = go (s:ss)
    go (Empty:s:ss) = go (s:ss)

type GenM = HtmlT (State (Map String Int))

genLexemes :: [Convention] -> [Lexeme CategorySpec a] -> State (Map String Int) (Html ())
genLexemes _ [] = pure "∅"
genLexemes convs ls_ = commuteHtmlT2 $ genLexemes' ls_
  where
    genLexemes' :: [Lexeme CategorySpec a] -> GenM ()
    genLexemes' [] = "∅"
    genLexemes' ls = applySpacing " " $ go <$> ls

    go :: Lexeme CategorySpec a -> Spacing (GenM ())
    go (Grapheme g) = Both $ renderG g
    go (Category s) = Both $ renderSpec s
    go (GreedyCategory s) = Both $ renderSpec s
    go (Optional ls) = Both $ "(" <> genLexemes' ls <> ")"
    go (GreedyOptional ls) = Both $ "(" <> genLexemes' ls <> ")"
    go Metathesis = Both $ span_ [class_ "comment"] "reversed"
    go Geminate = After "ː"
    go (Wildcard l) = ("… " <>) <$> go l
    go (Kleene l) = (<>"…") <$> go l
    go Discard = Empty
    go (Backreference (Right _) _) = error "Numeric backreferences not supported"
    go (Backreference (Left i) s) = Both $ do
        sub <- subscriptFor i
        subscriptWith sub $ renderSpec s
    go (Multiple gs) = go (Category gs)
    go (Feature neg name i _ l) = Both $ do
        sub <- traverse subscriptFor i
        -- not 100% correct but at least consistent with 'renderSpec'
        "{" <> genLexemes' [l]
            <> (if neg then "∓" else "±")
            <> case sub of
                    Just sub' -> subscriptWith sub' $ toHtml name
                    Nothing -> toHtml name
            <> "}"
    go Autosegment {} = error "Autosegments should not appear"

    subscriptFor :: String -> GenM Int
    subscriptFor i =
        gets (Map.lookup i) >>= \case
            Nothing -> do
                sub <- gets (\m -> if Map.null m then 1 else 1 + maximum m)
                modify $ Map.insert i sub
                pure sub
            Just sub -> pure sub

    subscriptWith :: Int -> GenM () -> GenM ()
    subscriptWith n h = h <> sub_ (toHtml $ show n)

    renderSpec :: CategorySpec a -> GenM ()
    renderSpec (MustInline g) = toHtml g
    renderSpec (CategorySpec ((Union,g):gs)) =
        "{"
        <> genLexemes' g
        <> foldMap renderMod gs
        <> "}"
    renderSpec (CategorySpec s) = error $ "genLexemes: meaningless category: " ++ show s

    renderMod :: (CategoryModification, [Lexeme CategorySpec a]) -> GenM ()
    renderMod (Union, g) = ", " <> genLexemes' g
    renderMod (Intersect, g) = "+" <> genLexemes' g
    renderMod (Subtract, g) = "-" <> genLexemes' g

    renderG :: Grapheme -> GenM ()
    renderG cs
        | Just Convention{c_ipa} <- find (c_sym ==& pack cs) convs
        = abbr_ [title_ $ genIPA c_ipa] $ toHtml cs
        | otherwise = toHtml cs

genChange :: [Convention] -> Change -> Html ()
genChange convs c = addNotes $ case ch_overrides c of
    NoOverride -> mconcat
        [ ruleStart
        , if null envs' then mempty else " / "
        , mconcat $ intersperse ", " envs'
        , case exception' of
              Nothing -> mempty
              Just e -> " ! " <> e
        , sporadicity
        ]
    WholeRule t -> formatted t
    WholeEnvironment t -> mconcat
        [ ruleStart
        , " / " <> formatted t
        , sporadicity
        ]
    ExtraConditions ts -> mconcat
        [ ruleStart
        , if null envs' then mempty else " / "
        , mconcat $ intersperse ", " $ envs' ++ (formatted <$> ts)
        , case exception' of
              Nothing -> mempty
              Just e -> " ! " <> e
        , sporadicity
        ]
  where

    ruleStart = mconcat [target', " → ", replacement']
    sporadicity
        | sporadic (flags (ch_rule c)) /= ApplyAlways = " (" <> i_ "sporadic" <> ")"
        | otherwise = mempty

    (target', replacement', envs', exception') = flip evalState Map.empty $ do
        let r = ch_rule c
        t <- genLexemes convs $ target r
        p <- genLexemes convs $ replacement r
        es <- traverse fillHole (environment r)
        ex <- traverse fillHole (exception r)
        pure (t, p, es, ex)

    fillHole :: Environment CategorySpec -> State (Map String Int) (Html ())
    fillHole ([], []) = pure "_"
    fillHole (e1,[]) = (<> " _") <$> genLexemes convs e1
    fillHole ([],e2) = ("_ " <>) <$> genLexemes convs e2
    fillHole (e1,e2) = do
        e1' <- genLexemes convs e1
        e2' <- genLexemes convs e2
        pure $ e1' <> " _ " <> e2'

    addNotes :: Html () -> Html ()
    addNotes = case ch_notes c of
        [] -> id
        ns -> flip mappend $ genComments ns


lookupByID :: V.Vector Languoid -> Text -> Text
lookupByID ls i = maybe i fst $ V.uncons (V.mapMaybe go ls)
  where
    go l
        | la_id l == i = Just $ la_name l
        | otherwise    = Nothing

genEdge :: [ReferenceData] -> V.Vector Languoid -> Edge -> Html ()
genEdge rds ls e = div_ [class_ "box changes", id_ (e_from e <> "-" <> e_to e)] $
    header <> foldMap body (e_changes e)
  where
    header = h3_ $ toHtml $
        lookupByID ls (e_from e) <> " to " <> lookupByID ls (e_to e)
    body (source, changes) =
        let convs = r_conventions $ fromJust $ find (r_source ==& source) rds
        in
            section "Source" (sourceLink source)
            <> ul_ (foldMap (li_ . genChange convs) changes)

data ContentsLine = EdgeLine Text Text | InfoLine Text
    deriving (Show)

genSoundChanges
    :: [ReferenceData]
    -> V.Vector Languoid
    -> [LangInfo]
    -> [Edge]
    -> (Html (), [ContentsLine], Set Text)
genSoundChanges rds ls lis es = foldMap genTree roots
  where
    roots :: Set Text
    roots = Set.fromList $ e_from <$> filter e_root es

    genTree :: Text -> (Html (), [ContentsLine], Set Text)
    genTree from =
        foldMap genLangInfo' (filter (l_root ==& from) lis)
        <> foldMap genEdgesFrom (filter (e_from ==& from) es)

    genLangInfo' :: LangInfo -> (Html (), [ContentsLine], Set Text)
    genLangInfo' li =
        ( genLangInfo rds ls li
        , [InfoLine $ l_root li]
        , Set.fromList $ i_source <$> l_inventory li
        )

    genEdgesFrom :: Edge -> (Html (), [ContentsLine], Set Text)
    genEdgesFrom e =
        ( genEdge rds ls e
        , [EdgeLine (e_from e) (e_to e)]
        , Set.fromList $ fst <$> e_changes e
        )
        <> genTree (e_to e)

genContents :: V.Vector Languoid -> [ContentsLine] -> Html ()
genContents ls = ul_ [class_ "contents"] . foldMap (li_ . genLine)
  where
    genLine :: ContentsLine -> Html ()
    genLine (EdgeLine from to) =
        a_ [href_ ("#" <> from <> "-" <> to)] $ toHtml $
            lookupByID ls from <> " to " <> lookupByID ls to
    genLine (InfoLine l) =
        a_ [href_ ("#" <> l)] $ toHtml $ lookupByID ls l

findAndGenReference
    :: [(Text, Inlines)]
    -> [Reference Inlines]
    -> [ReferenceData]
    -> Text
    -> Html ()
findAndGenReference bib refs rds ident =
    let bibentry' = lookup ident bib
        Just ref = find ((==ident) . unItemId . referenceId) refs
        refdata' = find ((==ident) . r_source) rds
    in case (bibentry', refdata') of
        (Just bibentry, Just refdata) -> genReference (doi ref) bibentry refdata
        _ -> error $ unpack $ "Could not find reference: " <> ident
  where
    doi :: CiteprocOutput a => Reference a -> Text
    doi r = case lookupVariable "DOI" r of
        Just (TextVal t) -> t
        Nothing -> "(none)"
        x -> error $ "found for DOI: " ++ show x

genPage
    :: V.Vector Languoid
    -> [LangInfo]
    -> [Reference Inlines]
    -> Style Inlines
    -> [ReferenceData]
    -> SoundChanges
    -> Html ()
genPage ls lis refs style rds sc = html_ $ do
    let os = CiteprocOptions
            { linkCitations = False
            , linkBibliography = True
            }
        bib = resultBibliography $ citeproc os style Nothing refs []
    head_ $ do
        meta_ [charset_ "utf-8"]
        title_ $ toHtml $ "Sound changes: " <> sc_title sc
        link_ [href_ "style.css", rel_ "stylesheet"]
    body_ $ do
        let (rendered, contents, refset) = genSoundChanges rds ls lis (sc_contents sc)
        h1_ $ toHtml $ "Sound changes: " <> sc_title sc
        h2_ "Contents"
        genContents ls contents
        h2_ "Sources"
        foldMap (findAndGenReference bib refs rds) refset
        h2_ "Changes"
        rendered

genIndex :: UTCTime -> [(Text, Text)] -> Html ()
genIndex time pages = html_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        title_ "Index Diachronica Redux"
        link_ [href_ "style.css", rel_ "stylesheet"]
    body_ $ do
        h1_ "Index Diachronica Redux"
        p_ $ toHtml $ "(generated at " ++ formatShow iso8601Format time ++ ")"
        p_ $ do
            "Contents:"
            ul_ [] $
                for_ (sortOn fst pages) $ \(title, url) ->
                    li_ $ a_ [href_ url] $ toHtml title
