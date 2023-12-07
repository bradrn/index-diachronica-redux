{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ID.Generate (genPage) where

import Brassica.SoundChange.Types (Lexeme(..), Grapheme(..), Rule(..), Flags(..))
import Lucid
import Data.List (intersperse, find)
import Data.Maybe (isNothing, fromJust)
import Data.Set (Set)
import Data.Text (Text, unpack, pack)
import Text.Pandoc ()
import Text.Pandoc.Builder (Inline(..), Inlines, QuoteType(..))

import qualified Data.Set as Set
import qualified Data.Vector as V

import ID.Schemata

unlinesHtml :: [Html ()] -> Html ()
unlinesHtml = mconcat . intersperse (br_ [])

(==&) :: Eq b => (a -> b) -> b -> (a -> Bool)
(==&) f v = (==v) . f

genIPA :: [Text] -> Text
genIPA is = "[" <> mconcat (intersperse "~" is) <> "]"

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
    -- doi = case lookupVariable "DOI" r of
    --     Just (TextVal t) -> t
    --     x -> error $ "genReference: found for DOI: " ++ show x

    stars :: Int -> Html ()
    stars 3 = "★★★"
    stars 2 = "★★☆"
    stars 1 = "★☆☆"
    stars _ = "☆☆☆"

genComments :: [Text] -> Html ()
genComments [] = mempty
genComments cs = ul_ [class_ "comment"] $ foldMap (li_ . toHtml) cs

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

genLangInfo :: LangInfo -> Html ()
genLangInfo l = foldMap go (l_inventory l)
  where
    go :: Inventory -> Html ()
    go i = div_ [class_ "box lang", id_ (l_name l)] $ mconcat
        [ h3_ $ toHtml $ l_name l
        , section "Source" $ sourceLink $ i_source i
        , section "Consonant inventory" $ toTable (i_consonants i) <> genComments (i_cnotes i)
        , section "Vowel inventory" $ toTable (i_vowels i) <> genComments (i_cnotes i)
        , section "Suprasegmentals" $ genSuprasegmentals (i_supras i)
        ]

    toTable :: [[Text]] -> Html ()
    toTable = table_ [class_ "phonemes"] . foldMap (tr_ . foldMap (td_ . toHtml))

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

genLexemes :: [Convention] -> [Lexeme a] -> Html ()
genLexemes _ [] = "∅"
genLexemes convs ls_ = applySpacing " " $ go <$> ls_
  where
    go :: Lexeme a -> Spacing (Html ())
    go (Grapheme g) = Both $ renderG g
    go (Category gs) = Both $ toHtml $
        "{" <>
        mconcat (intersperse ", " $ fmap renderG gs)
        <> "}"
    go (Optional ls) = Both $ toHtml $ "(" <> genLexemes convs ls <> ")"
    go Metathesis = Both $ span_ [class_ "comment"] "reversed"
    go Geminate = After "ː"
    go (Wildcard l) = ("… " <>) <$> go l
    go (Kleene l) = (<>"…") <$> go l
    go Discard = Empty
    go (Backreference _ _) = error "genLexemes: backreferences not yet supported"
    go (Multiple gs) = go (Category gs)

    renderG :: Grapheme -> Html ()
    renderG (GMulti cs)
        | Just Convention{c_ipa} <- find (c_sym ==& pack cs) convs
        = abbr_ [title_ $ genIPA c_ipa] $ toHtml cs
        | otherwise = toHtml cs
    renderG GBoundary = "#"

genChange :: [Convention] -> Change -> Html ()
genChange convs c = addNotes $ case ch_overrides c of
    NoOverride -> mkRuleWith $ mconcat
        [ if null (environment r) then mempty else " / "
        , mconcat $ intersperse ", " $ fillHole <$> environment r
        , if isNothing (exception r) then mempty else " ! "
        , maybe mempty fillHole $ exception r
        ]
    WholeRule t -> toHtml t
    WholeEnvironment t -> mkRuleWith $ toHtml $ " / " <> t
    ExtraConditions ts -> mkRuleWith $ mconcat
        [ if null (environment r) then mempty else " / "
        , mconcat $ intersperse ", " $
            (fillHole <$> environment r) ++
            (toHtml <$> ts)
        , if isNothing (exception r) then mempty else " ! "
        , maybe mempty fillHole $ exception r
        ]
  where
    r = ch_rule c

    mkRuleWith env = mconcat
        [ genLexemes convs (target r)
        , " → "
        , genLexemes convs (replacement r)
        , env
        , if sporadic $ flags r then " (" <> i_ "sporadic" <> ")" else mempty
        ]

    fillHole ([],[]) = "_"
    fillHole (e1,[]) = genLexemes convs e1 <> " _"
    fillHole ([],e2) = "_ " <> genLexemes convs e2
    fillHole (e1,e2) = genLexemes convs e1 <> " _ " <> genLexemes convs e2

    addNotes :: Html () -> Html ()
    addNotes = case ch_notes c of
        [] -> id
        ns -> flip mappend $ ul_ [class_ "comment"] $ foldMap (li_ . toHtml) ns

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
        ( genLangInfo li
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
    -> [ReferenceData]
    -> Text
    -> Html ()
findAndGenReference refs rds ident =
    let bibentry' = lookup ident refs
        refdata' = find ((==ident) . r_source) rds
    in case (bibentry', refdata') of
        (Just bibentry, Just refdata) -> genReference ident bibentry refdata
        _ -> error $ unpack $ "Could not find reference: " <> ident

genPage
    :: V.Vector Languoid
    -> [LangInfo]
    -> [(Text, Inlines)]
    -> [ReferenceData]
    -> SoundChanges
    -> Html ()
genPage ls lis refs rds sc = html_ $ do
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
        foldMap (findAndGenReference refs rds) refset
        h2_ "Changes"
        rendered
