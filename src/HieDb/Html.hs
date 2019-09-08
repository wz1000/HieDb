{-# LANGUAGE OverloadedStrings #-}

module HieDb.Html
    ( Color (..)
    , Span (..)
    , generate
    ) where

import           Control.Monad (forM_)
import           Data.Function (on)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.List (foldl', sortBy)
import           Data.String (fromString)
import           Data.Text   (Text)
import qualified Data.Text as T
import           Lucid
import           Module

generate :: FilePath -> ModuleName -> [Text] -> [Span] -> IO ()
generate fp mn ts sps = renderToFile fp $ doctypehtml_ $ do
    head_ $ title_ $ toHtml $ moduleNameString mn
    body_ $
        forM_ (layout ts sps) generateLine'
  where
    generateLine' :: (Int, Text, [LineSpan]) -> Html ()
    generateLine' (i, t, lsps) = pre_ [style_ "margin:0em;font-size:large"] $ do
        span_ [style_ "background-color:lightcyan;padding-right:1em"] $ padLineNumber i
        go 1 t lsps

    go :: Int -> Text -> [LineSpan] -> Html ()
    go _   t [] = toHtml t
    go col t lsps@(lsp : lsps')
        | col < lspStartColumn lsp = do
            let (t1, t2) = T.splitAt (lspStartColumn lsp - col) t
            toHtml t1
            go (lspStartColumn lsp) t2 lsps
        | otherwise = do
            let l        = lspEndColumn lsp - lspStartColumn lsp + 1
                (t1, t2) = T.splitAt l t
            span_ [lineSpanAttribute lsp] $ toHtml t1
            go (lspEndColumn lsp + 1) t2 lsps'

padLineNumber :: Int -> Html ()
padLineNumber n = let s = show n in go s $ length s
  where
    go s l
        | l >= 6    = toHtml s
        | otherwise = go (' ' : s) (l + 1)

data Color = Reachable | Unreachable deriving (Show, Read, Eq, Ord)

data Span = Span
    { spStartLine   :: !Int
    , spStartColumn :: !Int
    , spEndLine     :: !Int
    , spEndColumn   :: !Int
    , spColor       :: !Color
    } deriving (Show, Read, Eq, Ord)

data LineSpan = LineSpan
    { lspLine        :: !Int
    , lspStartColumn :: !Int
    , lspEndColumn   :: !Int
    , lspColor       :: !Color
    } deriving (Show, Read, Eq, Ord)

lineSpanAttribute :: LineSpan -> Attribute
lineSpanAttribute lsp =
    let color = case lspColor lsp of
            Reachable   -> "lightgreen"
            Unreachable -> "yellow"
    in  style_ $ "background-color:" <> color

lineSpans :: (Int -> Int) -> Span -> [LineSpan]
lineSpans cols sp
    | spStartLine sp == spEndLine sp = return LineSpan
        { lspLine        = spStartLine sp
        , lspStartColumn = spStartColumn sp
        , lspEndColumn   = spEndColumn sp
        , lspColor       = spColor sp
        }
    | otherwise =
        let lsp1  = LineSpan
                        { lspLine        = spStartLine sp
                        , lspStartColumn = spStartColumn sp
                        , lspEndColumn   = cols $ spStartLine sp
                        , lspColor       = spColor sp
                        }
            lsp i = LineSpan
                        { lspLine        = i
                        , lspStartColumn = 1
                        , lspEndColumn   = cols i
                        , lspColor       = spColor sp
                        }
            lsp2  = LineSpan
                        { lspLine        = spEndLine sp
                        , lspStartColumn = 1
                        , lspEndColumn   = spEndColumn sp
                        , lspColor       = spColor sp
                        }
        in  lsp1 : [lsp i | i <- [spStartLine sp + 1 .. spEndLine sp - 1]] ++ [lsp2]

layout :: [Text] -> [Span] -> [(Int, Text, [LineSpan])]
layout ts ss =
    let m1 = IM.fromList [(i, (t, T.length t, [])) | (i, t) <- zip [1..] ts]
        m2 = foldl' f m1 ss :: IntMap (Text, Int, [LineSpan])
    in  [(i, t, lsps) | (i, (t, lsps)) <- IM.toList $ j <$> m2]
  where
    f :: IntMap (Text, Int, [LineSpan]) -> Span -> IntMap (Text, Int, [LineSpan])
    f m = foldl' g m . lineSpans lookup'
      where lookup' i = case IM.lookup i m of
                Nothing        -> 0
                Just (_, l, _) -> l

    g :: IntMap (Text, Int, [LineSpan]) -> LineSpan -> IntMap (Text, Int, [LineSpan])
    g m lsp = IM.adjust (h lsp) (lspLine lsp) m

    h :: LineSpan -> (Text, Int, [LineSpan]) -> (Text, Int, [LineSpan])
    h lsp (t, l, lsps) = (t, l, lsp : lsps)

    j :: (Text, Int, [LineSpan]) -> (Text, [LineSpan])
    j (t, _, lsps) = (t, sortBy (compare `on` lspStartColumn) lsps)
