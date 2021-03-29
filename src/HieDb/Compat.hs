
{-# LANGUAGE CPP #-}
module HieDb.Compat where

import Compat.HieTypes

#if __GLASGOW_HASKELL__ >= 900
import Compat.HieUtils

import qualified Data.Map as M
import qualified Data.Set as S


-- nodeInfo' :: Ord a => HieAST a -> NodeInfo a
nodeInfo' :: HieAST TypeIndex -> NodeInfo TypeIndex
nodeInfo' = M.foldl' combineNodeInfo' emptyNodeInfo . getSourcedNodeInfo . sourcedNodeInfo

combineNodeInfo' :: Ord a => NodeInfo a -> NodeInfo a -> NodeInfo a
(NodeInfo as ai ad) `combineNodeInfo'` (NodeInfo bs bi bd) =
  NodeInfo (S.union as bs) (mergeSorted ai bi) (M.unionWith (<>) ad bd)
  where
    mergeSorted :: Ord a => [a] -> [a] -> [a]
    mergeSorted la@(a:as) lb@(b:bs) = case compare a b of
                                        LT -> a : mergeSorted as lb
                                        EQ -> a : mergeSorted as bs
                                        GT -> b : mergeSorted la bs
    mergeSorted as [] = as
    mergeSorted [] bs = bs
#else
import qualified FastString as FS

import Module

nodeInfo' :: HieAST TypeIndex -> NodeInfo TypeIndex
nodeInfo' = nodeInfo
type Unit = UnitId
unitString :: Unit -> String
unitString = unitIdString
stringToUnit :: String -> Unit
stringToUnit = stringToUnitId
moduleUnit :: Module -> Unit
moduleUnit = moduleUnitId
unhelpfulSpanFS :: FS.FastString -> FS.FastString
unhelpfulSpanFS = id
#endif

#if __GLASGOW_HASKELL__ >= 900
#else
#endif
