#!/usr/bin/env runghc
module Day7.Part1 where

import Day7
import Data.Array
import Data.Graph
import Data.Maybe

main = interact (show . solve "shiny gold" . parse rules)

-- solve myBag rs = (+) (-1) $ forestSize $ dfs graph [vertex]
-- solve myBag rs = length $ keys -- filter (path graph vertex . fromJust . vertexFromKey) keys
solve myBag rs = graph
  where keys = filter (/= myBag) $ map ruleToKey rs
        (graph, keyFromVertex, vertexFromKey) = graphFromEdges . map ruleToEdge $ rs
        Just vertex = vertexFromKey myBag
        

ruleToKey = name . container

forestSize [] = 0
forestSize ((Node _ branches):siblings) = nodeSum + siblingSum + branchSum
  where
    nodeSum = 1
    siblingSum = forestSize siblings
    branchSum = forestSize branches

ruleToEdge :: Rule -> (String, String, [String])
ruleToEdge r = (node, node, edges)
  where
    node = name . container $ r
    edges = map (name . item) . contents $ r
