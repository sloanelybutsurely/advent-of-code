#!/usr/bin/env runghc
module Day8.Part1 where

import Day8

main = interact (show . acc . run . parse program)
