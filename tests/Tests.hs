module Main where
import Test.HUnit(Counts(..),Test(..),runTestTT,(~?=),(~:))
import Control.Monad (unless)
import Data.List (nub, (\\), sort)
import Data.Vector.V2
import System.Exit (exitFailure)

-- modules under test
import Graphics.Triangulation.Delaunay (triangulate)

-- TODO: use test-framework
success :: Counts -> Bool
success cs = errors cs == 0 && failures cs == 0

tests ::  IO Counts
tests = runTestTT $ TestList 
  [ fewPointsTests
  , triPointsAreDistinctTests
  , trisAreDistinctTests
  , setCoverageTests
  ]

fewPointsTests = TestList 
  [ "triangulate no points"  ~: triangulate []                     ~?= []
  , "triangulate one point"  ~: triangulate [v2 1 1]               ~?= []
  , "triangulate two points" ~: triangulate [v2 1 1, v2 2 2]       ~?= []
  , "triangulate many dups"  ~: triangulate (replicate 5 (v2 1 1)) ~?= []
  ]
  where
    v2 = Vector2 

triPointsAreDistinctTests = TestList 
  [ "each tri has distinct pts" ~: filter (not . hasDistinctPoints) (triangulate pts) ~?= []
  ]
  where 
    pts = [v 0 7, v 24 33, v 10 13, v 20 0, v 22 11] where v = Vector2

trisAreDistinctTests = TestList
  [ "each (normalized) tri is distinct" ~: ts ~?= nub ts
  ]
  where
    pts = [v 0 0, v 1 0, v 1 1] where v = Vector2
    ts = fmap norm (triangulate pts)
    norm = sort . canon
    canon (Vector2 a b, Vector2 c d, Vector2 e f) = [(a,b),(c,d),(e,f)]

setCoverageTests = TestList
  [ "set coverage"     ~: uncoveredPts pts  ~?= []
  , "set coverage (2)" ~: uncoveredPts pts' ~?= []
  ]
  where
    v    = Vector2
    pts  = [v 0 0, v 1 0, v 1 1, v 0 1]
    pts' = pts ++ [v 2 0]
    uncoveredPts ps = ps \\ concatTris (triangulate ps)
    concatTris = concatMap (\(a,b,c) -> [a,b,c])

hasDistinctPoints :: (Vector2, Vector2, Vector2) -> Bool
hasDistinctPoints (p1,p2,p3) = p1/=p2 && p1/=p3 && p2/=p3

main = do
  result <- tests
  unless (success result) exitFailure

