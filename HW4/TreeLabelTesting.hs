module TreeLabelTesting where

import Test.HUnit

import qualified TreeLabelWithoutStateMonadTesting as WO (tests)
import qualified TreeLabelWithStateMonadTesting as W (tests)

main = runTestTT $ TestList [
  WO.tests,
  W.tests
  ]
