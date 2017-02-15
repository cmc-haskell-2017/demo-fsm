module FSMSpec where

import Test.Hspec

import FSM

spec :: Spec
spec = do
  describe "autoLang" $ do
    it "autoLang sampleDetAuto is correct" $ do
      autoLang sampleDetAuto `shouldBe` ["a"]

    it "autoLang sampleNonDetAuto is correct (up to first 1000 words)" $ do
      take 1000 (autoLang sampleNonDetAuto) `shouldBe` take 1000 (iterate ('a':) "b")

    it "autoLang sampleAuto is correct (up to first 5 words)" $ do
      take 5 (autoLang sampleAuto) `shouldBe` ["cca","cabca","cababca","cabababca","cababababca"]

  describe "states" $ do
    it "states (< 0) (\\s -> [('a', -1)]) 0 \"hello\"  ==>  []" $ do
      states (< 0) (\s -> [('a', -1)]) 0 "hello" `shouldBe` []
    it "states (< 0) (\\s -> [('b', -1), ('a', 1), ('a', 0)]) 0 \"aab\"  ==>  [[0,1,1,-1],[0,1,0,-1],[0,0,1,-1],[0,0,0,-1]]" $ do
      states (< 0) (\s -> [('b', -1), ('a', 1), ('a', 0)]) 0 "aab" `shouldBe` [[0,1,1,-1],[0,1,0,-1],[0,0,1,-1],[0,0,0,-1]]
    it "states (< 0) (\\s -> [('a', -1)]) 0 \"a\"  ==>  [[0,-1]]" $ do
      states (< 0) (\s -> [('a', -1)]) 0 "a" `shouldBe` [[0,-1]]
