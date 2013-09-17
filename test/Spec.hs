{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.Hspec

import Text.Heredoc

main :: IO ()
main = hspec $ do
    describe "Text.Heredoc" $ do
        it "create a multi-line string." testMultiLineString
        it "create a multi-line string whose edge is demarcated by the pipe character ('|')."
            testMultiLineStringWithPipe
        it "create a multi-line string from file."
            testMultiLineStringFromFile

testMultiLineString :: IO ()
testMultiLineString = do
    [here|line1
line2|] `shouldBe` "line1\nline2"
    [here|
line1
line2
|] `shouldBe` "line1\nline2\n"
    [here|

line1
line2|] `shouldBe` "\nline1\nline2"

testMultiLineStringWithPipe :: IO ()
testMultiLineStringWithPipe = do
    [str|line1
        |line2
      |  line3|] `shouldBe` "line1\nline2\n  line3"
    [str|
    |line1
        |line2
      |  line3|] `shouldBe` "\nline1\nline2\n  line3"

testMultiLineStringFromFile :: IO ()
testMultiLineStringFromFile = do
    [there|test/file/a1.txt|] `shouldBe` "a\nb\nc\n"
    [there|test/file/a2.txt|] `shouldBe` "\na\nb\nc\n"
