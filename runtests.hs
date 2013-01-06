module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Tokenizer

import Control.Monad

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Parser"
            [ testCase "Assign int" (testFile "tests/assign_int.php" (show $ Seq [Expression (Assign (PHPVariable "foo") (Literal (PHPInt 1))) ]))
            , testCase "Assign bool" (testFile "tests/assign_bool.php" (
                    show $ Seq [ Expression (Assign (PHPVariable "foo") (Literal (PHPBool True)))
                               , Expression (Assign (PHPVariable "foo") (Literal (PHPBool False)))
                               ]))
            , testCase "Assign string double quote" (testFile "tests/assign_str_double_quote.php" (
                    show $ Seq [Expression (Assign (PHPVariable "foo") (Literal (PHPString "bar")))]))
            , testCase "Assign string single quote" (testFile "tests/assign_str_single_quote.php" (
                    show $ Seq [Expression (Assign (PHPVariable "foo") (Literal (PHPString "bar")))]))
            , testCase "Assign null" (testFile "tests/assign_null.php" (show $ Seq [Expression (Assign (PHPVariable "foo") (Literal PHPNull))]))
            , testCase "Assign float" (testFile "tests/assign_float.php" (show $ Seq [Expression (Assign (PHPVariable "foo") (Literal (PHPFloat 10.5)))]))
            , testCase "Assign var" (testFile "tests/assign_var.php" (show $ Seq [Expression (Assign (PHPVariable "foo") (Variable (PHPVariable "bar")))]))
            ]

testFile :: FilePath -> String -> IO ()
testFile file expected = do
    res <- readFile file
    (expected @=? (show $ parseString res))
