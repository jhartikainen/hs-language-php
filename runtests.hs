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
            [ testCase "Assign int" (testFile "tests/assign_int.php" (show $ [PHPCode $ Seq [Expression (Assign (PHPVariable "foo") (Literal (PHPInt 1))) ]]))
            , testCase "Assign bool" (testFile "tests/assign_bool.php" (
                    show $ [PHPCode $ Seq [ Expression (Assign (PHPVariable "foo") (Literal (PHPBool True)))
                               , Expression (Assign (PHPVariable "foo") (Literal (PHPBool False)))
                               ]]))
            , testCase "Assign string double quote" (testFile "tests/assign_str_double_quote.php" (
                    show $ [PHPCode $ Seq [Expression (Assign (PHPVariable "foo") (Literal (PHPString "bar")))]]))
            , testCase "Assign string single quote" (testFile "tests/assign_str_single_quote.php" (
                    show $ [PHPCode $ Seq [Expression (Assign (PHPVariable "foo") (Literal (PHPString "bar")))]]))
            , testCase "Assign null" (testFile "tests/assign_null.php" (show $ [PHPCode $ Seq [Expression (Assign (PHPVariable "foo") (Literal PHPNull))]]))
            , testCase "Assign float" (testFile "tests/assign_float.php" (show $ [PHPCode $ Seq [Expression (Assign (PHPVariable "foo") (Literal (PHPFloat 10.5)))]]))
            , testCase "Assign var" (testFile "tests/assign_var.php" (show $ [PHPCode $ Seq [Expression (Assign (PHPVariable "foo") (Variable (PHPVariable "bar")))]]))
            , testCase "String: single quote escape" (testFile "tests/string_single_quote_escape.php" (show $ [PHPCode $ Seq [Expression (Literal $ PHPString "foo'bar")]]))
            , testCase "String: double quote escape" (testFile "tests/string_double_quote_escape.php" (show $ [PHPCode $ Seq [Expression (Literal $ PHPString "foo\"bar")]]))
            , testCase "If, plain" (testFile "tests/if_plain.php" (show $ [PHPCode $ Seq [If (Literal (PHPBool True)) (Seq []) Nothing]]))
            , testCase "If, else" (testFile "tests/if_else.php" (show $ [PHPCode $ Seq [If (Literal (PHPBool True)) (Seq []) (Just (Else (Seq [])))]]))
            , testCase "If, elseif, else" (testFile "tests/if_elseif_else.php" (
                    show $ [PHPCode $ Seq [If (Literal (PHPBool True)) (Seq []) 
                                  (Just (ElseIf (Literal (PHPBool True)) (Seq []) 
                                    (Just (Else (Seq [])))))]]))
            , testCase "Math: simple add" (testFile "tests/math_simple_add.php" (
                    show $ [PHPCode $ Seq [Expression (BinaryExpr Add (Literal (PHPInt 1)) (Literal (PHPInt 1)))]]))
            , testCase "Call: one arg" (testFile "tests/call_1_arg.php" (
                    show $ [PHPCode $ Seq [Expression (Call (FunctionCall "test") [Literal (PHPInt 1)])]]))
            , testCase "Call: two args with whitespace" (testFile "tests/call_two_with_white.php" (
                    show $ [PHPCode $ Seq [Expression (Call (FunctionCall "test") [Literal (PHPBool True),Literal (PHPInt 1)])]]))
            , testCase "Function: No args, no body" (testFile "tests/func_no_args_no_body.php" (
                    show $ [PHPCode $ Seq [Function "foo" [] (Seq [])]]))
            , testCase "Function: Some args, no body" (testFile "tests/func_some_args_no_body.php" (
                    show $ [PHPCode $ Seq [Function "foo" [ FunctionArgumentDef {argName = "test", argDefault = Nothing}
                                               , FunctionArgumentDef {argName = "test2", argDefault = Nothing}] (Seq [])]]))
            , testCase "Function: No args, simple body" (testFile "tests/func_no_args_simple_body.php" (
                    show $ [PHPCode $ Seq [Function "foo" [] (Seq [Expression (Assign (PHPVariable "hello") (Literal (PHPInt 1)))])]]))
            , testCase "Function: Args with defaults" (testFile "tests/func_args_with_defaults.php" (
                    show $ [PHPCode $ Seq [Function "x" [ FunctionArgumentDef {argName = "a", argDefault = Just (PHPInt 1)}
                                             , FunctionArgumentDef {argName = "b", argDefault = Nothing}] (Seq [])]]))
            , testCase "Return" (testFile "tests/return.php" (show $ [PHPCode $ Seq [Return (Literal (PHPBool True))]]))
            , testCase "While: simple cond no body" (testFile "tests/while_simple_no_body.php" (show $ [PHPCode $ Seq [While (Literal (PHPBool True)) (Seq [])]]))
            , testCase "While: cond and body" (testFile "tests/while_cond_and_body.php" (
                    show $ [PHPCode $ Seq [While (BinaryExpr Less (Variable (PHPVariable "i")) (Literal (PHPInt 5))) (Seq
                                    [ Expression (Assign (PHPVariable "i") (BinaryExpr Add (Variable (PHPVariable "i")) (Literal (PHPInt 1))))])]]))
            , testCase "Plaintext: space after PHP stmt" (testFile "tests/plaintext_space_after_php.php" (show $ [PHPCode $ Seq [Expression $ Literal $ PHPInt 1], PlainText " foo"]))
            , testCase "Plaintext: multiline file" (testFile "tests/plaintext_multiline_file.php" (show $ [PlainText "foo\nbar\nbaz"]))
            ]

testFile :: FilePath -> String -> IO ()
testFile file expected = do
    res <- readFile file
    (expected @=? (show $ parseString res))
