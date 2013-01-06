module Conversion where

import Tokenizer

phpValToBool :: PHPValue -> Bool
phpValToBool (PHPString a) | a == ""   = False
                           | a == "0"  = False
                           | otherwise = True

phpValToBool (PHPInt a) | a == 0    = False
                        | otherwise = True

phpValToBool (PHPBool a) = a

phpValToBool (PHPFloat a) | a == 0    = False
                          | otherwise = True

phpValToBool PHPNull = False

phpValToInt :: PHPValue -> Integer
phpValToInt (PHPString _) = error "string to int behavior is not implemented"
phpValToInt (PHPInt a) = a
phpValToInt (PHPFloat a) = floor a
phpValToInt (PHPBool a) | a == True  = 1
                        | a == False = 0
phpValToInt PHPNull = 0
