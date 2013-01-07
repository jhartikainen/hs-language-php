module Conversion where

import Tokenizer

castToBool :: PHPValue -> PHPValue
castToBool (PHPString a) | a == ""   = PHPBool False
                         | a == "0"  = PHPBool False
                         | otherwise = PHPBool True

castToBool (PHPInt a) | a == 0    = PHPBool False
                      | otherwise = PHPBool True

castToBool a@(PHPBool _) = a

castToBool (PHPFloat a) | a == 0    = PHPBool False
                        | otherwise = PHPBool True

castToBool PHPNull = PHPBool False

castToInt :: PHPValue -> PHPValue
castToInt (PHPString _) = error "string to int behavior is not implemented"
castToInt a@(PHPInt _) = a
castToInt (PHPFloat a) = PHPInt $ floor a
castToInt (PHPBool a) | a == True  = PHPInt 1
                      | a == False = PHPInt 0
castToInt PHPNull = PHPInt 0

castToFloat :: PHPValue -> PHPValue
castToFloat (PHPString _) = error "undefined behavior for string to float"
castToFloat (PHPInt a) = PHPFloat $ fromInteger a
castToFloat a@(PHPFloat _) = a
castToFloat PHPNull = PHPFloat 0
castToFloat (PHPBool a) | a == True  = PHPFloat 1
                        | a == False = PHPFloat 0

castToString :: PHPValue -> PHPValue
castToString a@(PHPString _) = a
castToString (PHPInt a) = PHPString (show a)
castToString (PHPFloat a) = PHPString (show a)
castToString a@(PHPBool _) = castToString $ castToInt a
castToString PHPNull = PHPString ""
