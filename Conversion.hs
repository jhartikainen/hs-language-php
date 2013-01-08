module Conversion where

import Tokenizer
import Text.Read
import Data.Maybe

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

isTruthy :: PHPValue -> Bool
isTruthy = getBool . castToBool
    where getBool (PHPBool b) = b

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

stringToNumeric :: PHPValue -> PHPValue
stringToNumeric (PHPString a) = if isFloat a then PHPFloat $ read a else PHPInt $ fromMaybe 0 $ readMaybe a
    where
        isFloat a = elem 'E' a || elem 'e' a || elem '.' a

stringToNumeric _ = error "Trying to convert non-string"

phpSum :: PHPValue -> PHPValue -> PHPValue
phpSum (PHPFloat a) (PHPFloat b) = PHPFloat (a + b)
phpSum (PHPInt a) (PHPInt b) = PHPInt (a + b)
phpSum a@(PHPFloat _) b = phpSum a (castToFloat b)
phpSum a b@(PHPFloat _) = phpSum (castToFloat a) b
phpSum a@(PHPInt _) b = phpSum a (castToInt b)
phpSum a b@(PHPInt _) = phpSum (castToInt a) b
phpSum a b = phpSum (castToInt a) (castToInt b)

phpSubtract :: PHPValue -> PHPValue -> PHPValue
phpSubtract a b = uncurry sub $ makeCompatible (a, b)
    where sub (PHPFloat a) (PHPFloat b) = PHPFloat (a - b)
          sub (PHPInt a) (PHPInt b) = PHPInt (a - b)

phpMultiply :: PHPValue -> PHPValue -> PHPValue
phpMultiply a b = uncurry mul $ makeCompatible (a, b)
    where mul (PHPFloat a) (PHPFloat b) = PHPFloat (a * b)
          mul (PHPInt a) (PHPInt b) = PHPInt (a * b)

phpDivide :: PHPValue -> PHPValue -> PHPValue
phpDivide a b = uncurry div $ makeCompatible (a, b)
    where div (PHPFloat a) (PHPFloat b) | b /= 0    = PHPFloat (a / b)
                                        | otherwise = PHPBool False
          div (PHPInt a) (PHPInt b) | b /= 0    = let f = (fromInteger a / fromInteger b)
                                                  in if (fromIntegral $ floor f) == f
                                                       then PHPInt (floor f)
                                                       else PHPFloat f
                                    | otherwise = PHPBool False

phpModulo :: PHPValue -> PHPValue -> PHPValue
phpModulo a b = uncurry m $ makeCompatible (a, b)
    where m (PHPInt a) (PHPInt b) | b /= 0    = PHPInt (a `mod` b)
                                  | otherwise = PHPBool False
          m a@(PHPFloat _) b@(PHPFloat _) = phpModulo (castToInt a) (castToInt b)

makeCompatible :: (PHPValue, PHPValue) -> (PHPValue, PHPValue)
makeCompatible (a@(PHPFloat _), b) = (a, castToFloat b)
makeCompatible (a, b@(PHPFloat _)) = (castToFloat a, b)
makeCompatible (a@(PHPInt _), b) = (a, castToInt b)
makeCompatible (a, b@(PHPInt _)) = (castToInt a, b)

boolAnd :: PHPValue -> PHPValue -> PHPValue
boolAnd a b = PHPBool $ (isTruthy a) && (isTruthy b)

boolOr :: PHPValue -> PHPValue -> PHPValue
boolOr a b = PHPBool $ (isTruthy a) || (isTruthy b)

boolEquals :: PHPValue -> PHPValue -> PHPValue
boolEquals a b = PHPBool $ (isTruthy a) == (isTruthy b)

boolStrictEquals :: PHPValue -> PHPValue -> PHPValue
boolStrictEquals (PHPFloat a) (PHPFloat b) = PHPBool (a == b)
boolStrictEquals (PHPInt a) (PHPInt b) = PHPBool (a == b)
boolStrictEquals (PHPString a) (PHPString b) = PHPBool (a == b)
boolStrictEquals (PHPBool a) (PHPBool b) = PHPBool (a == b)
boolStrictEquals PHPNull PHPNull = PHPBool True
boolStrictEquals _ _ = PHPBool False

boolGreater :: PHPValue -> PHPValue -> PHPValue
boolGreater a b = uncurry cmp $ makeCompatible (a, b)
    where cmp (PHPFloat a) (PHPFloat b) = PHPBool (a > b)
          cmp (PHPInt a) (PHPInt b) = PHPBool (a > b)

boolLess :: PHPValue -> PHPValue -> PHPValue
boolLess a b = uncurry cmp $ makeCompatible (a, b)
    where cmp (PHPFloat a) (PHPFloat b) = PHPBool (a < b)
          cmp (PHPInt a) (PHPInt b) = PHPBool (a < b)
