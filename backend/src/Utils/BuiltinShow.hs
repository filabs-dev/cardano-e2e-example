{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Utils.BuiltinShow where

import qualified PlutusTx.AssocMap           as M
import           Ledger.Value                as Value
import           Ledger                      hiding (singleton, TxOut, txOutAddress, txOutValue, txOutDatumHash)
import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Api
import           PlutusTx.Builtins.Internal  (BuiltinInteger, modInteger, divideInteger)

class BuiltinShow a where
  bshow :: a -> BuiltinString

instance BuiltinShow () where
  {-# INLINABLE bshow #-}
  bshow () = "()"

instance BuiltinShow BuiltinByteString where
  {-# INLINABLE bshow #-}
  bshow c = concatByteString (lengthOfByteString c)
    where
      concatByteString :: BuiltinInteger -> BuiltinString
      concatByteString n
        | n==0 = ""
        | otherwise =
          let
            m = intToHex (modInteger (indexByteString c (n-1)) 16)
            l = intToHex (divideInteger (indexByteString c (n-1)) 16)
          in concatByteString (n-1) <> l <> m

instance BuiltinShow BuiltinString where
  {-# INLINABLE bshow #-}
  bshow = id

instance BuiltinShow Integer where
  {-# INLINABLE bshow #-}
  bshow = decodeUtf8 . integerToBS

instance BuiltinShow a => BuiltinShow [a] where
  {-# INLINABLE bshow #-}
  bshow a = braces $ iterShow a
    where
      iterShow [] = ""
      iterShow [x] = bshow x
      iterShow (x:xs) = bshow x <> "," <> iterShow xs

instance (BuiltinShow a, BuiltinShow b) => BuiltinShow (M.Map a b) where
    {-# INLINABLE bshow #-}
    bshow a = "Map " <> bshow (M.toList a)

instance (BuiltinShow a, BuiltinShow b) => BuiltinShow (a,b) where
  {-# INLINABLE bshow #-}
  bshow (a,b) = parenthesis (bshow a <> "," <> bshow b)

instance BuiltinShow Bool where
  {-# INLINABLE bshow #-}
  bshow True = "True"
  bshow False = "False"

instance BuiltinShow a => BuiltinShow (Maybe a) where
  {-# INLINABLE bshow #-}
  bshow Nothing = "Nothing"
  bshow (Just a) = "Just " <> parenthesis (bshow a)

instance BuiltinShow POSIXTime where
  {-# INLINABLE bshow #-}
  bshow POSIXTime{..} = "POSIXTime " <> bshow getPOSIXTime

instance BuiltinShow a => BuiltinShow (Extended a) where
  {-# INLINABLE bshow #-}
  bshow NegInf = "NegInf"
  bshow PosInf = "PosInf"
  bshow (Finite a) = "Finite " <> bshow a

instance BuiltinShow a => BuiltinShow (LowerBound a) where
  {-# INLINABLE bshow #-}
  bshow (LowerBound a b) = "LowerBound " <> parenthesis (bshow a) <> " " <> bshow b

instance BuiltinShow a => BuiltinShow (UpperBound a) where
  {-# INLINABLE bshow #-}
  bshow (UpperBound a b) = "UpperBound " <> parenthesis (bshow a) <> " " <> bshow b

instance BuiltinShow a => BuiltinShow (Interval a) where
  {-# INLINABLE bshow #-}
  bshow (Interval a b) = "Interval " <> parenthesis (bshow a) <> " " <> parenthesis (bshow b)

instance BuiltinShow CurrencySymbol where
  {-# INLINABLE bshow #-}
  bshow = bshow . unCurrencySymbol

instance BuiltinShow TokenName where
  {-# INLINABLE bshow #-}
  bshow = decodeUtf8 . unTokenName

instance BuiltinShow Value where
  {-# INLINABLE bshow #-}
  bshow (Value a) = "Value " <> parenthesis (bshow a)

instance BuiltinShow PubKeyHash where
  {-# INLINABLE bshow #-}
  bshow PubKeyHash{..} = bshow getPubKeyHash

instance BuiltinShow PaymentPubKeyHash where
  {-# INLINABLE bshow #-}
  bshow PaymentPubKeyHash{..} = "PaymentPubKeyHash " <> bshow unPaymentPubKeyHash

instance BuiltinShow TxId where
  {-# INLINABLE bshow #-}
  bshow TxId{..} = "TxId " <> bshow getTxId

instance BuiltinShow DatumHash where
  {-# INLINABLE bshow #-}
  bshow (DatumHash a) = "DatumHash " <> bshow a

instance BuiltinShow Datum where
  {-# INLINABLE bshow #-}
  bshow = const "BuiltinData"

instance BuiltinShow ValidatorHash where
  {-# INLINABLE bshow #-}
  bshow (ValidatorHash a) = bshow a

instance BuiltinShow Credential where
  {-# INLINABLE bshow #-}
  bshow (PubKeyCredential a) = "PubKeyCredential " <> bshow a
  bshow (ScriptCredential a) = "ScriptCredential " <> bshow a

instance BuiltinShow StakingCredential where
  {-# INLINABLE bshow #-}
  bshow (StakingHash a) = "StakingHash " <> bshow a
  bshow (StakingPtr a b c) = "StakingPtr "
                          <> bshow a <> " "
                          <> bshow b <> " "
                          <> bshow c

instance BuiltinShow Address where
  {-# INLINABlE bshow #-}
  bshow Address{..} = "Address " <> curlyBraces (
                      "addressCredential=" <> bshow addressCredential
                   <> ", addressStakingCredential=" <>bshow addressStakingCredential
                   )

instance BuiltinShow TxOut where
  {-# INLINABLE bshow #-}
  bshow TxOut{..} = "TxOut " <> curlyBraces (
                    "txOutAddress=" <> bshow txOutAddress
                 <> ", txOutValue=" <> bshow txOutValue
                 <> ", txOutDatumHash=" <> bshow txOutDatumHash
                 )

instance BuiltinShow TxOutRef where
  {-# INLINABLE bshow #-}
  bshow TxOutRef{..} = "TxOutRef " <> curlyBraces (
                       "txOutRefId=" <> bshow txOutRefId
                    <> ", txOutRefIdx=" <> bshow txOutRefIdx
                    )

instance BuiltinShow TxInInfo where
  {-# INLINABLE bshow #-}
  bshow TxInInfo{..} = "TxInInfo " <> curlyBraces (
                       "txInInfoOutRef=" <> bshow txInInfoOutRef
                    <> ", txInInfoResolved=" <> bshow txInInfoResolved
                    )

instance BuiltinShow DCert where
  {-# INLINABLE bshow #-}
  bshow (DCertDelegRegKey a) = "DCertDelegRegKey " <> bshow a
  bshow (DCertDelegDeRegKey a) = "DCertDelegDeRegKey " <> bshow a
  bshow (DCertDelegDelegate a b) = "DCertDelegDelegate " <> bshow a <> " " <> bshow b
  bshow (DCertPoolRegister a b) = "DCertPoolRegister " <> bshow a <> " " <> bshow b
  bshow (DCertPoolRetire a b) = "DCertPoolRetire " <> bshow a <> " " <> bshow b
  bshow DCertGenesis = "DCertGenesis"
  bshow DCertMir = "DCertMir"

instance BuiltinShow TxInfo where
  {-# INLINABLE bshow #-}
  bshow TxInfo{..} = "TxInfo " <> curlyBraces (
                     "TxInfoInputs=" <> bshow txInfoInputs
                  <> ", txInfoOutputs=" <> bshow txInfoOutputs
                  <> ", txInfoFee=" <> bshow txInfoFee
                  <> ", txInfoMint=" <> bshow txInfoMint
                  <> ", txInfoDCert=" <> bshow txInfoDCert
                  <> ", txInfoWdrl=" <> bshow txInfoWdrl
                  <> ", txInfoValidRange=" <> bshow txInfoValidRange
                  <> ", txInfoSignatiories=" <> bshow txInfoSignatories
                  <> ", txInfoData=" <> bshow txInfoData
                  <> ", txInfoId=" <> bshow txInfoId
                  )

instance BuiltinShow ScriptPurpose where
  {-# INLINABLE bshow #-}
  bshow (Minting a) = "Minting " <> bshow a
  bshow (Spending a) = "Spending " <> bshow a
  bshow (Rewarding a) = "Rewarding " <> bshow a
  bshow (Certifying a) = "Certifying " <> bshow a

instance BuiltinShow ScriptContext where
  {-# INLINABLE bshow #-}
  bshow ScriptContext{..} = "ScriptContext " <> curlyBraces (
                            "scriptContextTxInfo=" <> bshow scriptContextTxInfo
                         <> ", scriptContextPurpose=" <> bshow scriptContextPurpose
                         )

parenthesis :: BuiltinString -> BuiltinString
parenthesis a = "( " <> a <> " )"

curlyBraces :: BuiltinString -> BuiltinString
curlyBraces a = "{ " <> a <> " }"

braces :: BuiltinString -> BuiltinString
braces a = "[ " <> a <> " ]"

-- Convert from an integer to its text representation. Example: 123 => "123"
{-# INLINEABLE integerToBS #-}
integerToBS :: Integer -> BuiltinByteString
integerToBS x
  -- 45 is ASCII code for '-'
  | x < 0 = consByteString 45 $ integerToBS (negate x)
  -- x is single-digit
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = integerToBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    -- 48 is ASCII code for '0'
    digitToBS d = consByteString (d + 48) emptyByteString

{-# INLINABLE intToHex #-}
intToHex :: BuiltinInteger -> BuiltinString
intToHex n | n==0 = "0"
           | n==1 = "1"
           | n==2 = "2"
           | n==3 = "3"
           | n==4 = "4"
           | n==5 = "5"
           | n==6 = "6"
           | n==7 = "7"
           | n==8 = "8"
           | n==9 = "9"
           | n==10 = "a"
           | n==11 = "b"
           | n==12 = "c"
           | n==13 = "d"
           | n==14 = "e"
           | n==15 = "f"