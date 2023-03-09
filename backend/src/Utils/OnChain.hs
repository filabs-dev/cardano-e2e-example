{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Utils.OnChain
Description : Utils functions for OnChain code.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.OnChain where

-- IOG imports
import Ledger     ( Address(..), Value, PubKeyHash
                  , getDatum, minAdaTxOut, toPubKeyHash
                  )
import Ledger.Ada ( toValue )
import PlutusTx   ( FromData(..) )
import PlutusTx.Prelude ( Maybe(..), Bool(..)
                        , (.), (>>=), (==), ($), (!!)
                        , mapMaybe, map, mconcat
                        , traceError, filter
                        )
import Plutus.V1.Ledger.Contexts ( ScriptContext(..), TxInfo(..), TxInInfo(..)
                                 , findDatum
                                 )
import Plutus.V1.Ledger.Tx       ( TxOut(..), txOutDatum )
import Plutus.V1.Ledger.Credential ( Credential(..))

-- | Get all the script inputs from the context
{-# INLINABLE getScriptInputs #-}
getScriptInputs :: ScriptContext -> [TxOut]
getScriptInputs ctx = filter
                            (isScript . txOutAddress)
                            (map txInInfoResolved
                                 (txInfoInputs $ scriptContextTxInfo ctx))
  where
    isScript (Address (ScriptCredential _) _) = True
    isScript _ = False

-- | Get the UTxOs of given address.
{-# INLINABLE outputsAt #-}
outputsAt :: Address -> TxInfo -> [TxOut]
outputsAt addr info = mapMaybe flt (txInfoOutputs info)
  where
    flt txOut@TxOut{..} | txOutAddress == addr = Just txOut
    flt _                                      = Nothing

-- | Get the values paid to a public key address by a pending transaction.
{-# INLINABLE valueOutputsAt #-}
valueOutputsAt :: Address -> TxInfo -> [Value]
valueOutputsAt addr = map txOutValue . outputsAt addr

-- | Get the total value paid to a public key address by a pending transaction.
{-# INLINABLE valuePaidTo #-}
valuePaidTo :: Address -> TxInfo -> Value
valuePaidTo addr ptx = mconcat (valueOutputsAt addr ptx)

-- | Gets the datum attached to a UTxO.
{-# INLINABLE getTxOutDatum #-}
getTxOutDatum :: FromData d => TxOut -> TxInfo -> Maybe d
getTxOutDatum o info = txOutDatum o
                       >>= (`findDatum` info)
                       >>= fromBuiltinData . getDatum

-- | The public key hash is part of the address.
{-# INLINABLE pubKeyHashInAddress #-}
pubKeyHashInAddress :: PubKeyHash -> Address -> Bool
pubKeyHashInAddress pkh addr = case toPubKeyHash addr of
                                   Just spkh -> spkh == pkh
                                   _ -> False

-- | Get the only element on the list, fail otherwise.
{-# INLINABLE getSingleton #-}
getSingleton :: [a] -> a
getSingleton [a] = a
getSingleton []  = traceError "Empty list"
getSingleton _   = traceError "More than one element"

-- | Get the only element on the list, fail otherwise.
{-# INLINABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = traceError "Got Nothing"

-- | Minimum amount of ADAs that every UTxO must have
{-# INLINABLE minAda #-}
minAda :: Value
minAda = toValue minAdaTxOut

-- | Verifies if a list is empty
{-# INLINABLE null #-}
null :: [a] -> Bool
null [] = True
null _  = False
