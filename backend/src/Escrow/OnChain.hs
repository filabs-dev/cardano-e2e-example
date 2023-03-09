{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}
{-# options_ghc -fexpose-all-unfoldings    #-}
{-|
Module      : Escrow.OnChain
Description : OnChain validator for the Escrow dApp.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define here the on-chain code of the validator and the minting policy.
-}

module Escrow.OnChain
    ( -- * Validators
      mkEscrowValidator
    , mkControlTokenMintingPolicy
    )
where

-- IOG imports
import Plutus.V1.Ledger.Api   ( ScriptContext(..), TxInfo(..), TxOut(..)
                              , PubKeyHash, TokenName
                              )
import Plutus.V1.Ledger.Value ( CurrencySymbol, Value, AssetClass
                              , assetClass, assetClassValueOf, assetClassValue
                              , flattenValue, leq
                              )
import Plutus.V1.Ledger.Contexts ( getContinuingOutputs )
import PlutusTx.Prelude       ( Integer, Bool(..)
                              , ($), (&&), (||), (==), (>), (<>)
                              , length, traceIfFalse
                              )


import Plutus.Contract.Test   ()
-- Escrow imports
import Escrow.Business     ( ReceiverAddress(..), EscrowInfo(..), SenderAddress
                           , signerIsSender, signerIsReceiver, eInfoSenderAddr
                           , valueToSender
                           )
import Escrow.Types        ( EscrowDatum(..), EscrowRedeemer(..)
                           , ScriptAddress
                           )
import Utils.OnChain       ( fromJust, getSingleton, getScriptInputs
                           , valuePaidTo, outputsAt, getTxOutDatum, null
                           )
import Utils.WalletAddress ( toAddress )

{- | Escrow Validator

Validates the transactions that involve spending the script UTxO with Cancel or
Resolve redeemers.

Cancel:
 - The address that is trying to cancel the escrow is the same as the Sender’s
   address.
 - There is only one script input UTxO
 - The control token is burned after the transaction.

Resolve:
 - The address that is trying to resolve is the same as the Receiver’s address.
 - The Sender’s address receives the tokens specified on the datum.
 - The Receiver's address receives the tokens held on the value of the script UTxO
   (without the control token)
 - There is only one script input UTxO
 - The control token is burned after the transaction.
-}
{-# INLINABLE mkEscrowValidator #-}
mkEscrowValidator :: ReceiverAddress
                  -> EscrowDatum
                  -> EscrowRedeemer
                  -> ScriptContext
                  -> Bool
mkEscrowValidator raddr EscrowDatum{..} r ctx =
    case r of
        CancelEscrow  -> cancelValidator eInfo signer && controlTokenBurned
        ResolveEscrow -> resolveValidator info eInfo raddr signer scriptValue
                          && controlTokenBurned
        UpdateEscrow  -> updateValidator ctx (sender eInfo) eAssetClass signer
    &&
    traceIfFalse "more than one script input utxo"
                 (length sUtxos == 1)

  where
    controlTokenBurned :: Bool
    controlTokenBurned =
      traceIfFalse "controlToken was not burned"
      (eAssetClass == assetClass mintedCS mintedTN && mintedA == -1)

    mintedCS :: CurrencySymbol
    mintedTN :: TokenName
    (mintedCS, mintedTN, mintedA) = getSingleton $
                                    flattenValue $ txInfoMint info

    signer :: PubKeyHash
    signer = getSingleton $ txInfoSignatories info

    info :: TxInfo
    info = scriptContextTxInfo ctx

    sUtxos :: [TxOut]
    sUtxos = getScriptInputs ctx

    scriptValue :: Value
    scriptValue = txOutValue (getSingleton sUtxos)
                <> assetClassValue eAssetClass (-1)


{- | Checks:
 - The address that is trying to cancel the escrow is the same as the Sender’s
   address.
-}
{-# INLINABLE cancelValidator #-}
cancelValidator :: EscrowInfo -> PubKeyHash -> Bool
cancelValidator EscrowInfo{..} signer =
    traceIfFalse
    "cancelValidator: Wrong sender signature" $ signerIsSender signer sender

{- | Checks:
 - The address that is trying to resolve is the same as the Receiver’s address.
 - The Sender’s address receives the tokens specified on the datum.
 - The Receiver's address receives the tokens held on the value of the script UTxO
   (without the control token)
-}
{-# INLINABLE resolveValidator #-}
resolveValidator :: TxInfo
                 -> EscrowInfo
                 -> ReceiverAddress
                 -> PubKeyHash
                 -> Value
                 -> Bool
resolveValidator info ei raddr@ReceiverAddress{..} signer scriptValue =
    traceIfFalse "resolveValidator: Wrong receiver signature"
                 (signerIsReceiver signer raddr)
    &&
    traceIfFalse "resolveValidator: Wrong sender's payment"
                 (valueToSender ei `leq` senderV)
    &&
    traceIfFalse "resolveValidator: Wrong receiver's payment"
                 (scriptValue `leq` receiverV)
  where
    senderV :: Value
    senderV = valuePaidTo (eInfoSenderAddr ei) info

    receiverV :: Value
    receiverV = valuePaidTo (toAddress rAddr) info

{- | Checks:
 - The value of the input script UTxO is the same as the output script UTxO.
 - The address that is trying to update is the same as the Sender’s address.
 - The control token is in the script UTxO, and it isn't burned nor minted.
 - The new receive amount is bigger than zero.
 - The control token asset class is the same in the output script datum.
-}
{-# INLINABLE updateValidator #-}
updateValidator :: ScriptContext -> SenderAddress -> AssetClass -> PubKeyHash -> Bool
updateValidator ctx sender eAssetClassInput signer =
    traceIfFalse "updateValidator: Wrong sender signature"
                 (signerIsSender signer sender)
    &&
    traceIfFalse "updateValidator: Control token burnt or minted"
      controlDontForge
    &&
    traceIfFalse "updateValidator: Incorrect script output value"
      (scriptInValue == scriptOutValue)
    &&
    traceIfFalse "updateValidator: Incorrect new receive value"
      correctAmount
    &&
    traceIfFalse "updateValidator: Incorrect control token asset class"
      correctControlAssetClass
  where
    controlDontForge :: Bool
    controlDontForge = null $ flattenValue $ txInfoMint info

    info :: TxInfo
    info = scriptContextTxInfo ctx

    scriptInValue :: Value
    scriptInValue = txOutValue (getSingleton sUtxos)

    scriptOutValue :: Value
    scriptOutValue = txOutValue escrowUtxo

    escrowUtxo :: TxOut
    escrowUtxo = getSingleton sOutUTxOs

    sUtxos :: [TxOut]
    sUtxos = getScriptInputs ctx

    sOutUTxOs :: [TxOut]
    sOutUTxOs = getContinuingOutputs ctx

    escrowDatum :: EscrowDatum
    escrowDatum = fromJust $ getTxOutDatum escrowUtxo info

    correctAmount :: Bool
    correctAmount = rAmount (eInfo escrowDatum) > 0

    correctControlAssetClass :: Bool
    correctControlAssetClass =
      eAssetClass escrowDatum == eAssetClassInput


{- | Escrow Control Token minting policy

The token minting policy is parametrized by the script address and has the
following checks:

On minting:
- Only one token with the correct token name is minted
- The token is paid to the script address
- The sender’s address is signing the transaction
- The token being minted is the correct control token
- The amount of tokens that the receiver wants to offer is more than 0

On Burning:
 - One token is being burned.
-}
{-# INLINABLE mkControlTokenMintingPolicy #-}
mkControlTokenMintingPolicy :: ScriptAddress -> () -> ScriptContext -> Bool
mkControlTokenMintingPolicy addr _ ctx =
    traceIfFalse "Burning less or more than one token" (mintedA == -1)
    ||
    (   traceIfFalse "Minting more than one token"
                     (mintedA == 1)
     && traceIfFalse "The control token was not paid to the script address"
                     controlTokenPaid
     && traceIfFalse "Wrong information in Datum"
                     correctDatum
    )
  where
    mintedCS :: CurrencySymbol
    mintedTN :: TokenName
    mintedA :: Integer
    (mintedCS, mintedTN, mintedA) = getSingleton $
                                    flattenValue $ txInfoMint info

    controlTokenPaid :: Bool
    controlTokenPaid =
        assetClassValueOf (txOutValue escrowUtxo) (assetClass mintedCS mintedTN)
        ==
        mintedA

    correctDatum :: Bool
    correctDatum =
        traceIfFalse
          "The signer is not the sender on the escrow"
          correctSigner
     && traceIfFalse
          "The asset minted does not match with the control token"
          correctControlAssetClass
     && traceIfFalse
          "The receive amount of tokens to exchange is not positive"
          correctAmount

    correctSigner :: Bool
    correctSigner = signerIsSender signer (sender $ eInfo escrowDatum)

    correctControlAssetClass :: Bool
    correctControlAssetClass =
      eAssetClass escrowDatum == assetClass mintedCS mintedTN

    correctAmount :: Bool
    correctAmount = rAmount (eInfo escrowDatum) > 0

    escrowUtxo :: TxOut
    escrowUtxo = getSingleton $ outputsAt addr info

    escrowDatum :: EscrowDatum
    escrowDatum = fromJust $ getTxOutDatum escrowUtxo info

    signer :: PubKeyHash
    signer = getSingleton $ txInfoSignatories info

    info :: TxInfo
    info = scriptContextTxInfo ctx
