{-# LANGUAGE NumericUnderscores #-}

{-|
Module      : Tests.Utils
Description : Util functions for the Tests.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

In this module we define a variety of functions and constants to be used on the
traces and other testing modules.
-}

module Tests.Utils where

-- Non-IOG imports
import Control.Monad   ( void )
import Data.Default    ( def )
import Data.Map as Map ( fromList, fromList)
import Data.Monoid     ( Last(..) )
import Data.Text       ( Text )

-- IOG imports
import Control.Monad.Freer.Extras ( logInfo )
import Plutus.Trace.Emulator      ( ContractHandle
                                  , EmulatorConfig (EmulatorConfig)
                                  , EmulatorTrace, observableState, waitNSlots
                                  )
import Plutus.Contract.Test       ( w1, w2, w3, w4 )
import Ledger                     ( Address, PaymentPubKey, TxOutRef )
import Ledger.Ada                 ( lovelaceValueOf )
import Ledger.Value               as Value
import Wallet.Emulator.Types      ( Wallet (..) )
import Wallet.Emulator.Wallet     ( mockWalletAddress
                                  , mockWalletPaymentPubKey
                                  )

-- Escrow imports
import Escrow.OffChain.Actions    ( EscrowSchema )
import Escrow.OffChain.Parameters ( ObservableState(..) )
import Escrow.Business            ( EscrowInfo )

wallets :: [(Wallet,Value)]
wallets = [ (senderWallet,   v <> paymentA 100)
          , (receiverWallet, v <> paymentB 100)
          , (w3,             v <> paymentA 100)
          , (w4,             v <> paymentA 100)
          ]
  where
    v :: Value
    v = lovelaceValueOf 100_000_000

tokenA, tokenB :: TokenName
tokenA = "A"
tokenB = "B"

tokenACurrencySymbol, tokenBCurrencySymbol :: CurrencySymbol
tokenACurrencySymbol =
    "246ea4f1fd944bc8b0957050a31ab0487016be233725c9f931b1aaaa"
tokenBCurrencySymbol =
    "0b1e203c7e13914e095bf462441205c1b377e978718fcb93fd44bbbb"

paymentA, paymentB :: Integer -> Value
paymentA = singleton tokenACurrencySymbol tokenA
paymentB = singleton tokenBCurrencySymbol tokenB

senderWallet, receiverWallet :: Wallet
senderWallet   = w1
receiverWallet = w2

senderAddr, receiverAddr :: Address
senderAddr   = mockWalletAddress senderWallet
receiverAddr = mockWalletAddress receiverWallet

senderPpk, receiverPpk :: PaymentPubKey
senderPpk   = mockWalletPaymentPubKey senderWallet
receiverPpk = mockWalletPaymentPubKey receiverWallet

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ fromList wallets) def

-- | Polls the Emulator until it finds an ObservableState
getObservableState
    :: ContractHandle (Last ObservableState) EscrowSchema Text
    -> EmulatorTrace ObservableState
getObservableState h = do
    void $ waitNSlots 1
    l <- observableState h
    case l of
        Last (Just observable) ->
            logInfo (show observable) >> return observable
        Last _ ->
            waitNSlots 1 >> getObservableState h