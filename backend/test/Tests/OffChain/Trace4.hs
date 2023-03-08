{-|
Module      : Tests.OffChain.Trace4
Description : Trace4 for unit testing the Escrow contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

In this trace, the Sender starts the contract locking the payment in the script
and then cancels it getting back it funds.

Trace execution description:
  1. The sender (Wallet 1) starts the escrow and deposits the payment, blocking
     it inside the script
  2. The sender cancels the escrow, recovering the locked payment
-}

module Tests.OffChain.Trace4 where

-- Non-IOG imports
import Control.Lens  ( (.~), (&) )
import Control.Monad ( void )
import Data.Default  ( Default (..) )
import Test.Tasty    ( TestTree )

-- IOG imports
import Ledger.Value          ( assetClass )
import Plutus.Trace.Emulator ( activateContractWallet, callEndpoint
                             , EmulatorTrace, runEmulatorTraceIO', waitNSlots
                             )
import Control.Monad.Freer.Extras.Log
import Plutus.Contract.Test  ( (.&&.), checkPredicateOptions
                             , defaultCheckOptions, emulatorConfig
                             , walletFundsChange
                             )
import PlutusTx               ( BuiltinData, fromBuiltinData )

-- Escrow imports
import Escrow        (EscrowDatum, mkStartParams, mkCancelParams, mkUpdateParams, mkResolveParams
                     , mkReceiverAddress, endpoints, escrowUtxo
                     , mkSenderAddress
                     )
import Tests.Utils   ( emConfig
                     , wallet1, wallet2, wallet3
                     , valueA, valueB, valueC
                     , wallet1Addr, wallet2Addr, wallet3Addr
                     , tokenACurrencySymbol, tokenAName
                     , tokenBCurrencySymbol, tokenBName
                     , tokenCCurrencySymbol, tokenCName
                     , getEscrowInfoList, mockReloadFlag
                     )
import Tests.BCExplorer

testMsg :: String
testMsg = "Starting and updating 2 escrow"

test :: TestTree
test = checkPredicateOptions
        (defaultCheckOptions & emulatorConfig .~ emConfig)
        testMsg
        (    walletFundsChange wallet1 (valueA (-75) <> valueC 10)
        .&&. walletFundsChange wallet2 (valueC (-23) <> valueA 20)
        .&&. walletFundsChange wallet3 (valueA 55 <> valueC 13)
        )
        trace

trace :: EmulatorTrace ()
trace = do
    let startParams1 = mkStartParams
                      (mkReceiverAddress wallet3Addr)
                      75
                      (assetClass tokenACurrencySymbol tokenAName)
                      10
                      (assetClass tokenBCurrencySymbol tokenBName)
    let startParams2 = mkStartParams
                      (mkReceiverAddress wallet3Addr)
                      23
                      (assetClass tokenCCurrencySymbol tokenCName)
                      89
                      (assetClass tokenBCurrencySymbol tokenBName)

    h1 <- activateContractWallet wallet1 $ endpoints wallet1Addr
    callEndpoint @"start" h1 startParams1
    void $ waitNSlots 10
    h2 <- activateContractWallet wallet2 $ endpoints wallet2Addr
    callEndpoint @"start" h2 startParams2
    void $ waitNSlots 10
    h3 <- activateContractWallet wallet3 $ endpoints wallet3Addr
    callEndpoint @"reload" h3 mockReloadFlag
    utxos <- getEscrowInfoList h3

    let updateParams1 = mkUpdateParams
                        (escrowUtxo $ utxos !! 0)
                        (mkSenderAddress wallet1Addr)
                        (mkReceiverAddress wallet3Addr)
                        10
                        (assetClass tokenCCurrencySymbol tokenCName)
        updateParams2 = mkUpdateParams
                        (escrowUtxo $ utxos !! 1)
                        (mkSenderAddress wallet2Addr)
                        (mkReceiverAddress wallet3Addr)
                        20
                        (assetClass tokenACurrencySymbol tokenAName)

    callEndpoint @"update" h1 updateParams1
    void $ waitNSlots 10
    callEndpoint @"update" h2 updateParams2
    void $ waitNSlots 10

    callEndpoint @"reload" h3 mockReloadFlag
    void $ waitNSlots 10
    utxos <- getEscrowInfoList h3

    let resolveParams1 = mkResolveParams $ escrowUtxo $ utxos !! 0
        resolveParams2 = mkResolveParams $ escrowUtxo $ utxos !! 1

    callEndpoint @"resolve" h3 resolveParams1
    void $ waitNSlots 10
    callEndpoint @"resolve" h3 resolveParams2
    void $ waitNSlots 10

    printBlockChainCFD [ FD (fromBuiltinData :: BuiltinData -> Maybe EscrowDatum) ]
    void $ waitNSlots 10

    logInfo @String ("WALLET 1: " ++ show wallet1Addr)
    logInfo @String ("WALLET 2: " ++ show wallet2Addr)
    void $ waitNSlots 10

-- | For running the trace from the repl
runTrace :: IO ()
runTrace = do
  putStrLn $ "\n" ++ testMsg ++ ".\n"
  runEmulatorTraceIO' def emConfig trace
