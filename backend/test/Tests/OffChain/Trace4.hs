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
                     , wallet1, wallet2
                     , wallet1Addr, wallet2Addr
                     , tokenACurrencySymbol, tokenAName
                     , tokenBCurrencySymbol, tokenBName
                     , tokenCCurrencySymbol, tokenCName
                     , getEscrowInfoList, mockReloadFlag
                     )
import Tests.BCExplorer

testMsg :: String
testMsg = "Update Test"

-- test :: TestTree
-- test = checkPredicateOptions
--        (defaultCheckOptions & emulatorConfig .~ emConfig)
--        trace
--        (walletFundsChange wallet1 )

trace :: EmulatorTrace ()
trace = do
    let startParams = mkStartParams
                      (mkReceiverAddress wallet2Addr)
                      75
                      (assetClass tokenACurrencySymbol tokenAName)
                      10
                      (assetClass tokenBCurrencySymbol tokenBName)

    h1 <- activateContractWallet wallet1 $ endpoints wallet1Addr
    callEndpoint @"start" h1 startParams
    void $ waitNSlots 10
    h2 <- activateContractWallet wallet2 $ endpoints wallet2Addr
    callEndpoint @"reload" h2 mockReloadFlag
    utxos <- getEscrowInfoList h2
    let updateParams = mkUpdateParams
                       (escrowUtxo $ head utxos)
                       (mkSenderAddress wallet1Addr)
                       (mkReceiverAddress wallet2Addr)
                       1000
                       (assetClass tokenCCurrencySymbol tokenCName)

    callEndpoint @"update" h1 updateParams
    void $ waitNSlots 10

    callEndpoint @"reload" h2 mockReloadFlag
    utxos <- getEscrowInfoList h2

    let resolveParams1 = mkResolveParams $ escrowUtxo $ head utxos

    callEndpoint @"resolve" h2 resolveParams1
    void $ waitNSlots 10

    printBlockChainCFD [ FD (fromBuiltinData :: BuiltinData -> Maybe EscrowDatum) ]
    void $ waitNSlots 10

runTrace :: IO ()
runTrace = do
  putStrLn $ "\n" ++ testMsg ++ ".\n"
  runEmulatorTraceIO' def emConfig trace
