import { ContractEndpoints, CIP30WalletWrapper } from "cardano-pab-client";
import { StartParams, mkWalletAddressFromString } from "./parameters";
/**
 * The representation of the contract Utxo state
 */
export type UtxoInfo = {
  SenderAddr: String;
  SendAsset: String;
  SendAmount: Number;
  ReceiveAsset: String;
  ReceiveAmount: Number;
}

export type ObsState = UtxoInfo[]

export class UserEndpoints {
  contractState: ObsState = undefined
  endpoints: ContractEndpoints = undefined
  wallet: CIP30WalletWrapper = undefined

  constructor(
    contractState: ObsState,
  ) {
    this.contractState = contractState
  }

  public async connect(walletName): Promise<UserEndpoints> {
    const {
      getWalletInitialAPI,
      CIP30WalletWrapper,
      PABApi,
      ContractEndpoints
    } = await import("cardano-pab-client");

    let walletInitialAPI = getWalletInitialAPI(window, walletName);

    // this will ask the user to give to this dApp access to their wallet methods
    const walletInjectedFromBrowser = await walletInitialAPI.enable();

    // then we can initialize the CIP30WalletWrapper class of the library
    this.wallet = await CIP30WalletWrapper.init(walletInjectedFromBrowser);
    const addrs = await this.wallet.getUsedAddresses()
    const walletAddr = mkWalletAddressFromString(addrs[0])
    console.log(`Connected Address:`)
    console.log(walletAddr)

    // Try to get unbalanced transaction from PAB
    const walletId = await this.wallet.getWalletId();
    const pabApi = new PABApi(process.env.REACT_APP_PAB_URL);

    this.endpoints = await ContractEndpoints.connect(
      walletId,
      { endpointTag: "Connect", params: walletAddr },
      pabApi,
    );

    return this
  }

  public async start(sp: StartParams) {
    const {
      TxBudgetAPI,
      Balancer,
      getProtocolParamsFromBlockfrost,
      succeeded
    } = await import("cardano-pab-client");
    console.log(`Start Params:`)
    console.log(sp)
    // Initialize Balancer
    const protocolParams = await getProtocolParamsFromBlockfrost(
      process.env.REACT_APP_BLOCKFROST_URL,
      process.env.REACT_APP_BLOCKFROST_API_KEY,
    );
    const balancer = await Balancer.init(protocolParams);

    // Try to get unbalanced transaction from PAB
    const pabResponse = await this.endpoints.doOperation(
      { endpointTag: "start", params: sp }
    );

    if (!succeeded(pabResponse)) {
      alert(
        `Didn't got the unbalanced transaction from the PAB. Error: ${pabResponse.error}`
      );
    } else {
      // the pab yielded the unbalanced transaction. balance, sign and submit it.
      const etx = pabResponse.value;
      console.log(`Unbalanced tx:`);
      console.log(etx)
      const walletInfo = await this.wallet.getWalletInfo();
      const txBudgetApi = new TxBudgetAPI({
        baseUrl: process.env.REACT_APP_ESTIMATOR_URL,
        timeout: 10000,
      });

      const fullyBalancedTx = await balancer.fullBalanceTx(
        etx,
        walletInfo,
        // configuration for the balanceTx and rebalanceTx methods which are interally
        // used by this method
        { feeUpperBound: 1000000, mergeSignerOutputs: false },
        // a high-order function that exposes the balanced tx and the inputs info so to
        // calculate the executions units, which are then set in the transaction and
        // goes to the rebalancing step
        async (balancedTx, inputsInfo) => {
          const txBudgetResponse = await txBudgetApi.estimate(balancedTx, inputsInfo);
          if (succeeded(txBudgetResponse)) {
            const units = txBudgetResponse.value;
            return units;
          } else {
            console.log("BALANCER FAILED")
            console.log(txBudgetResponse.error)
            return []
          }
        }
      );
      // print to the console the fully balanced tx cbor for debugging purposes
      console.log(`Balanced tx: ${fullyBalancedTx}`);
      // now that the transaction is balanced, sign and submit it with the wallet
      const response = await this.wallet.signAndSubmit(fullyBalancedTx);
      if (succeeded(response)) {
        const txHash = response.value;
        alert(`Start suceeded. Tx hash: ${txHash}`);
      } else {
        alert(`Start failed when trying to submit it. Error: ${response.error}`);
      }
    }
  }
}