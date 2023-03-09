import type { TxOutRef, Plutus, WalletAddress } from "cardano-pab-client";

export type StartParams = {
  receiverAddress: WalletAddress
  sendAssetClass: Plutus.AssetClass
  sendAmount: number
  receiveAssetClass: Plutus.AssetClass
  receiveAmount: number
}

export async function mkStartParams(
  rAdd: string,
  sCurrency: string,
  sTokenN: string,
  sAm: number,
  rCurrency: string,
  rTokenN: string,
  rAm: number,
): Promise<StartParams> {
  const { Address, AssetClass, succeeded } = await import("cardano-pab-client");
  const result = await Address.fromBech32(rAdd);
  if (succeeded(result)) {
    const receiverAddress = result.value.toWalletAddress();
    const sendAssetClass = new AssetClass(sCurrency, sTokenN).toPlutusAssetClass();
    const receiveAssetClass = new AssetClass(rCurrency, rTokenN).toPlutusAssetClass();
    return {
      receiverAddress,
      sendAssetClass,
      sendAmount: sAm,
      receiveAssetClass,
      receiveAmount: rAm,
    }
  } else {
    throw new Error(result.error);
  }
}

export type UpdateParams = {
  upTxOutRef: Plutus.TxOutRef
  newSenderAddress: WalletAddress,
  upReceiverAddress: WalletAddress
  newReceiveAmount: number,
  newReceiveAssetClass: Plutus.AssetClass
}

export async function mkUpdateParams(
  ref: string,
  sAdd: string,
  rAdd: string,
  rAm: number,
  rCurrency: string,
  rTokenN: string,
): Promise<UpdateParams> {
  const { Address, TxOutRef, AssetClass, succeeded } = await import("cardano-pab-client");
  const [txId, idx]: string[] = ref.split("#");
  const result = await Address.fromBech32(rAdd);
  if (succeeded(result)) {
    const upReceiverAddress = result.value.toWalletAddress();
    const upTxOutRef = new TxOutRef(txId, Number(idx)).toPlutusTxOutRef();
    const newReceiveAssetClass = new AssetClass(rCurrency, rTokenN).toPlutusAssetClass();
    const senderResult = await Address.fromBech32(sAdd);
    if (succeeded(senderResult)) {
      const newSenderAddress = senderResult.value.toWalletAddress();
      return {
        upTxOutRef,
        newSenderAddress,
        upReceiverAddress,
        newReceiveAmount: rAm,
        newReceiveAssetClass,
      }
    } else {
      throw new Error(senderResult.error);
    }
  } else {
    throw new Error(result.error);
  }
}


export type CancelParams = {
  cpReceiverAddress: WalletAddress
  cpTxOutRef: Plutus.TxOutRef
}

export async function mkCancelParams(rAdd: string, ref: string): Promise<CancelParams> {
  const { Address, TxOutRef, succeeded } = await import("cardano-pab-client");
  const [txId, idx]: string[] = ref.split("#");
  const result = await Address.fromBech32(rAdd);
  if (succeeded(result)) {
    const cpReceiverAddress = result.value.toWalletAddress();
    const cpTxOutRef = new TxOutRef(txId, Number(idx)).toPlutusTxOutRef();
    return { cpTxOutRef, cpReceiverAddress };
  } else {
    throw new Error(result.error);
  }
}

export type ResolveParams = {
  rpTxOutRef: Plutus.TxOutRef
}

export function mkResolveParams(ref: TxOutRef): ResolveParams {
  return { rpTxOutRef: ref.toPlutusTxOutRef() };
}
