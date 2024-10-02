{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Generic.PABHelpers where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Control.Concurrent                  as ControlConcurrent (threadDelay)
import qualified Control.Monad                       as ControlMonad
import qualified Control.Monad                       as Monad (void)
import qualified Control.Monad.Freer                 as MonadFreer (interpret)
import qualified Control.Monad.Freer.Internal        as MonadFreerInternal (Eff)
import qualified Control.Monad.IO.Class              as MonadIOClass (MonadIO (..))
import qualified Data.Default                        as DataDefault (def)
import qualified Data.List                           as DataList
import qualified Data.Map                            as DataMap
import qualified Data.Maybe                          as DataMaybe
import qualified Ledger
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Address                      as LedgerAddress (Address)
import qualified Ledger.Blockchain                   as LedgerBlockchain
import qualified Ledger.CardanoWallet                as LedgerCardanoWallet
import qualified Ledger.Value                        as LedgerValue
import qualified Plutus.ChainIndex                   as ChainIndex
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.Contract.CardanoAPI          as PlutusContractCardanoAPI
import qualified Plutus.PAB.Core                     as PABCore (PABEffects)
import qualified Plutus.PAB.Effects.Contract.Builtin as PABEffectsContractBuiltin (Builtin, BuiltinHandler (contractHandler), HasDefinitions, handleBuiltin)
import qualified Plutus.PAB.Simulator                as PABSimulator
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx.Builtins.Internal          as TxBuiltinsInternal hiding (consByteString, head)
import qualified PlutusTx.Eq                         as PlutusTxEq
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Prettyprinter.Internal              as PrettyprinterInternal
import qualified System.IO                           as SystemIO
import qualified Text.Read                           as TextRead (readMaybe)
import qualified Wallet.Emulator.Wallet              as WalletEmulator

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.CLIHelpers                  as CLIHelpers
import qualified Generic.OffChainHelpers             as OffChainHelpers
import qualified Generic.OnChainHelpers              as OnChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

type PABEffects a = PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a))

--------------------------------------------------------------------------------2

handlers :: (PrettyprinterInternal.Pretty a, PABEffectsContractBuiltin.HasDefinitions a) => PABSimulator.SimulatorEffectHandlers (PABEffectsContractBuiltin.Builtin a)
handlers = PABSimulator.mkSimulatorHandlers DataDefault.def P.$ MonadFreer.interpret (PABEffectsContractBuiltin.contractHandler PABEffectsContractBuiltin.handleBuiltin)

--------------------------------------------------------------------------------2

getWallet :: Integer -> WalletEmulator.Wallet
getWallet = WalletEmulator.knownWallet

--------------------------------------------------------------------------------2

walletPaymentPubKeyHash :: Integer -> Ledger.PaymentPubKeyHash
walletPaymentPubKeyHash walletNumber = LedgerCardanoWallet.paymentPubKeyHash (LedgerCardanoWallet.fromWalletNumber $ LedgerCardanoWallet.WalletNumber walletNumber)

walletAddress :: Integer -> Ledger.CardanoAddress
walletAddress walletNumber = LedgerCardanoWallet.mockWalletAddress (LedgerCardanoWallet.fromWalletNumber $ LedgerCardanoWallet.WalletNumber walletNumber)

walletPaymentPubKeyHashAddress :: Integer -> LedgerAddress.Address
walletPaymentPubKeyHashAddress walletNumber = Ledger.pubKeyHashAddress (walletPaymentPubKeyHash walletNumber) Nothing

--------------------------------------------------------------------------------2

getDatumInPABSimulator :: forall datumType. (LedgerApiV2.FromData datumType) => LedgerBlockchain.Blockchain -> (Ledger.TxOutRef, Ledger.TxOut) -> Maybe datumType
getDatumInPABSimulator blockchain (uTxORef', uTxOut') =
    let txOutTx =
            Ledger.TxOutTx
                { Ledger.txOutTxTx = Ledger._emulatorTx $ Ledger.unOnChain $ OnChainHelpers.fromJust $ Ledger.transaction blockchain (Ledger.txOutRefId uTxORef'),
                  Ledger.txOutTxOut = uTxOut'
                }
    in  case Ledger.txOutTxDatum txOutTx of
            Nothing -> Nothing
            Just d  -> LedgerApiV2.fromBuiltinData @datumType $ LedgerApiV2.getDatum d

--------------------------------------------------------------------------------2

getUTxOsListInPABSimulator :: Ledger.Blockchain -> LedgerAddress.Address -> [(Ledger.TxOutRef, Ledger.TxOut)]
getUTxOsListInPABSimulator blockchain addr =
    let !unspentOutputList = Ledger.unspentOutputs blockchain
        !uTxOs = [(txOutRef, txOut) | (txOutRef, txOut) <- DataMap.toList unspentOutputList, OffChainHelpers.cardanoAddressToAddress (Ledger.txOutAddress txOut) == addr]
    in  uTxOs

--------------------------------------------------------------------------------2

isEqWallet :: WalletEmulator.Wallet -> WalletEmulator.Wallet -> Bool
isEqWallet w w' =
    TxBuiltinsInternal.BuiltinString (WalletEmulator.toBase16 $ WalletEmulator.getWalletId w) PlutusTxEq.== TxBuiltinsInternal.BuiltinString (WalletEmulator.toBase16 $ WalletEmulator.getWalletId w')

--------------------------------------------------------------------------------2

fromWallet :: Integer -> WalletEmulator.Entity -> Bool
fromWallet numWallet entity =
    case entity of
        WalletEmulator.WalletEntity wallet -> isEqWallet wallet (getWallet numWallet)
        _                                  -> False

--------------------------------------------------------------------------------2

fromScript :: LedgerApiV2.ValidatorHash -> WalletEmulator.Entity -> Bool
fromScript hash entity =
    case entity of
        WalletEmulator.ScriptEntity scriptHash ->
            hash == scriptHash
        _ -> False

--------------------------------------------------------------------------------2

walletFromEntity :: WalletEmulator.Entity -> Maybe WalletEmulator.Wallet
walletFromEntity entity =
    case entity of
        WalletEmulator.WalletEntity wallet -> Just wallet
        _                                  -> Nothing

--------------------------------------------------------------------------------2

balances :: (Maybe Integer, Integer) -> [(P.String, LedgerApiV2.ValidatorHash)] -> MonadFreerInternal.Eff (PABEffects a) ()
balances (_, walletCount) validatorNamesAndHashes  = do
    !balances' <- PABSimulator.currentBalances
    -- MonadIOClass.liftIO $ P.putStrLn ("Balances:" ++ P.show balances')
    let -- (entity, value) <- DataMap.toList balances'
        !balanceList = DataMap.toList balances'
        ----------------
        formatWallets =
            concat
                [ let fromWalletEntity walletNro' (entity, _) = fromWallet walletNro' entity
                      entiyValue' = find (fromWalletEntity walletNro) balanceList
                      sep = case walletNro of
                        1 -> []
                        _ -> ["--------------------------------"]
                  in  case entiyValue' of
                        Nothing -> []
                        Just (_, value) ->
                            sep
                                ++ [ "#: " ++ P.show walletNro,
                                     "Pk: " ++ P.show (walletPaymentPubKeyHash walletNro),
                                     "Values: "
                                   ]
                                ++ [P.show v | v <- OnChainHelpers.flattenValue value]
                  | walletNro <- [1 .. walletCount]
                ]
        ----------------
        formatScripts =
            concat
                [ let entiyValue' = find (\(ent, _) -> fromScript hash ent) balanceList
                      sep = ["--------------------------------"]
                  in  --   sep = case scriptNro of
                      --     0 -> []
                      --     _ -> ["--------------------------------"]
                      case entiyValue' of
                        Nothing -> []
                        Just (_, value) ->
                            sep
                                ++ [ "#: " ++ P.show scriptNro,
                                     "Script: " ++ P.show name,
                                     "Hash: " ++ P.show hash,
                                     "Values: "
                                   ]
                                ++ [P.show v | v <- OnChainHelpers.flattenValue value]
                  | (scriptNro, (name, hash)) <- OnChainHelpers.enumerate validatorNamesAndHashes
                ]
    ----------------
    mapM_ (MonadIOClass.liftIO . P.putStrLn) formatWallets
    mapM_ (MonadIOClass.liftIO . P.putStrLn) formatScripts
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"

--------------------------------------------------------------------------------2

showUTxOsAtAddress :: forall datumType a. (P.Show datumType, LedgerApiV2.FromData datumType) => LedgerApiV2.Address -> MonadFreerInternal.Eff (PABEffects a) ()
showUTxOsAtAddress address = do
    blockchain <- PABSimulator.blockchain
    let uTxOuts = getUTxOsListInPABSimulator blockchain address
        ----------------
        formatValues uTxORef = [P.show val | val <- LedgerValue.flattenValue $ OnChainHelpers.fromJust $ LedgerBlockchain.value blockchain uTxORef]
        ----------------
        formatUTxOValues :: [(Ledger.TxOutRef, Ledger.TxOut)] -> [P.String]
        formatUTxOValues utxos =
            concat
                [ "-----"
                    : P.show (1 P.+ OnChainHelpers.fromJust (DataList.elemIndex (uTxORef, uTxOut) utxos))
                    : ("At: " ++ P.show uTxORef)
                    : ("Datum: " ++ P.show (getDatumInPABSimulator @datumType blockchain (uTxORef, uTxOut)))
                    -- TODO: revisar contra TxInsReferenceNone, y mostrar si hay o no. Si no pega todo el codigo del script, no es lo que quiero.: ("RefScript: " ++ P.show (Ledger.txOutReferenceScript uTxOut))
                     : ("ReferenceScript: " ++
                            let s = Ledger.txOutReferenceScript uTxOut
                                s2 = PlutusContractCardanoAPI.fromCardanoTxOutRefScript s
                                s3 = ChainIndex.fromReferenceScript s2
                            in
                                P.show (P.fmap Ledger.scriptHash s3)
                                -- remplace: P.show (P.maybe Nothing (Just . Ledger.scriptHash) s3)
                     )
                    : "Values: "
                    : formatValues uTxORef
                  | (uTxORef, uTxOut) <- utxos
                ]
    ----------------
    MonadIOClass.liftIO $ P.putStrLn $ "UTxOs at: " ++ P.show address
    mapM_ (MonadIOClass.liftIO . P.putStrLn) $ formatUTxOValues uTxOuts
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"

--------------------------------------------------------------------------------2

selectWallet :: P.String ->  Integer -> Integer -> Bool -> MonadFreerInternal.Eff (PABEffects a) (Integer, Integer)
selectWallet fieldName defWallet walletCount swAdd = do
    MonadIOClass.liftIO $ P.putStrLn $ "Enter " ++ fieldName ++ " (default=" ++ P.show defWallet ++ "):"
    let formatWallets =
            concat
                [ [ "--------------------------------",
                    "#: " ++ P.show walletNro,
                    "Pk: " ++ P.show (walletPaymentPubKeyHash walletNro),
                    "walletId: " ++ P.show (WalletEmulator.getWalletId $ getWallet walletNro),
                    "prettyWalletName : " ++ P.show (WalletEmulator.prettyWalletName $ getWallet walletNro)
                  ]
                  | walletNro <- [1 .. walletCount]
                ]
    ----------------
    mapM_ (MonadIOClass.liftIO . P.putStrLn) formatWallets
    ----------------
    ControlMonad.when swAdd P.$ do
        MonadIOClass.liftIO$ P.putStrLn "--------------------------------"
        MonadIOClass.liftIO $ P.putStrLn "0 - Add Wallet (up to 10)"
    ----------------
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Enter option:"
    numberSrt <- MonadIOClass.liftIO P.getLine
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    if P.null numberSrt
        then
            return (defWallet, walletCount)
        else
            case TextRead.readMaybe numberSrt :: Maybe Integer of
                Just x ->
                    if x == 0 && swAdd
                        then do
                            -- TODO: me gustaria poder agregar wallets en PAB Simulator
                            -- PABSimulator.addWalletWith (Just $ LedgerAda.lovelaceOf 100)
                            selectWallet fieldName defWallet (walletCount + 1) swAdd
                        else
                            if x >= 1 && x <= walletCount
                                then return (x, walletCount)
                                else do
                                    MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                                    selectWallet fieldName defWallet walletCount swAdd
                _ -> do
                    MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                    selectWallet fieldName defWallet walletCount swAdd

--------------------------------------------------------------------------------2

selectWallets :: P.String ->  [Integer] -> Integer -> [Integer] -> MonadFreerInternal.Eff (PABEffects a) [Integer]
selectWallets fieldName defWallet walletCount acc = do
    MonadIOClass.liftIO $ P.putStrLn $ "Enter " ++ fieldName ++ " (default=" ++ P.show defWallet ++ "):"
    let formatWallets =
            concat
                [ [ "--------------------------------",
                    "#: " ++ P.show walletNro,
                    "Pk: " ++ P.show (walletPaymentPubKeyHash walletNro)
                  ]
                  | walletNro <- [1 .. walletCount]
                ]
        ----------------
        formatSelected :: [Integer] -> [P.String]
        formatSelected opciones' =
            concat [["--------------------------------", P.show walletNro] | walletNro <- opciones']
    ----------------
    mapM_ (MonadIOClass.liftIO . P.putStrLn) formatWallets
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Selected:"
    mapM_ (MonadIOClass.liftIO . P.putStrLn) (formatSelected acc)
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Option (0 to finish)"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Enter option:"
    numberSrt <- MonadIOClass.liftIO P.getLine
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    if P.null numberSrt
        then
            if P.null acc
                then return defWallet
                else return $ defWallet ++  OnChainHelpers.filterListWithList acc defWallet
        else
            case TextRead.readMaybe numberSrt :: Maybe Integer of
                Just 0 -> do
                    return acc
                Just x ->
                    if x >= 1 && x <= walletCount
                        then
                            let new = x
                                news = new : filter (new /=) acc
                            in  selectWallets fieldName defWallet walletCount news
                        else do
                            MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                            selectWallets fieldName defWallet walletCount acc
                _ -> do
                    MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                    selectWallets fieldName defWallet walletCount acc

--------------------------------------------------------------------------------2

selectUTxO :: forall datumType a. (P.Show datumType, LedgerApiV2.FromData datumType) => [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABEffects a) (Maybe (Integer, Ledger.TxOutRef, Ledger.TxOut))
selectUTxO uTxOuts blockchain = do
    case uTxOuts of
        [] -> do
            MonadIOClass.liftIO $ P.putStrLn "There is no UTxO to select"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            return Nothing
        uTxOutsWithAC -> do
            let formatValues uTxORef = [P.show val | val <- LedgerValue.flattenValue $ OnChainHelpers.fromJust $ LedgerBlockchain.value blockchain uTxORef]
                ----------------
                formatUTxOValues :: [(Ledger.TxOutRef, Ledger.TxOut)] -> [P.String]
                formatUTxOValues utxos =
                    concat
                        [ "-----"
                            : P.show (1 P.+ OnChainHelpers.fromJust (DataList.elemIndex (uTxORef, uTxOut) utxos))
                            : ("At: " ++ P.show uTxORef)
                            : "Datum: "
                            : P.show (getDatumInPABSimulator @datumType blockchain (uTxORef, uTxOut))
                            : "Values: "
                            : formatValues uTxORef
                          | (uTxORef, uTxOut) <- utxos
                        ]
            ----------------
            MonadIOClass.liftIO $ P.putStrLn "UTxOs:"
            mapM_ (MonadIOClass.liftIO . P.putStrLn) (formatUTxOValues uTxOutsWithAC)
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            MonadIOClass.liftIO $ P.putStrLn "Option (0 to cancel)"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            MonadIOClass.liftIO $ P.putStrLn "Enter option:"
            !opcionUTxO <- MonadIOClass.liftIO P.getLine
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            case TextRead.readMaybe opcionUTxO :: Maybe Integer of
                Just 0 -> do
                    return Nothing
                Just x -> do
                    if x >= 1 && x <= length uTxOutsWithAC
                        then do
                            let !new = (x, fst $ uTxOutsWithAC !! (x - 1), snd $ uTxOutsWithAC !! (x - 1))
                            return (Just new)
                        else do
                            MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                            selectUTxO @datumType @a uTxOutsWithAC blockchain
                _ -> do
                    MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                    selectUTxO @datumType @a uTxOutsWithAC blockchain

--------------------------------------------------------------------------------2

selectUTxOWithDatumAndAC :: forall datumType a. (P.Show datumType, LedgerApiV2.FromData datumType) => LedgerValue.AssetClass -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABEffects a) (Maybe (Integer, Ledger.TxOutRef, Ledger.TxOut))
selectUTxOWithDatumAndAC unit_AC uTxOuts blockchain = do
    let !uTxOutsWithAC =
            [ (txOutRef, txOut) | (txOutRef, txOut) <- uTxOuts, LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) unit_AC > 0
                                                                    && DataMaybe.isJust (getDatumInPABSimulator @datumType blockchain (txOutRef, txOut))
            ]
    selectUTxO @datumType @a uTxOutsWithAC blockchain

--------------------------------------------------------------------------------2

selectUTxOWithDatumAndCS :: forall datumType a. (P.Show datumType, LedgerApiV2.FromData datumType) => LedgerApiV2.CurrencySymbol -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABEffects a) (Maybe (Integer, Ledger.TxOutRef, Ledger.TxOut))
selectUTxOWithDatumAndCS unit_CS uTxOuts blockchain = do
    let !uTxOutsWithCS =
            [ (txOutRef, txOut) | (txOutRef, txOut) <- uTxOuts, OnChainHelpers.getAmtOfCurrencySymbol (Ledger.txOutValue txOut) unit_CS > 0
                                                                    && DataMaybe.isJust (getDatumInPABSimulator @datumType blockchain (txOutRef, txOut))
            ]
    selectUTxO @datumType @a uTxOutsWithCS blockchain

--------------------------------------------------------------------------------2

selectUTxOWithDatumAndNoCS :: forall datumType a. (P.Show datumType, LedgerApiV2.FromData datumType) => LedgerApiV2.CurrencySymbol -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABEffects a) (Maybe (Integer, Ledger.TxOutRef, Ledger.TxOut))
selectUTxOWithDatumAndNoCS unit_CS uTxOuts blockchain = do
    let !uTxOutsWithCS =
            [ (txOutRef, txOut) | (txOutRef, txOut) <- uTxOuts, OnChainHelpers.getAmtOfCurrencySymbol (Ledger.txOutValue txOut) unit_CS == 0
                                                                     && DataMaybe.isJust (getDatumInPABSimulator @datumType blockchain (txOutRef, txOut))
            ]
    selectUTxO @datumType @a uTxOutsWithCS blockchain

--------------------------------------------------------------------------------2

selectUTxOs :: forall datumType a. (P.Show datumType, LedgerApiV2.FromData datumType) => [(Integer, Ledger.TxOutRef, Ledger.TxOut)] -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABEffects a) [(Integer, Ledger.TxOutRef, Ledger.TxOut)]
selectUTxOs opciones uTxOuts blockchain = do
    let formatValues uTxORef = [P.show val | val <- LedgerValue.flattenValue $ OnChainHelpers.fromJust $ LedgerBlockchain.value blockchain uTxORef]
        ----------------
        formatUTxOValues :: [(Ledger.TxOutRef, Ledger.TxOut)] -> [P.String]
        formatUTxOValues utxos =
            concat
                [ "-----"
                    : P.show (1 P.+ OnChainHelpers.fromJust (DataList.elemIndex (uTxORef, uTxOut) utxos))
                    : ("At: " ++ P.show uTxORef)
                    : "Datum: "
                    : P.show (getDatumInPABSimulator @datumType blockchain (uTxORef, uTxOut))
                    : "Values: "
                    : formatValues uTxORef
                  | (uTxORef, uTxOut) <- utxos
                ]
        ----------------
        formatSelected :: [(Integer, Ledger.TxOutRef, Ledger.TxOut)] -> [P.String]
        formatSelected opciones' = concat [["--------------------------------", P.show numOpcion, P.show uTxORef] | (numOpcion, uTxORef, _) <- opciones']
    ----------------
    MonadIOClass.liftIO $ P.putStrLn "UTxOs:"
    mapM_ (MonadIOClass.liftIO . P.putStrLn) (formatUTxOValues uTxOuts)
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Selected:"
    mapM_ (MonadIOClass.liftIO . P.putStrLn) (formatSelected opciones)
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Option (0 to finish)"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Enter option:"
    opcionUTxO <- MonadIOClass.liftIO P.getLine
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    case TextRead.readMaybe opcionUTxO :: Maybe Integer of
        Just 0 ->
            return opciones
        Just x -> do
            if x >= 1 && x <= length uTxOuts
                then
                    let new = (x, fst $ uTxOuts !! (x - 1), snd $ uTxOuts !! (x - 1))
                        isEqOpciones (_, uTxORef1, _) (_, uTxORef2, _) = uTxORef1 == uTxORef2
                        news = new : filter (isEqOpciones new) opciones
                    in  selectUTxOs @datumType @a news uTxOuts blockchain
                else do
                    MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                    selectUTxOs @datumType @a opciones uTxOuts blockchain
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            selectUTxOs @datumType @a opciones uTxOuts blockchain

--------------------------------------------------------------------------------2

selectUTxOsWithAC :: forall datumType a. (P.Show datumType, LedgerApiV2.FromData datumType) => LedgerValue.AssetClass -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABEffects a) [(Integer, Ledger.TxOutRef, Ledger.TxOut)]
selectUTxOsWithAC unit_AC uTxOuts blockchain = do
    let !uTxOutsWithAC = [(txOutRef, txOut) | (txOutRef, txOut) <- uTxOuts, LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) unit_AC > 0]
    selectUTxOs @datumType @a [] uTxOutsWithAC blockchain

--------------------------------------------------------------------------------2

selectUTxOsWithCS :: forall datumType a. (P.Show datumType, LedgerApiV2.FromData datumType) => LedgerApiV2.CurrencySymbol -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABEffects a) [(Integer, Ledger.TxOutRef, Ledger.TxOut)]
selectUTxOsWithCS unit_CS uTxOuts blockchain = do
    let !uTxOutsWithCS = [(txOutRef, txOut) | (txOutRef, txOut) <- uTxOuts, OnChainHelpers.getAmtOfCurrencySymbol (Ledger.txOutValue txOut) unit_CS > 0]
    selectUTxOs @datumType @a [] uTxOutsWithCS blockchain

--------------------------------------------------------------------------------2

selectNFT :: [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABEffects a) (Maybe LedgerValue.AssetClass)
selectNFT uTxOuts blockchain = do
    let !addAllValues = foldl (<>) (LedgerAda.lovelaceValueOf 0) [Ledger.txOutValue txOut | (_, txOut) <- uTxOuts]
        !nfts' = [LedgerValue.AssetClass (cs, tk) | (cs, tk, am) <- OnChainHelpers.flattenValue addAllValues, am == 1]
    case nfts' of
        [] -> do
            MonadIOClass.liftIO $ P.putStrLn "There is no NFTs to select"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            return Nothing
        nfts -> do
            let formatNFTS =
                    concat
                        [ [ "--------------------------------",
                            "#: " ++ P.show (1 P.+ OnChainHelpers.fromJust (DataList.elemIndex nft nfts)),
                            "NFT: " ++ P.show nft
                          ]
                          | nft <- nfts
                        ]
            ----------------
            MonadIOClass.liftIO $ P.putStrLn "NFTs:"
            mapM_ (MonadIOClass.liftIO . P.putStrLn) formatNFTS
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            MonadIOClass.liftIO $ P.putStrLn "Enter option:"
            !opcionNFT <- MonadIOClass.liftIO P.getLine
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            case TextRead.readMaybe opcionNFT :: Maybe Integer of
                Just x -> do
                    if x >= 1 && x <= length nfts
                        then do
                            let !new = nfts !! (x - 1)
                            return $ Just new
                        else do
                            MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                            selectNFT uTxOuts blockchain
                _ -> do
                    MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                    selectNFT uTxOuts blockchain

--------------------------------------------------------------------------------2

selectNFTs :: [(Integer, Integer, LedgerValue.AssetClass)] -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABEffects a) [(Integer, Integer, LedgerValue.AssetClass)]
selectNFTs opciones uTxOuts blockchain = do
    let !addAllValues = foldl (<>) (LedgerAda.lovelaceValueOf 0) [Ledger.txOutValue txOut | (_, txOut) <- uTxOuts]
        !nfts' = [LedgerValue.AssetClass (cs, tk) | (cs, tk, am) <- OnChainHelpers.flattenValue addAllValues, am == 1]
    case nfts' of
        [] -> do
            MonadIOClass.liftIO $ P.putStrLn "There is no NFTs to select"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            return []
        nfts -> do
            let formatNFTS =
                    concat
                        [ [ "--------------------------------",
                            "#: " ++ P.show (1 P.+ OnChainHelpers.fromJust (DataList.elemIndex nft nfts)),
                            "NFT: " ++ P.show nft
                          ]
                          | nft <- nfts
                        ]
                ----------------
                formatSelected :: [(Integer, Integer, LedgerValue.AssetClass)] -> [P.String]
                formatSelected opciones' = concat [["--------------------------------", P.show numOpcion, P.show swPayWithADA, P.show nft] | (numOpcion, swPayWithADA, nft) <- opciones']
            ----------------
            MonadIOClass.liftIO $ P.putStrLn "NFTs:"
            mapM_ (MonadIOClass.liftIO . P.putStrLn) formatNFTS
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            MonadIOClass.liftIO $ P.putStrLn "Selected:"
            mapM_ (MonadIOClass.liftIO . P.putStrLn) (formatSelected opciones)
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            MonadIOClass.liftIO $ P.putStrLn "Enter option:"
            MonadIOClass.liftIO $ P.putStrLn "Option (0 to finish)"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            !opcionNFT <- MonadIOClass.liftIO P.getLine
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            case TextRead.readMaybe opcionNFT :: Maybe Integer of
                Just 0 ->
                    return opciones
                Just x -> do
                    if x >= 1 && x <= length nfts
                        then do
                            MonadIOClass.liftIO $ P.putStrLn "Pay with ADA? (y/n)"
                            swPayADABool <- MonadIOClass.liftIO CLIHelpers.getBool
                            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                            let swPayADAInt =
                                    if swPayADABool
                                        then 1 :: Integer
                                        else 0 :: Integer
                                ----------------
                                !new = (x, swPayADAInt, nfts !! (x - 1))
                                !news = new : filter (\(i, _, _) -> i P./= x) opciones
                            ----------------
                            selectNFTs news uTxOuts blockchain
                        else do
                            MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                            selectNFTs opciones uTxOuts blockchain
                _ -> do
                    MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                    selectNFTs opciones uTxOuts blockchain

--------------------------------------------------------------------------------2

prepareStdIn :: MonadFreerInternal.Eff (PABEffects a) ()
prepareStdIn = do
    _ <- MonadIOClass.liftIO $ SystemIO.hSetBuffering SystemIO.stdin SystemIO.NoBuffering
    _ <- MonadIOClass.liftIO $ SystemIO.hSetBuffering SystemIO.stdout SystemIO.NoBuffering -- SystemIO.LineBuffering
    _ <- MonadIOClass.liftIO $ SystemIO.hSetBuffering SystemIO.stderr SystemIO.NoBuffering -- SystemIO.LineBuffering
    _ <- MonadIOClass.liftIO $ SystemIO.hSetEcho SystemIO.stdin True
    return ()

--------------------------------------------------------------------------------2

waitStdinReady :: MonadFreerInternal.Eff (PABEffects a) ()
waitStdinReady = do
    _ <- MonadIOClass.liftIO $ ControlConcurrent.threadDelay 1_500_000 -- microseconds (10 seconds)
    _ <- MonadIOClass.liftIO $ SystemIO.hWaitForInput SystemIO.stdin (P.negate 0) -- 10 seconds
    _ <- MonadIOClass.liftIO $ SystemIO.hReady SystemIO.stdin
    _ <- MonadIOClass.liftIO $ ControlConcurrent.threadDelay 500_000 -- microseconds (10 seconds)

    return ()

--------------------------------------------------------------------------------2

waitKeyPress :: MonadFreerInternal.Eff (PABEffects a) ()
waitKeyPress = do
    -- MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Press any key to continue..."
    MonadIOClass.liftIO $ SystemIO.hSetEcho SystemIO.stdin False
    Monad.void $ MonadIOClass.liftIO P.getChar
    MonadIOClass.liftIO $ SystemIO.hSetEcho SystemIO.stdin True
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"

--------------------------------------------------------------------------------2

waitContractAndKeyPress :: PlutusContract.ContractInstanceId -> MonadFreerInternal.Eff (PABEffects a) ()
waitContractAndKeyPress contract = do
    _ <- PABSimulator.waitUntilFinished contract
    ----------------
    waitStdinReady
    ----------------
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    waitKeyPress

--------------------------------------------------------------------------------2
