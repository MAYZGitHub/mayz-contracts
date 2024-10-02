{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Generic.OffChainEval where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Cardano.Api                                     as CardanoApi
import qualified Cardano.Api.Shelley                             as CardanoApiShelley
import qualified Cardano.Binary                                  as CardanoBinary (ToCBOR, serializeEncoding)
import qualified Cardano.Ledger.Alonzo.Scripts                   as AlonzoScripts
import qualified Cardano.Ledger.Alonzo.Tx                        as AlonzoTx
import qualified Cardano.Ledger.Alonzo.TxWitness                 as AlonzoTxWitness
import qualified Cardano.Ledger.Babbage.TxBody                   as BabbageTxBody
import qualified Cardano.Ledger.Babbage.TxInfo                   as BabbageTxInfo
import qualified Cardano.Ledger.Core                             as CardanoLedgerCore
import qualified Cardano.Node.Emulator.TimeSlot                  as CardanoNodeEmulatorTimeSlot
import qualified Cardano.Node.Emulator.Validation                as CardanoNodeEmulatorValidation
import qualified Control.Lens                                    as ControlLens
import qualified Control.Monad                                   as ControlMonad
import qualified Control.Monad                                   as Monad
import qualified Data.Aeson                                      as DataAeson
import           Data.ByteString.Lazy                            (ByteString, readFile)
import qualified Data.ByteString.Lazy                            as LBS
import qualified Data.Default                                    as DataDefault
import qualified Data.Map                                        as DataMap
import qualified Data.Text                                       as DataText
import qualified Data.Typeable                                   as DataTypeable (Typeable)
import qualified Data.Void                                       as DataVoid (Void)
import qualified GHC.Generics                                    as Generic
import qualified GHC.Natural                                     as GHCNatural
import qualified Ledger
import qualified Ledger.Constraints                              as LedgerConstraints
import qualified Ledger.Tx                                       as LedgerTx
import qualified Ledger.Tx.CardanoAPI                            as LedgerTxCardanoAPI
import qualified Ledger.Tx.CardanoAPI.Internal                   as LedgerTxCardanoAPIInternal
import qualified Ledger.Tx.Constraints                           as LedgerTxConstraints
import qualified Plutus.ChainIndex                               as ChainIndex
import qualified Plutus.Contract                                 as PlutusContract
import qualified Plutus.Contract.CardanoAPI                      as PlutusContractCardanoAPI
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsTypedScriptsValidatorsV1 (DatumType, RedeemerType)
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import           PlutusTx.Prelude                                hiding (unless)
import qualified PlutusTx.Prelude                                as PlutusTxPrelude
import qualified Prelude                                         as P
import           System.Directory                                (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath                                 (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))
import qualified Text.Printf                                     as TextPrintf (printf)

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OffChainHelpers                         as OffChainHelpers
import qualified Generic.OnChainHelpers                          as OnChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2

evalAndSubmitTx ::
    forall w s.
    P.String ->
    [(LedgerApiV2.CurrencySymbol, LedgerApiV2.MintingPolicy)] ->
    [(LedgerApiV2.Address, LedgerApiV2.Validator)] ->
    [LedgerApiV2.BuiltinData -> Maybe P.String] ->
    LedgerConstraints.ScriptLookups DataVoid.Void ->
    LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void) ->
    -- LedgerApiV2.POSIXTime ->
    PlutusContract.Contract w s DataText.Text ()
evalAndSubmitTx nameEndPoint listOfMintingScripts listOfValidators showDatum lookupsTx tx  = do -- slotZeroTime
    ------------------------
    OffChainHelpers.printTitle (nameEndPoint ++ " : Create Tx")
    ------------------------
    !txUnBalanced <- PlutusContract.mkTxConstraints @DataVoid.Void lookupsTx tx
    !balanceTx <- PlutusContract.balanceTx txUnBalanced
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "txUnBalanced: %s" (P.show txUnBalanced)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "balanceTx: %s" (P.show balanceTx)
    ------------------------
    let
        getSomeCardanoApiTx :: LedgerTx.CardanoTx -> PlutusContract.Contract w s DataText.Text LedgerTx.SomeCardanoApiTx
        getSomeCardanoApiTx (Ledger.CardanoApiTx someCardanoApiTx') = return someCardanoApiTx'
        getSomeCardanoApiTx (LedgerTx.EmulatorTx _)                 =  PlutusContract.throwError "validateTx: No CardanoApiTx"
    someCardanoApiTx <- getSomeCardanoApiTx balanceTx
    let getEmulatorEraTx :: Ledger.SomeCardanoApiTx -> PlutusContract.Contract w s DataText.Text ( CardanoApiShelley.Tx CardanoApiShelley.BabbageEra)
        getEmulatorEraTx (LedgerTx.SomeTx tx' CardanoApiShelley.BabbageEraInCardanoMode) = return tx'
        getEmulatorEraTx _                                                               = PlutusContract.throwError "validateTx: No BabbageEraInCardanoMode"
        ------------------------
    emulatorEraTx <- getEmulatorEraTx someCardanoApiTx
        ------------------------
        -- !(CardanoApiShelley.ShelleyTx _ (AlonzoTx.ValidatedTx bodyTx AlonzoTxWitness.TxWitness {txrdmrs = AlonzoTxWitness.Redeemers redemersTx} (AlonzoTx.IsValid validTx) auxiliaryData)) = emulatorEraTx
    let !(CardanoApiShelley.ShelleyTx _ (AlonzoTx.ValidatedTx bodyTx AlonzoTxWitness.TxWitness {txrdmrs = AlonzoTxWitness.Redeemers redemersTx} (AlonzoTx.IsValid validTx) _)) = emulatorEraTx
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "BodyTx: %s" (P.show bodyTx)
        ------------------------
        !(CardanoApiShelley.ShelleyTx _ txVal) = emulatorEraTx
        ------------------------
        getSize :: (DataTypeable.Typeable era, CardanoBinary.ToCBOR (CardanoLedgerCore.TxBody era), CardanoBinary.ToCBOR (CardanoLedgerCore.AuxiliaryData era)) => AlonzoTx.ValidatedTx era -> P.Integer
        getSize !tx' = P.fromIntegral . LBS.length . CardanoBinary.serializeEncoding $ AlonzoTx.toCBORForSizeComputation tx'
        ------------------------
        !sizeTx = getSize txVal
        ------------------------
        !allRedeemers = DataMap.toList redemersTx
        ------------------------
        !( BabbageTxBody.TxBody
            _spendInputs
            _collateralInputs
            _referenceInputs
            _outputs
            _collateralReturn
            _totalCollateral
            _certs
            _wdrls
            _txfee
            _vldt
            _update
            _reqSignerHashes
            _mint
            _scriptIntegrityHash
            _adHash
            _txnetworkid
            ) = bodyTx
        ----------------------
        getTxBodyContent :: LedgerTx.SomeCardanoApiTx -> CardanoApiShelley.TxBodyContent CardanoApiShelley.ViewTx CardanoApiShelley.BabbageEra
        getTxBodyContent (LedgerTx.CardanoApiEmulatorEraTx (CardanoApiShelley.Tx (CardanoApiShelley.TxBody bodyContent) _)) = bodyContent
        ----------------------
        !txBodyContent = getTxBodyContent someCardanoApiTx
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "TxBodyContent: %s" (P.show txBodyContent)
        ----------------------
        getTxBodyContentInputsReference :: CardanoApi.TxBodyContent ctx era -> [LedgerTx.TxIn]
        getTxBodyContentInputsReference CardanoApi.TxBodyContent {..} =
            let txInsReferenceToPlutusTxIns CardanoApi.TxInsReferenceNone = []
                txInsReferenceToPlutusTxIns (CardanoApi.TxInsReference _ txIns'') =
                    fmap ((`LedgerTx.TxIn` Nothing) . LedgerTxCardanoAPIInternal.fromCardanoTxIn) txIns''
            in  txInsReferenceToPlutusTxIns txInsReference
        getTxBodyContentSignatures :: CardanoApi.TxBodyContent ctx era -> [LedgerApiV2.PubKeyHash]
        getTxBodyContentSignatures CardanoApi.TxBodyContent {..} =
            case txExtraKeyWits of
                CardanoApi.TxExtraKeyWitnessesNone ->
                    []
                CardanoApi.TxExtraKeyWitnesses _ xs ->
                    LedgerTxCardanoAPIInternal.fromCardanoPaymentKeyHash <$> xs
        ----------------------
        !txIns' = LedgerTx.getCardanoTxInputs balanceTx -- CardanoApi.txIns
        !txInsReference' = getTxBodyContentInputsReference txBodyContent -- CardanoApi.txInsReference
        !txOuts' = LedgerTx.getCardanoTxOutRefs balanceTx -- CardanoApi.txOuts
        !txFee' = LedgerTx.getCardanoTxFee balanceTx -- LedgerTxCardanoAPIInternal.fromCardanoFee $ CardanoApi.txFee txBodyContent  -- LedgerTx.txFee emTx
        !txValidityRange' = LedgerTx.getCardanoTxValidityRange balanceTx -- CardanoApi.txValidityRange txBodyContent -- CardanoNodeEmulatorTimeSlot.slotRangeToPOSIXTimeRange slotConfig (LedgerTx.txValidRange emTx)
        !txWithdrawals' = LedgerApiV2.fromList [] -- (LedgerApiV2.StakingHash $ LedgerTx.withdrawalCredential w, LedgerTx.withdrawalAmount w) | w <- txWithdrawals'] -- LedgerShelleyTxBody.unWdrl _wdrls -- CardanoApi.txWithdrawals txBodyContent -- _wdrls -- CardanoApi.txWithdrawals txBodyContent -- LedgerTx.txWithdrawals emTx
        !txCertificates' = [] -- _certs -- CardanoApi.txCertificates txBodyContent -- LedgerTx.certificateDcert <$> LedgerTx.txCertificates emTx
        !txMintValue' = LedgerTx.getCardanoTxMint balanceTx -- CardanoApi.txMintValue txBodyContent --LedgerTx.txMint emTx -- _mint
        -- txInfoRedeemers' = [] -- LedgerTx.getCardanoTxRedeemers balanceTx -- LedgerTx.txRedeemers emTx -- [(red', LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData () )]
        !txInfoRedeemers'' = mapM (BabbageTxInfo.transRedeemerPtr bodyTx) allRedeemers
        -- filter only left from either redeemer
        !txInfoRedeemers' = (\case Left _ -> []; Right x -> x) txInfoRedeemers''
        !txInfoData' = LedgerTx.getCardanoTxData balanceTx -- LedgerApiV2.fromList $ DataMap.toList $ LedgerTx.txData emTx
        !txInfoId' = LedgerTx.getCardanoTxId balanceTx -- LedgerTx.txId emTx
        !txInfoSignatories' = getTxBodyContentSignatures txBodyContent -- BabbageTxBody.reqSignerHashes txBodyContent --_reqSignerHashes -- LedgerTx.txSignatures emTx
        -- CardanoApi.txInsCollateral = TxInsCollateralNone
        -- CardanoApi.txTotalCollateral
        -- CardanoApi.txReturnCollateral
        -- CardanoApi.txMetadata
        -- CardanoApi.txAuxScripts
        -- CardanoApi.txExtraKeyWits
        -- CardanoApi.txProtocolParams = BuildTxWith Nothing
        -- CardanoApi.txUpdateProposal
        -- CardanoApi.txScriptValidity
        ----------------------
        getDecoratedTxOut :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerTx.DecoratedTxOut
        getDecoratedTxOut txInput' = do
            decoratedTxOut' <- PlutusContract.txOutFromRef txInput'
            let decoratedTxOut = OnChainHelpers.fromJust decoratedTxOut'
            return decoratedTxOut
        ----------------------
        -- getValueFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerApiV2.Value
        -- getValueFromTxOutRef txOutRef = do
        --     decoratedTxOut' <- PlutusContract.txOutFromRef txOutRef
        --     let decoratedTxOut = OnChainHelpers.fromJust decoratedTxOut'
        --     let value = OffChainHelpers.getValueFromDecoratedTxOut decoratedTxOut
        --     return value
        ----------------------
        getOutputDatumFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerApiV2.OutputDatum
        getOutputDatumFromTxOutRef txOutRef = do
            decoratedTxOut' <- PlutusContract.txOutFromRef txOutRef
            let decoratedTxOut = OnChainHelpers.fromJust decoratedTxOut'
                ----------------------
                toPlutusOutputDatum :: Maybe (LedgerApiV2.DatumHash, LedgerTx.DatumFromQuery) -> LedgerApiV2.OutputDatum
                toPlutusOutputDatum Nothing                            = LedgerApiV2.NoOutputDatum
                toPlutusOutputDatum (Just (_, LedgerTx.DatumInline d)) = LedgerApiV2.OutputDatum d
                toPlutusOutputDatum (Just (_, LedgerTx.DatumInBody d)) = LedgerApiV2.OutputDatum d
                toPlutusOutputDatum (Just (dh, _))                     = LedgerApiV2.OutputDatumHash dh
            ----------------------
            let datum = toPlutusOutputDatum $ decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutDatum
            return datum
        --------------------
        getReferenceScriptHashFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text (Maybe LedgerApiV2.ScriptHash)
        getReferenceScriptHashFromTxOutRef txOutRef = do
            decoratedTxOut' <- PlutusContract.txOutFromRef txOutRef
            let decoratedTxOut = OnChainHelpers.fromJust decoratedTxOut'
            let script' = decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutReferenceScript
            let script = OnChainHelpers.fromJust script'
            -- TODO: reemplaze esto: return $ P.maybe Nothing (Just . Ledger.scriptHash) script
            return ((Just . Ledger.scriptHash) P.=<< script)
        --------------------
        getReferenceScriptHashFromLedgerTxOut :: Ledger.TxOut -> PlutusContract.Contract w s DataText.Text (Maybe LedgerApiV2.ScriptHash)
        getReferenceScriptHashFromLedgerTxOut txOut = do
            let (CardanoApi.TxOut _ _ _ refScript') = LedgerTx.getTxOut txOut
            let refScript = PlutusContractCardanoAPI.fromCardanoTxOutRefScript refScript'
            let script' = ChainIndex.fromReferenceScript refScript
            return ((Just . Ledger.scriptHash) P.=<< script')
        --------------------
        getTxInInfoFromLedgerTxInput :: Ledger.TxIn -> PlutusContract.Contract w s DataText.Text LedgerApiV2.TxInInfo
        getTxInInfoFromLedgerTxInput txInput = do
            let txOutRef = LedgerTx.txInRef txInput
            decoratedTxOut <- getDecoratedTxOut txOutRef
            let address = LedgerTx._decoratedTxOutAddress decoratedTxOut -- ControlLens.^? LedgerTx.decoratedTxOutAddress
            let value = OffChainHelpers.getValueFromDecoratedTxOut decoratedTxOut
            datum <- getOutputDatumFromTxOutRef txOutRef
            refScriptHash <- getReferenceScriptHashFromTxOutRef txOutRef
            return $
                LedgerApiV2.TxInInfo
                    (LedgerTx.txInRef txInput)
                    (LedgerApiV2.TxOut address value datum refScriptHash)
        --------------------
        getTxOutFromLedgerTxOut :: (Ledger.TxOut, Ledger.TxOutRef) -> PlutusContract.Contract w s DataText.Text LedgerApiV2.TxOut
        getTxOutFromLedgerTxOut (txOut, _) =
            return $ LedgerTxCardanoAPIInternal.fromCardanoTxOutToPV2TxInfoTxOut $ LedgerTx.getTxOut txOut
        --------------------
        slotConfig :: CardanoNodeEmulatorTimeSlot.SlotConfig
        !slotConfig = DataDefault.def
            -- CardanoNodeEmulatorTimeSlot.SlotConfig
            --     { scSlotLength = 1,
            --       scSlotZeroTime = 0-- slotZeroTime
            --     }
    --------------------
    !mockTxInfoInputs <- traverse getTxInInfoFromLedgerTxInput txIns' -- :: [LedgerApiV2.TxInInfo]
    !mockTxReferenceInputs <- traverse getTxInInfoFromLedgerTxInput txInsReference' -- :: [LedgerApiV2.TxInInfo]
    !mockTxInfoOutputs <- traverse getTxOutFromLedgerTxOut txOuts' -- :: [LedgerApiV2.TxOut]
    ------------------
    let showExMemory :: LedgerApiV2.ExMemory -> P.String
        showExMemory (LedgerApiV2.ExMemory mem) = showStrNumbers mem
        ----------------------
        showExCPU :: LedgerApiV2.ExCPU -> P.String
        showExCPU (LedgerApiV2.ExCPU cpu) = showStrNumbers cpu
        ----------------------
        showStrNumbers :: (P.Show a) => a -> P.String
        showStrNumbers = OffChainHelpers.addSeparatorEachN '_' 3 . P.show
        ----------------------
    let showDatumType_ :: [LedgerApiV2.BuiltinData -> Maybe P.String] -> LedgerApiV2.BuiltinData -> Maybe P.String
        showDatumType_ [] bs = Just $ P.show bs
        showDatumType_ (f : fs) datum =
            case f datum of
                Nothing -> showDatumType_ fs datum
                Just d  -> Just d -- Just (LedgerApiV2.Datum $ PlutusTx.toBuiltinData d)
                ------------------
        showDatumType :: LedgerApiV2.OutputDatum -> Maybe P.String
        showDatumType (LedgerApiV2.OutputDatum (LedgerApiV2.Datum d)) = showDatumType_ showDatum d
        showDatumType (LedgerApiV2.OutputDatumHash _)                 = Nothing
        showDatumType LedgerApiV2.NoOutputDatum                       = Nothing
        ------------------
        getDatumFromOutputDatum :: LedgerApiV2.OutputDatum -> Maybe LedgerApiV2.Datum
        getDatumFromOutputDatum (LedgerApiV2.OutputDatum d)     = Just d
        getDatumFromOutputDatum (LedgerApiV2.OutputDatumHash _) = Nothing
        getDatumFromOutputDatum LedgerApiV2.NoOutputDatum       = Nothing
        ------------------
        getUnsafeDatumFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerApiV2.Datum
        getUnsafeDatumFromTxOutRef txOutRef = do
            outputDatum <- getOutputDatumFromTxOutRef txOutRef
            case getDatumFromOutputDatum outputDatum of
                Nothing -> PlutusContract.throwError "getUnsafeDatumFromTxOutRef: No Datum"
                Just d  -> return d
        ----------------------
        fromCardanoTxOutDatum :: CardanoApi.TxOutDatum CardanoApi.CtxTx era -> LedgerApiV2.OutputDatum
        fromCardanoTxOutDatum CardanoApi.TxOutDatumNone =
            LedgerApiV2.NoOutputDatum
        fromCardanoTxOutDatum (CardanoApi.TxOutDatumHash _ h) =
            LedgerApiV2.OutputDatumHash $ LedgerApiV2.DatumHash $ PlutusTxPrelude.toBuiltin (CardanoApi.serialiseToRawBytes h)
        fromCardanoTxOutDatum (CardanoApi.TxOutDatumInTx _ d) =
            LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusContractCardanoAPI.fromCardanoScriptData d
        -- LedgerApiV2.OutputDatumHash $ LedgerApiV2.DatumHash $ PlutusTxPrelude.toBuiltin (CardanoApi.serialiseToRawBytes (CardanoApi.hashScriptData d))
        fromCardanoTxOutDatum (CardanoApi.TxOutDatumInline _ d) =
            LedgerApiV2.OutputDatum $ LedgerApiV2.Datum $ PlutusContractCardanoAPI.fromCardanoScriptData d
        ------------------
        formatValues value = [P.show v | v <- OnChainHelpers.flattenValue value]
        ------------------
        formatTxInInfo :: P.String -> (Integer, LedgerApiV2.TxInInfo) -> PlutusContract.Contract w s DataText.Text [P.String]
        formatTxInInfo title (n, txInInfo) = do
            let txOutRef = LedgerApiV2.txInInfoOutRef txInInfo
            decoratedTxOut <- getDecoratedTxOut txOutRef
            let address = LedgerTx._decoratedTxOutAddress decoratedTxOut -- ControlLens.^? LedgerTx.decoratedTxOutAddress
            let value = OffChainHelpers.getValueFromDecoratedTxOut decoratedTxOut
            datum <- getOutputDatumFromTxOutRef txOutRef
            let datumTypeStr = showDatumType datum
            refScriptHash <- getReferenceScriptHashFromTxOutRef txOutRef
            let sep = case n of
                    0 -> []
                    _ -> ["--------------------------------"]
            return $
                sep
                    ++ [ title ++ ": " ++ P.show (n + 1 :: Integer),
                         "TxOutRef: " ++ P.show txOutRef,
                         "Address: " ++ P.show address,
                         "Values: "
                       ]
                    ++ formatValues value
                    ++ [
                         -- "Datum: " ++ P.show datum,
                         "Datum: ",
                         P.show datumTypeStr,
                         "ReferenceScript: "++ P.show refScriptHash  -- TODO
                       ]
        ------------------
        formatTxInInfos :: P.String -> [LedgerApiV2.TxInInfo] -> PlutusContract.Contract w s DataText.Text [P.String]
        formatTxInInfos _ [] = return []
        formatTxInInfos title txInsInfo = do
            formatList <- mapM (formatTxInInfo title) (OnChainHelpers.enumerate txInsInfo)
            return $ concat formatList
        ------------------
        formatTxOut :: (Integer, (Ledger.TxOut, Ledger.TxOutRef)) -> PlutusContract.Contract w s DataText.Text [P.String]
        formatTxOut (n, (txOut, txOutRef)) = do
            let (CardanoApi.TxOut address' value' datum' _) = LedgerTx.getTxOut txOut
            let address = Ledger.toPlutusAddress address'
            let value = LedgerTxCardanoAPIInternal.fromCardanoTxOutValue value'
            let datum = fromCardanoTxOutDatum datum'
            let datumTypeStr = showDatumType datum
            refScriptHash <- getReferenceScriptHashFromLedgerTxOut txOut
            let sep = case n of
                    0 -> []
                    _ -> ["--------------------------------"]
            return $
                sep
                    ++ [ "Out: " ++ P.show (n + 1 :: Integer),
                         "TxOutRef: " ++ P.show txOutRef,
                         "Address: " ++ P.show address,
                         "Values: "
                       ]
                    ++ formatValues value
                    ++ [
                         -- "Datum': " ++ P.show datum',
                         -- "Datum: " ++ P.show datum,
                         "Datum: ",
                         P.show datumTypeStr,
                         "ReferenceScript: " ++ P.show refScriptHash -- TODO
                       ]
        ------------------
        formatTxOuts :: [(Ledger.TxOut, Ledger.TxOutRef)] -> PlutusContract.Contract w s DataText.Text [P.String]
        formatTxOuts [] = return []
        formatTxOuts txOuts = do
            formatList <- mapM formatTxOut (OnChainHelpers.enumerate txOuts)
            return $ concat formatList
    ---------------------
    OffChainHelpers.printTitle (nameEndPoint ++ " : Inputs & Outputs")
    ---------------------
    OffChainHelpers.printSubTitle "Reference Inputs"
    formatMockTxReferenceInputs <- formatTxInInfos "Ref" mockTxReferenceInputs
    mapM_ (PlutusContract.logInfo @P.String) formatMockTxReferenceInputs
    ---------------------
    OffChainHelpers.printSubTitle "Inputs"
    formatMockTxInfoInputs <- formatTxInInfos "In" mockTxInfoInputs
    mapM_ (PlutusContract.logInfo @P.String) formatMockTxInfoInputs
    ---------------------
    OffChainHelpers.printSubTitle "Outputs"
    formatTxOuts' <- formatTxOuts txOuts'
    mapM_ (PlutusContract.logInfo @P.String) formatTxOuts'
    ---------------------
    OffChainHelpers.printTitle (nameEndPoint ++ " : Memory & Size")
    ---------------------
    ControlMonad.when (sizeTx > 16000)
        P.$ PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "Tx is too big: %s" (showStrNumbers sizeTx)
    -- if sizeTx > 16000
    --     then PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "Tx is too big: %s" (showStrNumbers sizeTx)
    --     else return ()
    ------------------------
    let !allUnits = map (\(_, (_, AlonzoScripts.ExUnits mem steps)) -> (mem, steps)) allRedeemers
        !sumUnitsMem = foldl (\acc (mem, _) -> acc `GHCNatural.plusNatural` mem) 0 allUnits
        !sumUnitsSteps = foldl (\acc (_, steps) -> acc `GHCNatural.plusNatural` steps) 0 allUnits
        formatUnits !list = concat [["ExMemory: " ++ showStrNumbers mem ++ ", ExCPU: " ++ showStrNumbers step] | (mem, step) <- list]
    ---------------------
    mapM_ (PlutusContract.logInfo @P.String) (formatUnits allUnits)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Total ExMemory: %s, Total ExCPU: %s, Total Size: %s" (showStrNumbers sumUnitsMem) (showStrNumbers sumUnitsSteps) (showStrNumbers sizeTx)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Valid Tx: %s" (P.show validTx)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
    ------------------------
    let mockTxInfoFee :: LedgerApiV2.Value
        mockTxInfoFee = txFee'
        ----------------------
        mockTxInfoMint :: LedgerApiV2.Value
        mockTxInfoMint = txMintValue'
        ----------------------
        mockTxInfoDCert :: [LedgerApiV2.DCert]
        mockTxInfoDCert = txCertificates'
        ----------------------
        mockTxInfoWdrl :: LedgerApiV2.Map LedgerApiV2.StakingCredential Integer
        mockTxInfoWdrl = txWithdrawals'
        ----------------------
        mockTxInfoValidRange :: LedgerApiV2.POSIXTimeRange
        mockTxInfoValidRange = CardanoNodeEmulatorTimeSlot.slotRangeToPOSIXTimeRange slotConfig txValidityRange'
        ----------------------
        mockTxInfoSignatories :: [LedgerApiV2.PubKeyHash]
        mockTxInfoSignatories = txInfoSignatories'
        ----------------------
        mockTxInfoRedeemers :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
        mockTxInfoRedeemers = LedgerApiV2.fromList txInfoRedeemers'
        ----------------------
        mockTxInfoData :: LedgerApiV2.Map LedgerApiV2.DatumHash LedgerApiV2.Datum
        mockTxInfoData = LedgerApiV2.fromList $ DataMap.toList txInfoData'
        ----------------------
        mockTxInfoId :: LedgerApiV2.TxId
        mockTxInfoId = txInfoId'
        ----------------------
        getTxInfo :: LedgerApiV2.TxInfo
        getTxInfo =
            LedgerApiV2.TxInfo
                mockTxInfoInputs
                mockTxReferenceInputs
                mockTxInfoOutputs
                mockTxInfoFee
                mockTxInfoMint
                mockTxInfoDCert
                mockTxInfoWdrl
                mockTxInfoValidRange
                mockTxInfoSignatories
                mockTxInfoRedeemers
                mockTxInfoData
                mockTxInfoId
        ----------------------
        evalRedeemer :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer) -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.ExMemory, LedgerApiV2.ExCPU, P.Integer)
        evalRedeemer (scriptPurpose, redeemer) = do
            case scriptPurpose of
                LedgerApiV2.Minting mintingHash -> do
                    OffChainHelpers.printSubTitle "EvalRedeemer Minting"
                    let policy' = DataMap.lookup mintingHash $ DataMap.fromList listOfMintingScripts
                    case policy' of
                        Nothing -> do
                            PlutusContract.logInfo @P.String $ "Redeemer: Minting Policy to Eval not found"
                            return (0, 0, 0)
                        Just policy -> do
                            let mockCtx :: LedgerApiV2.ScriptContext
                                mockCtx = LedgerApiV2.ScriptContext getTxInfo scriptPurpose
                                ----------------------
                                !paramsData = [] :: [LedgerApiV2.Data]
                                !redeemerData = LedgerApiV2.toData redeemer
                                !ctxData = LedgerApiV2.toData mockCtx
                                (eval_log, eval_err, eval_size) = OffChainHelpers.evaluateScriptMint policy paramsData redeemerData ctxData
                            --------------------------------
                            PlutusContract.logInfo @P.String $ "ScriptPurpose: " ++ P.show scriptPurpose
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            -- PlutusContract.logInfo @P.String $ "import Generic.OffChainEval"
                            -- PlutusContract.logInfo @P.String $ "evaluatePolicy_With_StringParams "
                            -- PlutusContract.logInfo @P.String $ "{\"epHash\":\"" ++ P.show mintingHash ++ "\",\"epRdeemer\":\""++ P.show (OffChainHelpers.getEncodedJsonFromData redeemerData)++"\",\"epCtx\":\""++ P.show (OffChainHelpers.getEncodedJsonFromData ctxData)++"\"}"
                            -- PlutusContract.logInfo @P.String $ "--------------------------------"
                            PlutusContract.logInfo @P.String $ "Redeemer: " ++ P.show redeemer
                            PlutusContract.logInfo @P.String $ "Log: " ++ P.show eval_log
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            case eval_err of
                                Left _ -> do
                                    PlutusContract.logInfo @P.String $ "Eval Error , Size: " ++ showStrNumbers eval_size
                                    return (0, 0, eval_size)
                                Right exbudget -> do
                                    PlutusContract.logInfo @P.String $ "ExMemory: " ++ showExMemory ( LedgerApiV2.exBudgetMemory exbudget) ++ ", ExCPU: " ++  showExCPU (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++ "Size: " ++ showStrNumbers eval_size
                                    return (LedgerApiV2.exBudgetMemory exbudget, LedgerApiV2.exBudgetCPU exbudget, eval_size)
                -- PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
                LedgerApiV2.Spending txOutRef -> do
                    OffChainHelpers.printSubTitle "EvalRedeemer Spending"
                    let mockCtx :: LedgerApiV2.ScriptContext
                        mockCtx = LedgerApiV2.ScriptContext getTxInfo scriptPurpose
                    ----------------------
                    dec <- getDecoratedTxOut txOutRef
                    let address = LedgerTx._decoratedTxOutAddress dec
                        validator' = DataMap.lookup address $ DataMap.fromList listOfValidators
                    ----------------------
                    case validator' of
                        Nothing -> do
                            PlutusContract.logInfo @P.String $ "Redeemer: Validator to Eval not found"
                            return (0, 0, 0)
                        Just validator -> do
                            datum <- getUnsafeDatumFromTxOutRef txOutRef
                            let datumTypeStr = showDatumType_ showDatum (LedgerApiV2.getDatum  datum)
                            let !paramsData = [] :: [LedgerApiV2.Data]
                                !datumData = LedgerApiV2.toData datum
                                !redeemerData = LedgerApiV2.toData redeemer
                                !ctxData = LedgerApiV2.toData mockCtx
                                (eval_log, eval_err, eval_size) = OffChainHelpers.evaluateScriptValidator validator paramsData datumData redeemerData ctxData
                            --------------------------------
                            PlutusContract.logInfo @P.String $ "ScriptPurpose: " ++ P.show scriptPurpose
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            PlutusContract.logInfo @P.String $ "import Generic.OffChainEval"
                            PlutusContract.logInfo @P.String $ "evaluateValidator_With_StringParams "
                            PlutusContract.logInfo @P.String $ "{\"evHash\":\"" ++ P.show (OffChainHelpers.hashValidator validator) ++ "\",\"evDatum\":\""++ P.show (OffChainHelpers.getEncodedJsonFromData datumData)++"\",\"evRedeemer\":\""++ P.show (OffChainHelpers.getEncodedJsonFromData redeemerData)++"\",\"evCtx\":\""++ P.show (OffChainHelpers.getEncodedJsonFromData ctxData)++"\"}"
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            PlutusContract.logInfo @P.String $ ":set -XOverloadedStrings"
                            PlutusContract.logInfo @P.String $ "import PlutusTx"
                            PlutusContract.logInfo @P.String $ "import qualified Plutus.V2.Ledger.Api                          as LedgerApiV2"
                            PlutusContract.logInfo @P.String $ "import qualified Ledger.Value                                  as LedgerValue"
                            PlutusContract.logInfo @P.String $ "import qualified Prelude                                       as P"
                            PlutusContract.logInfo @P.String $ "import Generic.OffChainHelpers"
                            PlutusContract.logInfo @P.String $ "import Generic.OffChainEval"
                            PlutusContract.logInfo @P.String $ "import Protocol.xxxxx.OnChain"
                            PlutusContract.logInfo @P.String $ "import Protocol.xxxxx.Types"
                            PlutusContract.logInfo @P.String $ "validatorParams = ValidatorParams { xxx = xxx} "
                            PlutusContract.logInfo @P.String $ "validatorWithParams = validator validatorParams"
                            PlutusContract.logInfo @P.String $ "paramsData = [] :: [LedgerApiV2.Data]"
                            PlutusContract.logInfo @P.String $ "datumEncodedJson = " ++ P.show (OffChainHelpers.getEncodedJsonFromData datumData)
                            PlutusContract.logInfo @P.String $ "datumData = getDataFromEncodedJson datumEncodedJson"
                            PlutusContract.logInfo @P.String $ "redeemerEncodedJson = " ++P.show (OffChainHelpers.getEncodedJsonFromData redeemerData)
                            PlutusContract.logInfo @P.String $ "redeemerData = getDataFromEncodedJson redeemerEncodedJson"
                            PlutusContract.logInfo @P.String $ "ctxEncodedJson = " ++ P.show (OffChainHelpers.getEncodedJsonFromData ctxData)
                            PlutusContract.logInfo @P.String $ "ctxData = getDataFromEncodedJson ctxEncodedJson"
                            PlutusContract.logInfo @P.String $ "evaluateScriptValidator validatorWithParams paramsData datumData redeemerData ctx"
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            PlutusContract.logInfo @P.String $ "Script Hash: " ++ P.show (OffChainHelpers.hashValidator validator)
                            PlutusContract.logInfo @P.String $ "Script Address: " ++ P.show address
                            PlutusContract.logInfo @P.String $ "Datum: " ++ P.show datumTypeStr
                            PlutusContract.logInfo @P.String $ "Redeemer: " ++ P.show redeemer
                            PlutusContract.logInfo @P.String $ "Log: " ++ P.show eval_log
                            PlutusContract.logInfo @P.String $ "--------------------------------"
                            case eval_err of
                                Left _ -> do
                                    PlutusContract.logInfo @P.String $ "Eval Error, Size: " ++ showStrNumbers eval_size
                                    return (0, 0, eval_size)
                                Right exbudget -> do
                                    PlutusContract.logInfo @P.String $ "ExMemory: " ++ showExMemory (LedgerApiV2.exBudgetMemory exbudget) ++ ", ExCPU: " ++ showExCPU (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++ "Size: " ++ showStrNumbers eval_size
                                    return (LedgerApiV2.exBudgetMemory exbudget, LedgerApiV2.exBudgetCPU exbudget, eval_size)
                LedgerApiV2.Rewarding _ ->
                    return (0, 0, 0)
                LedgerApiV2.Certifying _ ->
                    return (0, 0, 0)
    --------------------
    resultEvalRedemeers <- mapM evalRedeemer txInfoRedeemers'
    let !sumsEvals = foldl (\(mem', steps', size') (mem, steps, size) -> (mem' P.+ mem, steps' P.+ steps, size' + size)) (0, 0, 0) resultEvalRedemeers
    OffChainHelpers.printSubTitle "EvalRedeemer Totals"
    let getFst (fst', _, _) = fst'
        getSnd (_, snd', _) = snd'
        getThd (_, _, thd') = thd'
    PlutusContract.logInfo @P.String $ "--------------------------------"
    PlutusContract.logInfo @P.String $ "--------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Total ExMemory: %s, Total ExCPU: %s, Total Size: %s" (showExMemory (getFst sumsEvals)) (showExCPU (getSnd sumsEvals)) (showStrNumbers (getThd sumsEvals))
    PlutusContract.logInfo @P.String $ "--------------------------------"
    PlutusContract.logInfo @P.String $ "--------------------------------"
    --------------------
    OffChainHelpers.printTitle (nameEndPoint ++ " : Submit Tx")
    !submittedTx <- PlutusContract.submitBalancedTx balanceTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmitedTx (txId: %s)" (P.show $ Ledger.getCardanoTxId submittedTx)
    !cSlot <- PlutusContract.currentNodeClientSlot
    !params <- PlutusContract.getParams
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "params: %s" (P.show params)
    --------------------
    let
        -- getCUtxoIndex :: LedgerTx.UnbalancedTx -> PlutusContract.Contract w s DataText.Text LedgerTx.SomeCardanoApiTx
        getcUtxoIndex (Right cUtxoIndex') = return cUtxoIndex'
        getcUtxoIndex (Left _)            =  PlutusContract.throwError "Cant get cUtxoIndex"
    -- cUtxoIndex <- getCUtxoIndex txUnBalanced
    cUtxoIndex <- getcUtxoIndex $ LedgerTxCardanoAPI.fromPlutusIndex $ Ledger.UtxoIndex $ LedgerTxConstraints.unBalancedTxUtxoIndex txUnBalanced
    --------------------
    -- Ledger.UtxoIndex DataMap.empty --mempty  -- Ledger.UtxoIndex DataMap.empty --LedgerTxCardanoAPI.fromPlutusIndex mempty -- (Ledger.UtxoIndex utxo)
    let validateCardanoTx = CardanoNodeEmulatorValidation.validateCardanoTx params cSlot cUtxoIndex submittedTx
    case validateCardanoTx of
        -- Left (validationPhase, validationError) ->
        Left err ->
            -- PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "validateCardanoTx (txId: %s) - ERROR: %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show err)
            -- PlutusContract.logError @P.String $ P.show err
            PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "ERROR: %s" (P.show err)
        Right mp -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "validateCardanoTx (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show mp)
            !txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf "ConfirmedTx (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
    OffChainHelpers.printSeparator

--------------------------------------------------------------------------------2

findHashAndReadMintingPolicy :: P.FilePath -> P.String -> P.IO (Maybe LedgerApiV2.MintingPolicy)
findHashAndReadMintingPolicy directory valueToSearch = do
  files <- listDirectoryRecursive directory ".symbol"
  P.putStrLn $ "Searching Minting Hash: "++valueToSearch++" in files: "++ P.show files
  matchingFile <- findValueInFiles valueToSearch files
  case matchingFile of
    Just fileName -> readPlutusFileAsMintingPolicy fileName
    Nothing       -> return Nothing

findHashAndReadValidator :: P.FilePath -> P.String -> P.IO (Maybe LedgerApiV2.Validator)
findHashAndReadValidator directory valueToSearch = do
  files <- listDirectoryRecursive directory ".hash"
  P.putStrLn $ "Searching Script Hash: "++valueToSearch++" in files: "++ P.show files
  matchingFile <- findValueInFiles valueToSearch files
  case matchingFile of
    Just fileName -> readPlutusFileAsValidator fileName
    Nothing       -> return Nothing

isCorrectFile :: P.FilePath -> P.FilePath ->  Bool
isCorrectFile filePath filterFile = takeExtension filePath P.== filterFile

listDirectoryRecursive :: P.FilePath -> P.FilePath -> P.IO [P.FilePath]
listDirectoryRecursive path filterFile = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
        contents <- listDirectory path
        let paths = map (path </>) contents
        Monad.foldM (\acc path' -> do
                    subP <- listDirectoryRecursive path' filterFile
                    return $ subP ++ acc
                    ) [] paths
    else
        if isCorrectFile path filterFile then
            return [path]
        else
            return []

findValueInFiles :: P.String -> [P.FilePath] -> P.IO (Maybe P.FilePath)
findValueInFiles _ [] = return Nothing
findValueInFiles valueToSearch (filePath:rest) = do
    contents <- readFile filePath
    P.putStrLn $ "Searching file: "++ P.show filePath
    if OffChainHelpers.stringContains valueToSearch (OffChainHelpers.lazyByteStringToString contents)
    then do
        return $ Just filePath
    else
        findValueInFiles valueToSearch rest

decodeSymbolFile :: ByteString -> P.String
decodeSymbolFile = OffChainHelpers.lazyByteStringToString

extractPathAndFilename ::  P.FilePath -> ( P.FilePath,  P.FilePath)
extractPathAndFilename filePath = (takeDirectory filePath, takeFileName filePath)


readPlutusFileAsMintingPolicy :: P.FilePath -> P.IO (Maybe  LedgerApiV2.MintingPolicy)
readPlutusFileAsMintingPolicy filePath = do
  let plutusfilePath = dropExtension filePath ++ ".plutus"
  fileExists <- doesFileExist plutusfilePath
  if fileExists
    then
        do
            let (path, fileName) = extractPathAndFilename plutusfilePath
            (Right policy) <- OffChainHelpers.readMintingPolicy path fileName
            return $ Just policy
    else return Nothing

readPlutusFileAsValidator :: P.FilePath -> P.IO (Maybe  LedgerApiV2.Validator)
readPlutusFileAsValidator filePath = do
  let plutusfilePath = dropExtension filePath ++ ".plutus"
  fileExists <- doesFileExist plutusfilePath
  if fileExists
    then
        do
            let (path, fileName) = extractPathAndFilename plutusfilePath
            (Right policy) <- OffChainHelpers.readValidator path fileName
            return $ Just policy
    else return Nothing


replaceQuotes :: P.String -> P.String
replaceQuotes = DataText.unpack . DataText.replace "\"\"" "\"" . DataText.pack

--------------------------------------------------------------------------------2

data ParamsEvalPolicy
    = ParamsEvalPolicy
          { epHash     :: P.String
          , epRedeemer :: P.String
          , epCtx      :: P.String
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, Generic.Generic, P.Eq, P.Ord, P.Show)

data ParamsEvalValidator
    = ParamsEvalValidator
          { evHash     :: P.String
          , evDatum    :: P.String
          , evRedeemer :: P.String
          , evCtx      :: P.String
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, Generic.Generic, P.Eq, P.Ord, P.Show)

--------------------------------------------------------------------------------2

evaluatePolicy_With_StringParams :: P.String -> P.IO  (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Integer)
evaluatePolicy_With_StringParams paramsJsonStr = do
    let paramsEvalPolicy' = DataAeson.decode @ParamsEvalPolicy (OffChainHelpers.stringToLazyByteString $ replaceQuotes paramsJsonStr)
    case paramsEvalPolicy' of
        Nothing -> do
            P.error "Cant decode paramsEvalPolicy"
        Just ParamsEvalPolicy{..} -> do
            policy' <- findHashAndReadMintingPolicy "export" epHash
            case policy' of
                Nothing -> P.error "Policy not found"
                Just policy -> do
                    P.putStrLn $ "Policy: "++ P.show policy
                    P.putStrLn $ "RedeemerStr: " ++ P.show epRedeemer
                    let paramsData = [] :: [LedgerApiV2.Data]
                        redeemerData = OffChainHelpers.getDataFromEncodedJson (OffChainHelpers.stringToLazyByteString epRedeemer)
                        ctxData = OffChainHelpers.getDataFromEncodedJson (OffChainHelpers.stringToLazyByteString epCtx)
                    P.putStrLn $ "RedeemerData: " ++ P.show redeemerData
                    let (eval_log, eval_err, eval_size) = OffChainHelpers.evaluateScriptMint policy paramsData redeemerData ctxData
                    return (eval_log, eval_err, eval_size)

--------------------------------------------------------------------------------2

evaluateValidator_With_StringParams :: P.String -> P.IO  (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Integer)
evaluateValidator_With_StringParams paramsJsonStr = do
    let paramsEvalValidator' = DataAeson.decode @ParamsEvalValidator (OffChainHelpers.stringToLazyByteString $ replaceQuotes paramsJsonStr)
    case paramsEvalValidator' of
        Nothing -> do
            P.error "Cant decode paramsEvalValidator"
        Just ParamsEvalValidator{..} -> do
            validator' <- findHashAndReadValidator "export" evHash
            case validator' of
                Nothing -> P.error "Policy not found"
                Just validator -> do
                    P.putStrLn $ "Validator: "++ P.show validator
                    P.putStrLn $ "DatumStr: " ++ P.show evDatum
                    P.putStrLn $ "RedeemerStr: " ++ P.show evRedeemer
                    let paramsData = [] :: [LedgerApiV2.Data]
                        datumData = OffChainHelpers.getDataFromEncodedJson (OffChainHelpers.stringToLazyByteString evDatum)
                        redeemerData = OffChainHelpers.getDataFromEncodedJson (OffChainHelpers.stringToLazyByteString evRedeemer)
                        ctxData = OffChainHelpers.getDataFromEncodedJson (OffChainHelpers.stringToLazyByteString evCtx)
                    P.putStrLn $ "datumData: " ++ P.show datumData
                    P.putStrLn $ "redeemerData: " ++ P.show redeemerData
                    let (eval_log, eval_err, eval_size) = OffChainHelpers.evaluateScriptValidator validator paramsData datumData redeemerData ctxData
                    return (eval_log, eval_err, eval_size)

--------------------------------------------------------------------------------2

