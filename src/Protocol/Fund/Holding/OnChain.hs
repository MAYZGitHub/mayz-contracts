{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

----------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
----------------------------------------------------------------------------2

module Protocol.Fund.Holding.OnChain where

----------------------------------------------------------------------------2
-- Import Externos
----------------------------------------------------------------------------2

import qualified Ledger
import qualified Ledger.Ada                  as LedgerAda
import qualified Ledger.Value                as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts   as LedgerContextsV2
import           PlutusTx                    (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as TxAssocMap
import           PlutusTx.Prelude
----------------------------------------------------------------------------2
-- Import Internos
----------------------------------------------------------------------------2

import qualified Generic.Constants           as T
import qualified Generic.OnChainHelpers      as OnChainHelpers
import qualified Generic.Types               as T
import qualified Protocol.Constants          as T
import qualified Protocol.Fund.Helpers       as FundHelpers
import qualified Protocol.Fund.Holding.Types as T
import qualified Protocol.Fund.Types         as FundT
import qualified Protocol.InvestUnit.Types   as InvestUnitT
import qualified Protocol.Protocol.Types     as ProtocolT
import qualified Protocol.Types              as T

----------------------------------------------------------------------------2
-- Modulo
----------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID (T.PolicyParams !ppFundPolicy_CS) !redRaw !ctxRaw =
    if  traceIfFalse "" useThisToMakeScriptUnique
        && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
        && case redeemer of
            (T.PolicyRedeemerMintID _) ->
                    ------------------
                    -- it runs along with Fund Validator (ValidatorRedeemerFundHoldingAdd)
                        -- validateAdminAction
                        -- traceIfFalse "not isCorrect_Output_Fund_Datum_With_HoldingAdded" isCorrect_Output_Fund_Datum_With_HoldingAdded
                        -- && traceIfFalse "not isCorrect_Output_Fund_Value_NotChanged" isCorrect_Output_Fund_Value_NotChanged
                        -- && traceIfFalse "not isMintingFundHoldingID" isMintingFundHoldingID
                    ------------------
                    -- Que se consuma FundDatum con redeemer correcto (AddHolding)
                    -- Para identificar el correcto FundDatum necesita la póliza Fund ID que está en los parámetros de esta póliza.
                    -- Que se genere salida con nuevo HoldingDatum en Holding Val (Holding Val está indicada en FundDatum)
                    -- Que el HoldingDatum sea correcto
                    -- Que se mintee Holding ID con own póliza
                    -- Que el HoldingDatum tenga el Holding ID
                    ------------------
                    traceIfFalse "not isMintingFundHoldingID" isMintingFundHoldingID &&
                    traceIfFalse "not isCorrect_Redeemer_Fund" (isCorrect_Redeemer_Fund isFundValidatorRedeemerFundHoldingAdd ) &&
                    traceIfFalse "not isCorrect_Output_FundHolding_Datum" isCorrect_Output_FundHolding_Datum &&
                    traceIfFalse "not isCorrect_Output_FundHolding_Value" isCorrect_Output_FundHolding_Value
                    ------------------
                where
                     ------------------
                    !outputs_txOuts = [ txOut | txOut <- LedgerApiV2.txInfoOutputs info,
                        OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)  ]
                    ------------------
                    -- 0 out is the FundDatum
                    -- 1 out is the FundHoldingDatum
                    ------------------
                    !outputs_txOuts_index1 = if length outputs_txOuts < 2
                        then traceError "Expected at least two outputs to script addresses"
                        else outputs_txOuts!!1
                    ------------------
                    !input_TxOut_And_FundDatum = (\(_, !txOut, !datum) -> (txOut, datum)) input_TxOutRef_TxOut_And_FundDatum
                    ------------------
                    !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundDatum
                    ------------------
                    !fundHolding_Index = FundT.fdHoldingsIndex fundDatum_In
                    !fundHoldingID_TN = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index
                    !fundHoldingID_AC = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN)
                    ------------------
                    !fundHoldingValidator_Hash = FundT.fdFundHoldingValidator_Hash fundDatum_In
                    !fundHoldingValidator_Address = Ledger.scriptHashAddress fundHoldingValidator_Hash
                    ------------------
                    !output_Own_TxOut_And_FundHoldingDatum =  fromMaybe
                        (traceError "Expected FundHolding at output index 1")
                        (OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                            @T.ValidatorDatum
                            @T.FundHoldingDatumType
                            ctx
                            outputs_txOuts_index1
                            fundHoldingID_AC
                            (Just fundHoldingValidator_Address)
                            T.getFundHolding_DatumType)
                    ------------------
                    !fundHoldingDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
                    ------------------
                    !valueFor_Mint_FundHoldingID = LedgerValue.assetClassValue fundHoldingID_AC 1
                    ------------------
                    !minADA_For_FundHoldingDatum_Out = T.hdMinADA fundHoldingDatum_Out
                    !value_MinADA_For_FundHoldingDatum_Out = LedgerAda.lovelaceValueOf minADA_For_FundHoldingDatum_Out
                    !valueFor_FundHoldingDatum_Out_Control = valueFor_Mint_FundHoldingID <> value_MinADA_For_FundHoldingDatum_Out
                    ------------------
                    !fundHoldingDatum_Out_Control = T.FundHoldingDatumType  {
                            T.hdFundHolding_Index = fundHolding_Index,
                            T.hdSubtotal_FT_Minted_Accumulated = 0,
                            T.hdSubtotal_FT_Minted = 0,
                            T.hdSubtotal_FT_Commissions = 0,
                            T.hdSubtotal_FT_Commissions_Rate1e6_PerMonth = 0,
                            T.hdSubtotal_FT_Commissions_Collected_Protocol = 0,
                            T.hdSubtotal_FT_Commissions_Collected_Managers = 0,
                            T.hdSubtotal_FT_Commissions_Collected_Delegators = 0,
                            T.hdMinADA = minADA_For_FundHoldingDatum_Out
                        }
                    ------------------
                    isFundValidatorRedeemerFundHoldingAdd :: FundT.ValidatorRedeemer -> Bool
                    isFundValidatorRedeemerFundHoldingAdd redemeerToCheck = case redemeerToCheck of
                        FundT.ValidatorRedeemerFundHoldingAdd _ -> True
                        _                                       -> False
                    ------------------
                    isMintingFundHoldingID :: Bool
                    !isMintingFundHoldingID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_FundHoldingID
                    -----------------
                    isCorrect_Output_FundHolding_Datum :: Bool
                    !isCorrect_Output_FundHolding_Datum =
                        fundHoldingDatum_Out `OnChainHelpers.isUnsafeEqDatums` fundHoldingDatum_Out_Control
                    ------------------
                    isCorrect_Output_FundHolding_Value :: Bool
                    !isCorrect_Output_FundHolding_Value =
                        let !valueOf_FundHoldingDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
                        in  valueOf_FundHoldingDatum_Out `OnChainHelpers.isEqValue` valueFor_FundHoldingDatum_Out_Control
                    ------------------
            (T.PolicyRedeemerBurnID _) ->
                    ------------------
                    -- it runs along with Fund Validator (ValidatorRedeemerFundHoldingDelete)
                        -- validateAdminAction
                        -- traceIfFalse "not isCorrect_Output_Fund_Datum_With_HoldingDeleted" isCorrect_Output_Fund_Datum_With_HoldingDeleted
                        -- && traceIfFalse "not isCorrect_Output_Fund_Value_NotChanged" isCorrect_Output_Fund_Value_NotChanged
                        -- && traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
                    ------------------
                    -- it runs along with FundHolding Validator (ValidatorRedeemerDelete)
                        -- traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
                    ------------------
                    -- Que se consuma FundDatum con redeemer correcto (DeleteHolding)
                    -- Para identificar el correcto FundDatum necesita la póliza Fund ID que está en los parámetros de esta póliza.
                    -- Que se queme Holding ID con own póliza
                    -- que no haya tokens en el FundHolding
                    ------------------
                    traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
                    && traceIfFalse "not isCorrect_Redeemer_Fund" (isCorrect_Redeemer_Fund isFundValidatorRedeemerFundHoldingDelete)
                    && traceIfFalse "not isCorrect_Redeemer_FundHolding" (isCorrect_Redeemer_FundHolding isFundHoldingValidatorRedeemerDelete)
                    && traceIfFalse "not isZeroAssets" isZeroAssets
                    ------------------
                where
                    ------------------
                    !input_TxOutRef_TxOut_And_FundHoldingDatum =
                        case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS
                            @T.ValidatorDatum
                            @T.FundHoldingDatumType
                            ctx
                            inputs_TxOutRefs_TxOuts
                            fundHoldingPolicyID_CS
                            T.getFundHolding_DatumType of
                        [x] -> x
                        _   -> traceError "Expected exactly one FundHolding input"
                    ------------------
                    !input_TxOut_And_FundHoldingDatum = (\(_, !txOut, !datum) -> (txOut, datum)) input_TxOutRef_TxOut_And_FundHoldingDatum
                    ------------------
                    !fundHoldingDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundHoldingDatum
                    ------------------
                    !fundHolding_Index = T.hdFundHolding_Index fundHoldingDatum_In
                    !fundHoldingID_TN = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index
                    !fundHoldingID_AC = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN)
                    ------------------
                    !valueFor_Mint_FundHoldingID = LedgerValue.assetClassValue fundHoldingID_AC 1
                    !valueFor_Burn_FundHoldingID = LedgerValue.assetClassValue fundHoldingID_AC (negate 1)
                    ------------------
                    isBurningFundHoldingID :: Bool
                    !isBurningFundHoldingID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_FundHoldingID
                    -----------------
                    isFundValidatorRedeemerFundHoldingDelete :: FundT.ValidatorRedeemer -> Bool
                    isFundValidatorRedeemerFundHoldingDelete !redemeerToCheck = case redemeerToCheck of
                        FundT.ValidatorRedeemerFundHoldingDelete _ -> True
                        _                                          -> False
                    -----------------
                    isFundHoldingValidatorRedeemerDelete :: T.ValidatorRedeemer -> Bool
                    isFundHoldingValidatorRedeemerDelete !redemeerToCheck = case redemeerToCheck of
                        T.ValidatorRedeemerDelete _ -> True
                        _                           -> False
                    ------------------
                    isZeroAssets :: Bool
                    !isZeroAssets =
                        let
                            ------------------
                            !minADA_For_FundHoldingDatum_In = T.hdMinADA fundHoldingDatum_In
                            !value_MinADA_For_FundHoldingDatum_In = LedgerAda.lovelaceValueOf minADA_For_FundHoldingDatum_In
                            !valueFor_FundHoldingDatum_In_Control = valueFor_Mint_FundHoldingID <> value_MinADA_For_FundHoldingDatum_In
                            ------------------
                            !valueOf_FundHoldingDatum_In = OnChainHelpers.getValue_In_TxOut_And_Datum input_TxOut_And_FundHoldingDatum
                        in  valueOf_FundHoldingDatum_In `OnChainHelpers.isEqValue` valueFor_FundHoldingDatum_In_Control
                    ------------------
                    isCorrect_Redeemer_FundHolding :: (T.ValidatorRedeemer -> Bool) -> Bool
                    isCorrect_Redeemer_FundHolding !isRedeemerType =
                        let !redeemerFor_FundHoldingDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef ) input_TxOutRef_TxOut_And_FundHoldingDatum) info
                        in case redeemerFor_FundHoldingDatum' of
                            Nothing -> False
                            Just redeemerFor_FundHoldingDatum -> case LedgerApiV2.fromBuiltinData @T.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_FundHoldingDatum of
                                Just x -> isRedeemerType x
                                _      -> False
                    ------------------
    then ()
    else error ()
        where
            ------------------
            !useThisToMakeScriptUnique = True
            ------------------
            !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
            !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
            !info = LedgerContextsV2.scriptContextTxInfo ctx
            ------------------
            !fundPolicy_CS = ppFundPolicy_CS
            !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
            ------------------
            !fundHoldingPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
            ------------------
            !inputs_TxOutRefs_TxOuts = [(LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | !txInfoInput <- LedgerApiV2.txInfoInputs info,
                OnChainHelpers.isScriptAddress $ LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)]
            ------------------
            !input_TxOutRef_TxOut_And_FundDatum =
                case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC
                    @FundT.ValidatorDatum
                    @FundT.FundDatumType
                    ctx
                    inputs_TxOutRefs_TxOuts
                    fundID_AC
                    FundT.getFund_DatumType of
                        [x] -> x
                        _   -> traceError "Expected exactly one Fund input"
            ------------------
            isCorrect_Redeemer_Fund :: (FundT.ValidatorRedeemer -> Bool) -> Bool
            isCorrect_Redeemer_Fund !isRedeemerType =
                let !redeemerFor_FundDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(!txOutRef, _, _) -> txOutRef ) input_TxOutRef_TxOut_And_FundDatum) info
                in case redeemerFor_FundDatum' of
                    Nothing -> False
                    Just !redeemerFor_FundDatum -> case LedgerApiV2.fromBuiltinData @FundT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_FundDatum of
                        Just x -> isRedeemerType x
                        _      -> False
            ------------------

----------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator (T.ValidatorParams !protocolPolicyID_CS !fundPolicy_CS !tokenEmergencyAdminPolicy_CS) !datumRaw !redRaw !ctxRaw =
    let
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        -- Si esta el token de emergencia se saltea todos los controles
        isEmergencyRedeemer :: Bool
        !isEmergencyRedeemer =
            case redeemer of
                (T.ValidatorRedeemerEmergency _) -> True
                _                                -> False
    in
        ------------------
        case isEmergencyRedeemer of
            True ->
                let
                    !tokenEmergencyAdmin_AC = LedgerValue.AssetClass (tokenEmergencyAdminPolicy_CS, T.protocolTokenEmergencyAdmin_TN)
                    -- search emergency admin token in output 0
                    !isEmergencyAdminTokenPresent = OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue $ head (LedgerApiV2.txInfoOutputs info)) tokenEmergencyAdmin_AC
                in
                    if traceIfFalse "not isEmergencyAdminTokenPresent" isEmergencyAdminTokenPresent
                        then ()
                        else error ()
            False ->
                if traceIfFalse "" useThisToMakeScriptUnique
                    && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
                    && traceIfFalse "Expected at least one FundHolding input" (not (null inputs_Own_TxOuts))
                    && validateRedeemer getRedeemerType
                    then ()
                    else error ()
                where
                    -- && traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                    ------------------
                    !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol && fundPolicy_CS /= LedgerApiV2.adaSymbol
                    ------------------
                    !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
                    ------------------
                    !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
                    !fundHolding_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
                    ------------------
                    !inputs_Own_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info,
                                    let !address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                    in  OnChainHelpers.isScriptAddress address && address == fundHolding_Validator_Address]
                    ------------------
                    !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                    ------------------
                    !outputs_txOuts = [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info,
                        OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)  ]
                    ------------------
                    !redeemerDepositAndWithdraw = 1
                    !redeemerReIdx = 2
                    !redeemerDelete = 3
                    !redeemerCommissions = 4
                    !redeemerUpdateMinADA = 5
                    !redeemerBalance = 6
                    ------------------
                    getRedeemerType :: Integer
                    !getRedeemerType = case redeemer of
                        (T.ValidatorRedeemerDeposit _)                       -> redeemerDepositAndWithdraw
                        (T.ValidatorRedeemerWithdraw _)                      -> redeemerDepositAndWithdraw
                        (T.ValidatorRedeemerReIndexing _)                    -> redeemerReIdx
                        (T.ValidatorRedeemerCollect_Protocol_Commission _)   -> redeemerCommissions
                        (T.ValidatorRedeemerCollect_Delegators_Commission _) -> redeemerCommissions
                        (T.ValidatorRedeemerCollect_Managers_Commission _)   -> redeemerCommissions
                        (T.ValidatorRedeemerDelete _)                        -> redeemerDelete
                        (T.ValidatorRedeemerUpdateMinADA _)                  -> redeemerUpdateMinADA
                        (T.ValidatorRedeemerBalanceAssets _)                 -> redeemerBalance
                        _                                                    -> traceError "Invalid redeemer"
                    ------------------
                    validateRedeemer :: Integer -> Bool
                    validateRedeemer !redeemerType
                        | redeemerType == redeemerDelete = validateDelete
                        | otherwise = validateAllButDelete
                        where
                            ------------------
                            validateDelete :: Bool
                            validateDelete =
                                ------------------
                                -- it runs along with Fund Validator (ValidatorRedeemerFundHoldingDelete)
                                    -- validateAdminAction
                                    -- traceIfFalse "not isCorrect_Output_Fund_Datum_With_HoldingDeleted" isCorrect_Output_Fund_Datum_With_HoldingDeleted
                                    -- && traceIfFalse "not isCorrect_Output_Fund_Value_NotChanged" isCorrect_Output_Fund_Value_NotChanged
                                    -- && traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
                                ------------------
                                -- it runs along with FundHolding ID Policy (PolicyRedeemerBurnID)
                                    -- traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
                                    -- && traceIfFalse "not isCorrect_Redeemer_Fund" (isCorrect_Redeemer_Fund isFundValidatorRedeemerFundHoldingDelete)
                                    -- && traceIfFalse "not isZeroAssets" isZeroAssets
                                ------------------
                                -- que sea fund admin, eso lo controla la validacion del Fund
                                -- Que se queme Holding ID con la correcta póliza indicada en FundDatum
                                -- Para identificar el correcto FundDatum necesita la póliza Fund ID que está en los parámetros de esta póliza.
                                -- que no haya tokens en el fundHOlding. Se verifica en la poliza de PolicyRedeemerBurnID
                                ------------------
                                traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                && traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
                                ------------------
                                where
                                    ------------------
                                    !outputs_txOuts_index0 =
                                        if null outputs_txOuts
                                            then traceError "Expected at least one output to script addresses"
                                            else head outputs_txOuts
                                    ------------------
                                    -- 0 out is the FundDatum
                                    ------------------
                                    !output_TxOut_And_FundDatum =
                                        fromMaybe
                                            (traceError "Expected Fund at output index 0")
                                            (OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                                @FundT.ValidatorDatum @FundT.FundDatumType
                                                ctx
                                                outputs_txOuts_index0
                                                fundID_AC
                                                Nothing
                                                FundT.getFund_DatumType)
                                    ------------------
                                    !fundDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_TxOut_And_FundDatum
                                    ------------------
                                    !fundHoldingPolicyID_CS = FundT.fdFundHoldingPolicyID_CS fundDatum_Out
                                    ------------------
                                    isBurningFundHoldingID :: Bool
                                    !isBurningFundHoldingID = OnChainHelpers.isNFT_Burning_With_CS fundHoldingPolicyID_CS info
                            ------------------
                            validateAllButDelete :: Bool
                            validateAllButDelete
                                | redeemerType == redeemerBalance = validateBalance
                                | otherwise = validateAllButBalance
                                ------------------
                                where
                                    ------------------
                                    !inputsRef_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoReferenceInputs info,
                                                OnChainHelpers.isScriptAddress $ LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)]
                                    ------------------
                                    !inputRef_TxOut_And_FundDatum =
                                        case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                                @FundT.ValidatorDatum @FundT.FundDatumType
                                                ctx
                                                inputsRef_TxOuts
                                                fundID_AC
                                                FundT.getFund_DatumType of
                                            [x] -> x
                                            _   -> traceError "Expected exactly one Fund input ref"
                                    ------------------
                                    !outputs_txOuts_index0 =
                                        if null outputs_txOuts
                                            then traceError "Expected at least one output to script addresses"
                                            else head outputs_txOuts
                                    ------------------
                                    !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_FundDatum
                                    ------------------
                                    !fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS, fundFT_TN)
                                    !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
                                    ------------------
                                    !fundHoldingPolicyID_CS = FundT.fdFundHoldingPolicyID_CS fundDatum_In
                                    ------------------
                                    isCorrect_Output_FundHolding_Datum :: T.FundHoldingDatumType -> T.FundHoldingDatumType -> Bool
                                    isCorrect_Output_FundHolding_Datum !fundHoldingDatum_Out' !fundHoldingDatum_Control = fundHoldingDatum_Out' `OnChainHelpers.isUnsafeEqDatums` fundHoldingDatum_Control
                                    ------------------
                                    isCorrect_Output_FundHolding_Value :: LedgerValue.Value -> LedgerValue.Value -> Bool
                                    isCorrect_Output_FundHolding_Value !valueOf_FundHoldingDatum_Out' !valueFor_FundHoldingDatum_Control =
                                        valueOf_FundHoldingDatum_Out' `OnChainHelpers.isEqValue` valueFor_FundHoldingDatum_Control
                                    ------------------
                                    validateAdminAction :: [T.WalletPaymentPKH] -> LedgerValue.AssetClass -> Bool
                                    validateAdminAction !admins' !tokenAdmin_AC =
                                            -- Que este el token de admin presente en output 1
                                            -- o Que sea Protocol Admin
                                            traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent" (OnChainHelpers.isSignedByAny admins' info || isAdminTokenPresent)
                                        where
                                            ------------------
                                            isAdminTokenPresent :: Bool
                                            isAdminTokenPresent = case LedgerApiV2.txInfoOutputs info of
                                                []         -> False
                                                -- search admin token in output 0
                                                (output:_) -> OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue output) tokenAdmin_AC
                                    ------------------
                                    validateFundAdminAction :: FundT.FundDatumType -> Bool
                                    validateFundAdminAction !fundDatum_In' =
                                        ------------------
                                        let !admins' = T.getAdmins fundDatum_In'
                                            !tokenAdminPolicy_CS = T.getAdminToken_CS fundDatum_In
                                            !tokenAdmin_AC = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.fundTokenAdmin_TN)
                                        ------------------
                                        in validateAdminAction admins' tokenAdmin_AC
                                    ------------------
                                    validateBalance :: Bool
                                    validateBalance = case redeemer of
                                        (T.ValidatorRedeemerBalanceAssets (T.ValidatorRedeemerBalanceAssetsType alterCommissionsFT)) ->
                                            ---------------------
                                            -- it runs alone
                                            ---------------------
                                            -- Que sea Fund Admin
                                            -- Que no se modifique datums ni la cantidad total de tokens en las entradas y salidas
                                            -- Que cada datum luego contiene la misma cantidad de minADA que dice el datum y el funHoldingID corresponmdoente al indice en el datum
                                            ------------------
                                            validateFundAdminAction fundDatum_In
                                            && traceIfFalse "not cantInputs == cantOutputs" (cantInputs == cantOutputs && length alterCommissionsFT == cantOutputs)
                                            && traceIfFalse "not isCorrect_Outputs_Commissions_SameTotal" isCorrect_Outputs_Commissions_SameTotal
                                            && traceIfFalse "not isCorrect_Outputs_FundHoldingDatums" isCorrect_Outputs_FundHoldingDatums
                                            && traceIfFalse "not isCorrect_Output_FundHolding_Datums_Values_SameTotal" isCorrect_Output_FundHolding_Values_SameTotal
                                            && traceIfFalse "not isCorrect_Output_FundHolding_Datums_Values" isCorrect_Output_FundHolding_Values
                                            ------------------
                                            where
                                                ------------------
                                                !outputs_Own_TxOuts = [txOut | !txOut <- outputs_txOuts,
                                                    LedgerApiV2.txOutAddress txOut == fundHolding_Validator_Address]
                                                ------------------
                                                !_outputs_txOuts_index0 =
                                                    if null outputs_Own_TxOuts
                                                        then traceError "Expected at least one own output"
                                                        else head outputs_Own_TxOuts
                                                ------------------
                                                !inputs_Own_TxOuts_And_FundHoldingDatums = OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
                                                        @T.ValidatorDatum @T.FundHoldingDatumType
                                                        ctx
                                                        inputs_Own_TxOuts
                                                        fundHoldingPolicyID_CS
                                                        T.getFundHolding_DatumType
                                                ------------------
                                                !cantInputs = length inputs_Own_TxOuts_And_FundHoldingDatums
                                                ------------------
                                                !outputs_Own_TxOuts_And_FundHoldingDatums =
                                                    OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
                                                        @T.ValidatorDatum @T.FundHoldingDatumType
                                                        ctx
                                                        outputs_Own_TxOuts
                                                        fundHoldingPolicyID_CS
                                                        T.getFundHolding_DatumType
                                                ------------------
                                                !cantOutputs = length outputs_Own_TxOuts_And_FundHoldingDatums
                                                ------------------
                                                !inputs_Values_And_FundHoldingDatums =  [(OnChainHelpers.getValue_In_TxOut_And_Datum txOut_And_Datum, OnChainHelpers.getDatum_In_TxOut_And_Datum txOut_And_Datum)  | !txOut_And_Datum <- inputs_Own_TxOuts_And_FundHoldingDatums]
                                                !outputs_Values_And_FundHoldingDatums =  [(OnChainHelpers.getValue_In_TxOut_And_Datum txOut_And_Datum, OnChainHelpers.getDatum_In_TxOut_And_Datum txOut_And_Datum)  | !txOut_And_Datum <- outputs_Own_TxOuts_And_FundHoldingDatums]
                                                ------------------
                                                !fundHoldingDatums_In = snd <$> inputs_Values_And_FundHoldingDatums
                                                !fundHoldingDatums_Out = snd <$> outputs_Values_And_FundHoldingDatums
                                                ------------------
                                                !valueOf_fundHoldingDatums_In = fst <$> inputs_Values_And_FundHoldingDatums
                                                !valueOf_fundHoldingDatums_Out = fst <$> outputs_Values_And_FundHoldingDatums
                                                ------------------
                                                isCorrect_Outputs_Commissions_SameTotal :: Bool
                                                !isCorrect_Outputs_Commissions_SameTotal = sum alterCommissionsFT == 0
                                                    -- let
                                                    --     sumAndSubtractTotal :: Integer -> (Integer, Integer) -> Integer
                                                    --     sumAndSubtractTotal !accValue (!inValue, !outValue) =
                                                    --         ((accValue + inValue) - outValue)
                                                    --     -- Perform the fold to sum and subtract
                                                    --     !finalValue = foldl sumAndSubtractTotal 0 (zip (T.hdSubtotal_FT_Commissions <$> fundHoldingDatums_In) (T.hdSubtotal_FT_Commissions <$> fundHoldingDatums_Out))
                                                    -- in
                                                    --     -- Check that the final accumulated value is zero for all tokens
                                                    --     finalValue == 0
                                                ------------------
                                                isCorrect_Outputs_FundHoldingDatums :: Bool
                                                !isCorrect_Outputs_FundHoldingDatums =
                                                    -- let
                                                    --     ------------------
                                                    --     -- no los ordeno, se supone vienen ordenados en la tx
                                                    --     -- fundHoldingDatums_In_Sorted = sort (snd <$> inputs_Values_And_FundHoldingDatums)
                                                    --     -- fundHoldingDatums_Out_Sorted = sort (snd <$> outputs_Values_And_FundHoldingDatums)
                                                    --     ------------------
                                                    --     -- !fundHoldingDatums_In = snd <$> inputs_Values_And_FundHoldingDatums
                                                    --     -- !fundHoldingDatums_Out = snd <$> outputs_Values_And_FundHoldingDatums
                                                    --     ------------------
                                                    --     fundHoldingDatums_Control = zipWith3 FundHelpers.mkUpdated_FundHolding_Datum_With_CommissionsMoved 
                                                    --                                 fundHoldingDatums_In 
                                                    --                                 fundHoldingDatums_Out 
                                                    --                                 rbCommissionsFT
                                                    -- in
                                                    --     all (uncurry OnChainHelpers.isUnsafeEqDatums) (zip fundHoldingDatums_Control fundHoldingDatums_Out)
                                                    let
                                                        {- 
                                                            Validate Fund Holding Datum updates:
                                                            1. Create control datum using input datum, new commission (commissionFT), and new rate from output datum
                                                            2. Check if control datum matches output datum
                                                            3. Verify proportional change in commissions and rates:
                                                               - If old commission was 0, accept any new rate
                                                               - Otherwise, ensure (newCommissions * oldRate) == (oldCommissions * newRate)
                                                            4. Both checks (datum equality and proportional change) must pass for validation to succeed
                                                            Note: This allows initialization of empty datums and maintains proportion for non-zero cases
                                                        -}
                                                        validateDatum :: T.FundHoldingDatumType -> T.FundHoldingDatumType -> Integer -> Bool
                                                        validateDatum !datumIn !datumOut !alter_Commission_FT =
                                                            let
                                                                ------------------
                                                                !oldCommissions = T.hdSubtotal_FT_Commissions datumIn
                                                                !newCommissions = oldCommissions + alter_Commission_FT  -- Use the parameter value directly
                                                                !oldRate = T.hdSubtotal_FT_Commissions_Rate1e6_PerMonth datumIn
                                                                !newRate = T.hdSubtotal_FT_Commissions_Rate1e6_PerMonth datumOut  -- Use the rate from datumOut
                                                                ------------------
                                                                !datumControl = FundHelpers.mkUpdated_FundHolding_Datum_With_CommissionsMoved datumIn newCommissions newRate
                                                                ------------------
                                                                isProportionalChange = (oldCommissions == 0) || ((newCommissions * oldRate) == (oldCommissions * newRate))
                                                            in
                                                                OnChainHelpers.isUnsafeEqDatums datumControl datumOut && isProportionalChange
                                                        validateAll :: [T.FundHoldingDatumType] -> [T.FundHoldingDatumType] -> [Integer] -> Bool
                                                        validateAll [] [] [] = True
                                                        validateAll (din:dis) (dout:dos) (c:cs) = validateDatum din dout c && validateAll dis dos cs
                                                        validateAll _ _ _ = False
                                                    in
                                                        validateAll fundHoldingDatums_In fundHoldingDatums_Out alterCommissionsFT
                                                ------------------
                                                -- !valueOf_FundHoldingDatums_In = foldl (<>) (LedgerAda.lovelaceValueOf 0) $ fst <$> inputs_Values_And_FundHoldingDatums
                                                -- !valueOf_FundHoldingDatums_Out = foldl (<>) (LedgerAda.lovelaceValueOf 0) $ fst <$> outputs_Values_And_FundHoldingDatums
                                                ------------------
                                                -- isCorrect_Output_FundHolding_Datums_Values_SameTotal :: Bool
                                                -- !isCorrect_Output_FundHolding_Datums_Values_SameTotal = valueOf_FundHoldingDatums_In == valueOf_FundHoldingDatums_Out
                                                ------------------
                                                isCorrect_Output_FundHolding_Values_SameTotal :: Bool
                                                !isCorrect_Output_FundHolding_Values_SameTotal =
                                                    let
                                                        -- Helper function to combine token values by summing input and subtracting output
                                                        sumAndSubtractTokens :: LedgerApiV2.Value -> (LedgerApiV2.Value, LedgerApiV2.Value) -> LedgerApiV2.Value
                                                        sumAndSubtractTokens !accValue (!inValue, !outValue) =
                                                            (accValue <> inValue <> negate outValue)
                                                        -- Perform the fold to sum and subtract token values pairwise
                                                        !finalValue = foldl sumAndSubtractTokens (LedgerAda.lovelaceValueOf 0) (zip valueOf_fundHoldingDatums_In valueOf_fundHoldingDatums_Out)
                                                    in
                                                        -- Check that the final accumulated value is zero for all tokens
                                                        finalValue `OnChainHelpers.isEqValue` LedgerAda.lovelaceValueOf 0
                                                ------------------
                                                isCorrect_Output_FundHolding_Values :: Bool
                                                !isCorrect_Output_FundHolding_Values =
                                                        all checkOutput outputs_Values_And_FundHoldingDatums
                                                    where
                                                        checkOutput :: (LedgerValue.Value, T.FundHoldingDatumType) -> Bool
                                                        checkOutput (!valueOf_FundHoldingDatum_Out', !fundHoldingDatum_Out') =
                                                            ------------------
                                                            let
                                                                !minADA = T.hdMinADA fundHoldingDatum_Out'
                                                                ------------------
                                                                !commissionsFT = T.hdSubtotal_FT_Commissions fundHoldingDatum_Out'
                                                                ------------------
                                                                !fundHolding_Index' = T.hdFundHolding_Index fundHoldingDatum_Out'
                                                                !fundHoldingID_TN' = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index'
                                                                !fundHoldingID_AC'= LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN')
                                                                ------------------
                                                            in  OnChainHelpers.isNFT_With_AC_InValue valueOf_FundHoldingDatum_Out' fundHoldingID_AC'
                                                                -- min ADA tiene que ser al menos lo que marca el datum, puede ser más, en el caso de que se haya usado ADA para la InvestUnit
                                                                && OnChainHelpers.getValueOfLovelace valueOf_FundHoldingDatum_Out' >= minADA
                                                                && OnChainHelpers.getAmt_With_AC_InValue valueOf_FundHoldingDatum_Out' fundFT_AC == commissionsFT
                                        _ -> False
                                    ------------------
                                    validateAllButBalance :: Bool
                                    validateAllButBalance
                                        ------------------
                                        | redeemerType == redeemerDepositAndWithdraw = validateDepositAndWithdraw
                                        | redeemerType == redeemerReIdx = validateReIdx
                                        | redeemerType == redeemerCommissions || redeemerType ==  redeemerUpdateMinADA = validateCommissionsAndUpdateMinADA
                                        | otherwise = False
                                        ------------------
                                        where
                                            ------------------
                                            !output_Own_TxOut_And_FundHoldingDatum =
                                                fromMaybe
                                                    (traceError "Expected FundHolding at output index 0")
                                                    (OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                                    @T.ValidatorDatum @T.FundHoldingDatumType
                                                    ctx
                                                    outputs_txOuts_index0
                                                    fundHoldingID_AC
                                                    (Just fundHolding_Validator_Address)
                                                    T.getFundHolding_DatumType)
                                            -------------------
                                            !fundHoldingDatum_In = T.getFundHolding_DatumType datum
                                            ------------------
                                            !fundHolding_Index = T.hdFundHolding_Index fundHoldingDatum_In
                                            !fundHoldingID_TN = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index
                                            !fundHoldingID_AC = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN)
                                            ------------------
                                            !valueOf_FundHoldingDatum_In = LedgerApiV2.txOutValue input_TxOut_BeingValidated
                                            ------------------
                                            !fundHoldingDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
                                            -------------------
                                            !valueOf_FundHoldingDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
                                            -------------------
                                            validateDepositAndWithdraw :: Bool
                                            validateDepositAndWithdraw =
                                                case redeemer of
                                                    (T.ValidatorRedeemerDeposit (T.ValidatorRedeemerDepositType !date !deposit)) ->
                                                            ------------------
                                                            -- it runs along with Fund Policy (PolicyRedeemerMintFT)
                                                                -- traceIfFalse "not isFundOpen" isFundOpen
                                                                -- traceIfFalse "not isMintingFT" isMintingFT &&
                                                                -- traceIfFalse "not isCorrect_Redeemer_FundHolding" (isCorrect_Redeemer_FundHolding isFundHoldingValidatorRedeemerDeposit)
                                                            ------------------
                                                            traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                            && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                                                            && traceIfFalse "not Correct Deposit Amount" (isCorrectAmount deposit T.maxDepositAndWithdrawInFunds)
                                                            && traceIfFalse "not isMintingFT" isMintingFT
                                                            && traceIfFalse "not isCorrect_Output_FundHolding_Datum_With_Deposit" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_Deposit)
                                                            && traceIfFalse "not isCorrect_Output_FundHolding_Value_With_Tokens_And_FT" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out valueFor_FundHoldingDatum_Control_With_Tokens_And_FT)
                                                        where
                                                            ------------------
                                                            !deadline = FundT.fdDeadline fundDatum_In
                                                            !commissionsTable_Numerator1e6 = FundT.fdCommissionsTable_Numerator1e6 fundDatum_In
                                                            ---------------------
                                                            !(userFT, commissionsFT, commissions_FT_Rate1e6_PerMonth) =
                                                                FundHelpers.calculateDepositCommissionsUsingMonths commissionsTable_Numerator1e6 deadline date deposit
                                                            ------------------
                                                            !valueOf_TokensForDeposit_Plus_FundHoldingDatum_Value = createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value valueOf_FundHoldingDatum_In deposit True
                                                            ------------------
                                                            !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC commissionsFT
                                                            ------------------
                                                            !valueFor_FundHoldingDatum_Control_With_Tokens_And_FT = valueOf_TokensForDeposit_Plus_FundHoldingDatum_Value <> valueFor_FT_Commissions
                                                            ------------------
                                                            !fundHoldingDatum_Control_With_Deposit = FundHelpers.mkUpdated_FundHolding_Datum_With_Deposit fundHoldingDatum_In deposit userFT commissionsFT commissions_FT_Rate1e6_PerMonth
                                                            ------------------
                                                            isMintingFT :: Bool
                                                            !isMintingFT = OnChainHelpers.isOnlyToken_Minting_With_AC_AndAmt fundFT_AC deposit info
                                                            ------------------
                                                    (T.ValidatorRedeemerWithdraw (T.ValidatorRedeemerWithdrawType !date !withdraw !withdrawPlusComissions)) ->
                                                            ------------------
                                                            -- it runs along with Fund Policy (PolicyRedeemerBurnFT)
                                                                -- traceIfFalse "not isBurningFT" isBurningFT &&
                                                                -- traceIfFalse "not isCorrect_Redeemer_FundHolding" (isCorrect_Redeemer_FundHolding isFundHoldingValidatorRedeemerWithdraw)
                                                            ------------------
                                                            -- there are no temporal restritions for withdraw
                                                            ------------------
                                                            traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                            && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                                                            && traceIfFalse "not Correct Withdraw Amount" (isCorrectAmount withdraw T.maxDepositAndWithdrawInFunds)
                                                            && traceIfFalse "not Correct Comissions" (isCorrectCommissionsAmount commissionsForUserFT commissionsForUserFT_calculated)
                                                            && traceIfFalse "not isEnough_FT_ForComission" isEnough_FT_ForComission
                                                            && traceIfFalse "not isEnough_Commissions_RatePerMonth" isEnough_Commissions_RatePerMonth
                                                            && traceIfFalse "not isBurningFT" isBurningFT
                                                            && traceIfFalse "not isCorrect_Output_FundHolding_Datum_With_Withdraw" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_Withdraw)
                                                            && traceIfFalse "not isCorrect_Output_FundHolding_Value_Without_Tokens_And_FT_for_Commissions" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out valueFor_FundHoldingDatum_ControlWithout_Tokens_And_FT_for_Commissions)
                                                            ------------------
                                                        where
                                                            ---------------------
                                                            !commissionsForUserFT = withdrawPlusComissions - withdraw
                                                            ---------------------
                                                            !deadline = FundT.fdDeadline fundDatum_In
                                                            !commissionsTable_Numerator1e6 = FundT.fdCommissionsTable_Numerator1e6 fundDatum_In
                                                            ---------------------
                                                            !commissionsForUserFT_calculated = FundHelpers.calculateWithdrawCommissionsAvailable commissionsTable_Numerator1e6 deadline date withdraw investUnit_Granularity
                                                            !commissions_FT_Rate1e6_PerMonth_calculated = FundHelpers.calculateWithdrawCommissionsRate deadline date commissionsForUserFT
                                                            ------------------
                                                            !valueOf_TokensForWithdraw_Plus_FundHoldingDatum_Value = createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value valueOf_FundHoldingDatum_In (negate withdrawPlusComissions) False
                                                            !valueFor_FT_CommissionsToGetBack = LedgerValue.assetClassValue fundFT_AC commissionsForUserFT
                                                            !valueFor_FundHoldingDatum_ControlWithout_Tokens_And_FT_for_Commissions = valueOf_TokensForWithdraw_Plus_FundHoldingDatum_Value <> negate valueFor_FT_CommissionsToGetBack
                                                            ------------------
                                                            !fundHoldingDatum_Control_With_Withdraw  = FundHelpers.mkUpdated_FundHolding_Datum_With_Withdraw fundHoldingDatum_In withdraw commissionsForUserFT commissions_FT_Rate1e6_PerMonth_calculated
                                                            ------------------
                                                            isEnough_FT_ForComission :: Bool
                                                            !isEnough_FT_ForComission = T.hdSubtotal_FT_Commissions fundHoldingDatum_In >= commissionsForUserFT
                                                            isEnough_Commissions_RatePerMonth :: Bool
                                                            !isEnough_Commissions_RatePerMonth = T.hdSubtotal_FT_Commissions_Rate1e6_PerMonth fundHoldingDatum_In >= commissions_FT_Rate1e6_PerMonth_calculated
                                                            ------------------
                                                            isBurningFT :: Bool
                                                            !isBurningFT = OnChainHelpers.isOnlyToken_Burning_With_AC_AndAmt fundFT_AC (negate withdrawPlusComissions) info
                                                            ------------------
                                                    _ -> False
                                                ------------------
                                                where
                                                    ------------------
                                                    !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
                                                    ------------------
                                                    !inputRef_TxOut_And_InvestUnitDatum =
                                                        case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                                                @InvestUnitT.ValidatorDatum @InvestUnitT.InvestUnitDatumType
                                                                ctx
                                                                inputsRef_TxOuts
                                                                investUnitID_AC
                                                                InvestUnitT.getInvestUnit_DatumType of
                                                            [x] -> x
                                                            _   -> traceError "Expected exactly one InvestUnit input ref"
                                                    ------------------
                                                    !investUnitDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_InvestUnitDatum
                                                    ------------------
                                                    !investUnit = InvestUnitT.iudInvestUnit investUnitDatum_In
                                                    !investUnitTokens = T.iuValues investUnit
                                                    ------------------
                                                    !investUnit_Granularity = getDecimalsInInvestUnit investUnitTokens
                                                    ------------------
                                                    {-
                                                    Function: isCorrectAmount

                                                    Purpose:
                                                    Validates deposit or withdrawal amounts based on predefined rules and invest unit granularity.

                                                    Parameters:
                                                    - amount: The deposit or withdrawal amount to validate.

                                                    Checks:
                                                    1. Amount is positive.
                                                    2. Amount doesn't exceed maximum allowed for deposits/withdrawals.
                                                    3. Amount is divisible by the smallest unit of granularity in the invest unit.

                                                    Example:
                                                    For an invest unit with tokens [455, 1000] (representing 4.55 and 10.00):
                                                    - Valid amount: 200
                                                        Because:
                                                        (200 * 455) /100 = 910 (whole number)
                                                        (200 * 1000) /100 = 2000 (whole number)
                                                    - Invalid amount: 157
                                                        Because:
                                                        (157 * 455) /100 = 714.35 (fractional, not allowed)
                                                        (157 * 1000) /100 = 1570 (whole number, but the other token fails)

                                                    The function getDecimalsInInvestUnit would return 100 for this invest unit,
                                                    because the smallest fraction is 0.01 (represented as 1 in the 455).
                                                    isCorrectAmount then checks if the amount is divisible by 100, ensuring
                                                    it can be evenly distributed across all tokens without creating fractions.

                                                    In this case:
                                                    200 is valid because 200 % 100 == 0
                                                    157 is invalid because 157 % 100 != 0
                                                    -}
                                                    ------------------
                                                    isCorrectAmount :: Integer -> Integer -> Bool
                                                    isCorrectAmount !amount !max' =
                                                            amount > 0
                                                            && (amount <= max')
                                                            && amount `remainder` investUnit_Granularity == 0
                                                    ------------------
                                                    isCorrectCommissionsAmount :: Integer -> Integer -> Bool
                                                    isCorrectCommissionsAmount !amount !max' =
                                                            amount >= 0
                                                            && (amount <= max')
                                                            && amount `remainder` investUnit_Granularity == 0
                                                    ------------------
                                                    {-
                                                    Function: getDecimalsInInvestUnit

                                                    Purpose:
                                                    Determines the smallest fraction (highest precision) used in the invest unit.

                                                    Parameters:
                                                    - tokens: List of InvestUnitTokens to analyze.

                                                    Process:
                                                    1. Iterates through all tokens in the invest unit.
                                                    2. For each token, checks if its amount (which is multiplied by 100) is divisible by 100, 10, or neither.
                                                    3. Returns the smallest divisor found:
                                                    - 100 if any token has a fraction (two decimal places)
                                                    - 10 if the smallest fraction is 0.1 (one decimal place)
                                                    - 1 if all tokens are whole numbers (no decimals)

                                                    Importance:
                                                    This function is crucial for ensuring that deposit/withdrawal amounts are compatible
                                                    with the invest unit's highest precision, which in turn guarantees exact divisions
                                                    in token amount calculations.

                                                    Examples:

                                                    1. Two decimal places:
                                                    Invest unit tokens: [455, 1000, 10000]
                                                    - 455 represents 4.55 (two decimal places)
                                                    - 1000 represents 10.00 (whole number)
                                                    - 10000 represents 100.00 (whole number)
                                                    Result: Returns 100
                                                    Explanation: The smallest fraction is 0.01 (in 4.55), so amounts must be divisible by 100.

                                                    2. One decimal place:
                                                    Invest unit tokens: [150, 1000, 2050]
                                                    - 150 represents 1.5 (one decimal place)
                                                    - 1000 represents 10.0 (whole number)
                                                    - 2050 represents 20.5 (one decimal place)
                                                    Result: Returns 10
                                                    Explanation: The smallest fraction is 0.1, so amounts must be divisible by 10.

                                                    3. No decimal places:
                                                    Invest unit tokens: [100, 1000, 5000]
                                                    - 100 represents 1 (whole number)
                                                    - 1000 represents 10 (whole number)
                                                    - 5000 represents 50 (whole number)
                                                    Result: Returns 1
                                                    Explanation: All numbers are whole, so amounts only need to be divisible by 1.

                                                    4. Mixed precision:
                                                    Invest unit tokens: [455, 150, 1000]
                                                    - 455 represents 4.55 (two decimal places)
                                                    - 150 represents 1.5 (one decimal place)
                                                    - 1000 represents 10 (whole number)
                                                    Result: Returns 100
                                                    Explanation: The highest precision is two decimal places (0.01 in 4.55),
                                                                    so amounts must be divisible by 100 to work with all tokens.

                                                    Usage in isCorrectAmount:
                                                    If getDecimalsInInvestUnit returns 100, then isCorrectAmount will check if
                                                    the deposit/withdrawal amount is divisible by 100. This ensures that the amount
                                                    can be evenly distributed across all tokens, including those with the highest precision.

                                                    For example, with the result 100:
                                                    - 200 is a valid amount (200 % 100 == 0)
                                                    - 1100 is a valid amount (1100 % 100 == 0)
                                                    - 155 is not a valid amount (155 % 100 != 0)
                                                    -}
                                                    ------------------
                                                    getDecimalsInInvestUnit :: [T.InvestUnitToken] -> Integer
                                                    getDecimalsInInvestUnit !tokens = go tokens 1
                                                        where
                                                            ------------------
                                                            go [] !acc = acc
                                                            go ((_, _, !amount):xs) acc =
                                                                let !accNew = max acc (dividedBy amount)
                                                                in go xs accNew
                                                            ------------------
                                                            dividedBy !amount
                                                                | amount `remainder` 100 == 0 = 1
                                                                | amount `remainder` 10 == 0 = 10
                                                                | otherwise = 100
                                                    ------------------
                                                    {-
                                                    Overall Function: createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value

                                                    Purpose:
                                                    Calculates the new value for a FundHolding after a deposit or withdrawal operation,
                                                    considering the current value and the invest unit definition.

                                                    Key Points:
                                                    1. Invest Unit amounts are stored multiplied by 100 to handle two decimal places.
                                                    Example: 4.55 tokens are stored as 455 in the invest unit.
                                                    2. Processes each currency symbol (CS) and its associated tokens.
                                                    3. Respects the granularity defined in the invest unit.
                                                    4. Uses a round-up approach for robustness, despite guaranteed exactness due to granularity checks.

                                                    Process:
                                                    1. Converts input Value to a list of currency symbols and token maps.
                                                    2. Updates or adds tokens based on the invest unit and deposit/withdrawal amount.
                                                    3. Ensures accurate token amount calculations.
                                                    4. Returns a new Value with updated token amounts.

                                                    Example:
                                                    For an invest unit with token A = 455 (4.55) and a deposit of 1000:
                                                    New amount of token A = (455 * 1000) / 100 = 4550
                                                    -}
                                                    createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value :: LedgerValue.Value -> Integer -> Bool -> LedgerValue.Value
                                                    createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value (LedgerValue.Value !mp) !amount !swRoundUp =
                                                        LedgerValue.Value mapCSResult
                                                        where
                                                            !listMapCS = TxAssocMap.toList mp
                                                            !listTokens =  investUnitTokens
                                                            !mapCSResult = TxAssocMap.fromList (updateListMapCS listTokens listMapCS)
                                                            ------------------
                                                            updateListMapCS [] !restListMapCS = restListMapCS
                                                            updateListMapCS ((!cs, !tn, !amt): restTokens) restListMapCS  =
                                                                let
                                                                    !(tokensFromSameCS, restTokensWithoutCS) = getOthersTokensFromSameCSAndDeleteFromList cs restTokens [] []
                                                                    !(mapFromSameCS, restMapWithoutCS) = getMapFromSameCSAndDeleteFromList cs restListMapCS []
                                                                    !mapFromSameCSWithTokensAdded = addTokensInMap cs mapFromSameCS ((tn, amt):tokensFromSameCS)
                                                                    !resultMap = mapFromSameCSWithTokensAdded : updateListMapCS restTokensWithoutCS restMapWithoutCS
                                                                in resultMap
                                                            ------------------
                                                            getOthersTokensFromSameCSAndDeleteFromList _ [] !accListFromSame !accListOthers = (accListFromSame, accListOthers)
                                                            getOthersTokensFromSameCSAndDeleteFromList !cs ((!cs', !tn', !amt'): restTokens) accListFromSame accListOthers
                                                                | cs == cs' = getOthersTokensFromSameCSAndDeleteFromList cs restTokens ( (tn', amt'): accListFromSame) accListOthers
                                                                | otherwise = getOthersTokensFromSameCSAndDeleteFromList cs restTokens accListFromSame ( (cs', tn', amt'): accListOthers)
                                                            ------------------
                                                            getMapFromSameCSAndDeleteFromList _ [] !accListMapOthers = (Nothing, accListMapOthers)
                                                            getMapFromSameCSAndDeleteFromList !cs ((!cs', !mapTN): restMap) accListOthers
                                                                | cs == cs' = (Just mapTN, restMap ++ accListOthers)
                                                                | otherwise = getMapFromSameCSAndDeleteFromList cs restMap ( (cs', mapTN): accListOthers)
                                                            ------------------
                                                            addTokensInMap !cs Nothing !tokens      = addTokensInMap' cs TxAssocMap.empty tokens
                                                            addTokensInMap !cs (Just !mapTN) !tokens = addTokensInMap' cs mapTN tokens
                                                            ------------------
                                                        {-
                                                            Function: addTokensInMap'

                                                            Purpose:
                                                            Adds or updates token amounts in a map based on invest unit definition and deposit/withdrawal amount.

                                                            Parameters:
                                                            - cs: Currency Symbol
                                                            - mapTN: Existing map of TokenName to amounts
                                                            - listTokensToAddInMap: List of tokens from the invest unit to process

                                                            Process:
                                                            1. Iterates through each token in the invest unit.
                                                            2. Calculates new amount: (invest_unit_amount * deposit_amount) / 100
                                                            Note: invest_unit_amount is already multiplied by 100, so this division brings it back to the correct scale.
                                                            3. Updates the map with new amounts, adding new entries or updating existing ones.

                                                            Note on Calculation:
                                                            Despite granularity check ensuring exact divisions, we still use multiply_By_Scaled_1e2_And_RoundUp
                                                            with round-up behavior as a safety measure

                                                            Example:
                                                            For an invest unit token of 455 (4.55) and a deposit of 1000:
                                                            New amount = (455 * 1000) / 100 = 4550
                                                            This would typically be exact, but the function would round up if there were any remainder.
                                                            -}
                                                            addTokensInMap' :: LedgerApiV2.CurrencySymbol -> LedgerApiV2.Map LedgerApiV2.TokenName Integer -> [(LedgerApiV2.TokenName, Integer)] -> (LedgerApiV2.CurrencySymbol, LedgerApiV2.Map LedgerApiV2.TokenName Integer)
                                                            addTokensInMap' !cs !mapTN []                   = (cs, mapTN)
                                                            addTokensInMap' !cs !mapTN !listTokensToAddInMap =
                                                                (cs, foldl (\acc (!tn', !amt') ->
                                                                    let
                                                                        !roundedAmt =
                                                                            -- NOTE: en depositos esta bien que redondee para arriba, en retiros redondea para abajo
                                                                            if swRoundUp then
                                                                                OnChainHelpers.multiply_By_Scaled_1e2_And_RoundUp amount amt'
                                                                            else
                                                                                OnChainHelpers.multiply_By_Scaled_1e2_And_RoundDown amount amt'
                                                                    in mapElement acc tn' roundedAmt
                                                                ) mapTN listTokensToAddInMap)
                                                            mapElement !acc !tn !amt =
                                                                case TxAssocMap.lookup tn acc of
                                                                    Nothing   -> TxAssocMap.insert tn amt acc
                                                                    Just !amt' -> TxAssocMap.insert tn (amt + amt') acc
                                            ------------------
                                            validateReIdx :: Bool
                                            validateReIdx =  case redeemer of
                                                (T.ValidatorRedeemerReIndexing (T.ValidatorRedeemerReIndexingType (T.InvestUnit !tokensToAdd) (T.InvestUnit !tokensToRemove))) ->
                                                        ------------------
                                                        -- it runs along with Invest Unit Validator (ValidatorRedeemerReIndexing)
                                                            -- validateAdminAction &&
                                                            -- && traceIfFalse "not isCorrect_Oracle_Signature" isCorrect_Oracle_Signature
                                                            -- && traceIfFalse "not isCorrect_Oracle_InRangeTime" isCorrect_Oracle_InRangeTime
                                                            -- && traceIfFalse "not isCorrect_Exchange_WithSamePriceADA" isCorrect_Exchange_WithSamePriceADA
                                                            -- && traceIfFalse "not isCorrect_Output_InvestUnit_Datum_WithTokensExchanged" isCorrect_Output_InvestUnit_Datum_WithTokensExchanged
                                                            -- && traceIfFalse "not isCorrect_Output_InvestDatum_Value_NotChanged" isCorrect_Output_InvestDatum_Value_NotChanged
                                                            -- && traceIfFalse "not isCorrect_Redeemer_FundHolding" isCorrect_Redeemer_FundHolding
                                                        ------------------
                                                        traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                        && traceIfFalse "not isCorrect_ReIdx_Amounts_And_Valid_Divisibility " isCorrect_ReIdx_Amounts_And_Valid_Divisibility
                                                        && traceIfFalse "not isCorrect_Output_FundHolding_Datum_NotChanged" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_NotChanged)
                                                        && traceIfFalse "not isCorrect_Output_FundHolding_Value_WithTokensExchanged" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out valueFor_FundHoldingDatum_Control_WithTokensExchanged)
                                                        && traceIfFalse "not isCorrect_Redeemer_InvestUnit" isCorrect_Redeemer_InvestUnit
                                                        ------------------
                                                    where
                                                        ------------------
                                                        !inputs_TxOutRefs_TxOuts = [(LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | !txInfoInput <- LedgerApiV2.txInfoInputs info,
                                                                OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput )]
                                                        ------------------
                                                        !inputsRef_TxOuts_And_FundHoldingDatums' =
                                                            OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
                                                                @T.ValidatorDatum @T.FundHoldingDatumType
                                                                ctx
                                                                inputsRef_TxOuts
                                                                fundHoldingPolicyID_CS
                                                                T.getFundHolding_DatumType
                                                        ------------------
                                                        !fundHoldingsCount = FundT.fdHoldingsCount fundDatum_In
                                                        ------------------
                                                        !allFundHoldingsIndex =  T.hdFundHolding_Index fundHoldingDatum_In : [ T.hdFundHolding_Index  dat | (_, !dat) <- inputsRef_TxOuts_And_FundHoldingDatums']
                                                        !countDistinctFundHoldingsIndex = OnChainHelpers.countDistinct allFundHoldingsIndex
                                                        ------------------
                                                        !inputsRef_TxOuts_And_FundHoldingDatums =
                                                            if countDistinctFundHoldingsIndex == fundHoldingsCount
                                                                then inputsRef_TxOuts_And_FundHoldingDatums'
                                                                else traceError "expected all but one FundHolding as input ref"
                                                        ------------------
                                                        !total_Deposits_IU = T.hdSubtotal_FT_Minted fundHoldingDatum_In + sum (T.hdSubtotal_FT_Minted . OnChainHelpers.getDatum_In_TxOut_And_Datum <$> inputsRef_TxOuts_And_FundHoldingDatums)
                                                        ------------------
                                                        --amount is multiplied by 100 to have more accuracy, but must be divided by 100 to use it
                                                        !valueOf_TotalTokensToAdd = OnChainHelpers.flattenValueToValue [(cs, tn, (am * total_Deposits_IU) `divide` 100) | (!cs, !tn, !am) <- tokensToAdd ]
                                                        !valueOf_TotalTokensToRemove = OnChainHelpers.flattenValueToValue [(cs, tn, (am * total_Deposits_IU) `divide` 100) | (!cs, !tn, !am) <- tokensToRemove  ]
                                                        ------------------
                                                        !valueFor_FundHoldingDatum_Control_WithTokensExchanged  = valueOf_FundHoldingDatum_In <> valueOf_TotalTokensToAdd <> negate valueOf_TotalTokensToRemove
                                                        ------------------
                                                        !fundHoldingDatum_Control_NotChanged = fundHoldingDatum_In
                                                        ------------------
                                                        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
                                                        ------------------
                                                        !input_TxOutRef_TxOut_And_InvestUnitDatum =
                                                            case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC
                                                                @InvestUnitT.ValidatorDatum @InvestUnitT.InvestUnitDatumType
                                                                ctx
                                                                inputs_TxOutRefs_TxOuts
                                                                investUnitID_AC
                                                                InvestUnitT.getInvestUnit_DatumType of
                                                                [x] -> x
                                                                _   -> traceError "Expected exactly one InvestUnit input"
                                                        ------------------
                                                        isCorrect_ReIdx_Amounts_And_Valid_Divisibility :: Bool
                                                        isCorrect_ReIdx_Amounts_And_Valid_Divisibility =
                                                            all (\(_, _, !am) -> am > 0 && (am * total_Deposits_IU) `remainder` 100  == 0  )  tokensToAdd
                                                            && all (\(_, _, !am) -> am > 0 && (am * total_Deposits_IU) `remainder` 100  == 0  )  tokensToRemove
                                                        ------------------
                                                        -- TODO: no hace falta comprobar redeemerFor_InvestUnitDatum, solo controlo que haya alguna input de invest unit
                                                        -- eso significa que es consumida y que el validador de InvestUnit se va a ejecutar. El validador solo acepta un redeemer
                                                        -- en ese validador esta la logica que comprueba que los valores de token to add y remove del redeemer de invet unit y de FundHolding son iguales
                                                        ------------------
                                                        isCorrect_Redeemer_InvestUnit :: Bool
                                                        !isCorrect_Redeemer_InvestUnit  =
                                                            let !redeemerFor_InvestUnitDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(!txOutRef, _, _) -> txOutRef ) input_TxOutRef_TxOut_And_InvestUnitDatum) info
                                                            in  case redeemerFor_InvestUnitDatum' of
                                                                        Nothing -> False
                                                                        Just !redeemerFor_InvestUnitDatum ->
                                                                            case LedgerApiV2.fromBuiltinData @InvestUnitT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_InvestUnitDatum of
                                                                                Just (InvestUnitT.ValidatorRedeemerReIndexing (InvestUnitT.ValidatorRedeemerReIndexingType !riuriTokensToAdd !riuriTokensToRemove _ _)) ->
                                                                                    T.InvestUnit tokensToAdd == riuriTokensToAdd && T.InvestUnit tokensToRemove == riuriTokensToRemove
                                                                                _ -> False
                                                    ------------------
                                                _ -> False
                                            ------------------
                                            validateCommissionsAndUpdateMinADA :: Bool
                                            validateCommissionsAndUpdateMinADA
                                                    ------------------
                                                    | redeemerType == redeemerCommissions = validateCommissions
                                                    | otherwise = validateUpdateMinADA
                                                    ------------------
                                                where
                                                    ------------------
                                                    validateUpdateMinADA :: Bool
                                                    validateUpdateMinADA =
                                                            ---------------------
                                                            -- it runs alone
                                                            ---------------------
                                                            -- Que sea Fund Admin
                                                            -- Que el FundHoldingDatum regrese a FundHolding Val (se hace automaticamente al buscar outputs en same address)
                                                            -- Que el FundHoldingDatum se actualiza correctamente
                                                            -- Que el FundHoldingDatum value cambie con el min ADA nuevo
                                                            -- no hay restricciones temporales
                                                            ------------------
                                                            validateFundAdminAction fundDatum_In
                                                            && traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                            && traceIfFalse "not min ADA > 0" (newMinADA > 0)
                                                            && traceIfFalse "not isCorrect_Output_FundHolding_Datum_UpdatedMinADA" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_UpdatedMinADA)
                                                            && traceIfFalse "not isCorrect_Output_FundHolding_Value_With_MinADAChanged" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out valueFor_FundHoldingDatum_Control)
                                                        where
                                                            ------------------
                                                            !newMinADA = T.hdMinADA fundHoldingDatum_Out
                                                            ------------------
                                                            !fundHoldingDatum_Control_With_UpdatedMinADA = FundHelpers.mkUpdated_FundHolding_Datum_With_MinADAChanged
                                                                        fundHoldingDatum_In
                                                                        newMinADA
                                                            ------------------
                                                            !valueFor_FundHoldingDatum_Control = valueOf_FundHoldingDatum_In <> LedgerAda.lovelaceValueOf (newMinADA - T.hdMinADA fundHoldingDatum_In)
                                                            ------------------
                                                    ------------------
                                                    validateCommissions :: Bool
                                                    validateCommissions =
                                                        case redeemer of
                                                            (T.ValidatorRedeemerCollect_Protocol_Commission (T.ValidatorRedeemerCollect_Protocol_CommissionType !date !withdraw ) ) ->
                                                                    ---------------------
                                                                    -- it runs alone
                                                                    ---------------------
                                                                    validateAdminAction admins tokenAdmin_AC
                                                                    && traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                                    && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                                                                    && traceIfFalse "not withdraw > 0" (withdraw > 0)
                                                                    && traceIfFalse "not isCommissionsAvailable" (isCommissionsAvailable (getAvailable getShare getTaken date) withdraw)
                                                                    && traceIfFalse "not isCorrect_Output_FundHolding_Datum_With_Collect_Protocol_Commission" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_Collect_Protocol_Commission)
                                                                    && traceIfFalse "not isCorrect_Output_FundHolding_Value_Without_FT_for_Commissions" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out $ getValueFor_FundHoldingDatum_Control_Without_FT_for_Commissions withdraw)
                                                                    ------------------
                                                                where
                                                                    ------------------
                                                                    !getShare = ProtocolT.pdShare_InBPx1e2_Protocol
                                                                    !getTaken = T.hdSubtotal_FT_Commissions_Collected_Protocol
                                                                    ------------------
                                                                    !fundHoldingDatum_Control_With_Collect_Protocol_Commission = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission fundHoldingDatum_In withdraw
                                                                    ------------------
                                                                    !admins = T.getAdmins protocolDatum_In
                                                                    !tokenAdminPolicy_CS = T.getAdminToken_CS protocolDatum_In
                                                                    !tokenAdmin_AC = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.protocolTokenAdmin_TN)
                                                                    ------------------
                                                            (T.ValidatorRedeemerCollect_Managers_Commission (T.ValidatorRedeemerCollect_Managers_CommissionType !date !withdraw)) ->
                                                                    ---------------------
                                                                    -- it runs alone
                                                                    ---------------------
                                                                    validateFundAdminAction fundDatum_In
                                                                    && traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                                    && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                                                                    && traceIfFalse "not withdraw > 0" (withdraw > 0)
                                                                    && traceIfFalse "not isCommissionsAvailable" (isCommissionsAvailable (getAvailable getShare getTaken date) withdraw)
                                                                    && traceIfFalse "not isCorrect_Output_FundHolding_Datum_With_Collect_Managers_Commission" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_Collect_Managers_Commission)
                                                                    && traceIfFalse "not isCorrect_Output_FundHolding_Value_Without_FT_for_Commissions" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out $ getValueFor_FundHoldingDatum_Control_Without_FT_for_Commissions withdraw)
                                                                    ------------------
                                                                where
                                                                    ------------------
                                                                    !getShare = ProtocolT.pdShare_InBPx1e2_Managers
                                                                    !getTaken = T.hdSubtotal_FT_Commissions_Collected_Managers
                                                                    ------------------
                                                                    !fundHoldingDatum_Control_With_Collect_Managers_Commission = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission fundHoldingDatum_In withdraw
                                                                    ------------------
                                                            (T.ValidatorRedeemerCollect_Delegators_Commission (T.ValidatorRedeemerCollect_Delegators_CommissionType !date !withdraw )) ->
                                                                    ---------------------
                                                                    -- it runs alone
                                                                    ---------------------
                                                                    traceIfFalse "not isSignedByAny delegatorsAdmins" (OnChainHelpers.isSignedByAny delegatorsAdmins info)
                                                                    && traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                                    && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                                                                    && traceIfFalse "not withdraw > 0" (withdraw > 0)
                                                                    && traceIfFalse "not isCommissionsAvailable" (isCommissionsAvailable (getAvailable getShare getTaken date) withdraw)
                                                                    && traceIfFalse "not isCorrect_Output_FundHolding_Datum_With_Collect_Delegators_Commission" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_Collect_Delegators_Commission)
                                                                    && traceIfFalse "not isCorrect_Output_FundHolding_Value_Without_FT_for_Commissions" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out $ getValueFor_FundHoldingDatum_Control_Without_FT_for_Commissions withdraw)
                                                                    ------------------
                                                                where
                                                                    ------------------
                                                                    !getShare = ProtocolT.pdShare_InBPx1e2_Delegators
                                                                    !getTaken = T.hdSubtotal_FT_Commissions_Collected_Delegators
                                                                    ------------------
                                                                    !fundHoldingDatum_Control_With_Collect_Delegators_Commission = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission fundHoldingDatum_In withdraw
                                                                    ------------------
                                                                    !delegatorsAdmins = ProtocolT.pdDelegatorsAdmins protocolDatum_In
                                                                    ------------------
                                                            _ -> False
                                                            ------------------
                                                        where
                                                            ------------------
                                                            !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                                                            ------------------
                                                            !inputRef_TxOut_And_ProtocolDatum =
                                                                case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                                                        @ProtocolT.ValidatorDatum @ProtocolT.ProtocolDatumType
                                                                        ctx
                                                                        inputsRef_TxOuts
                                                                        protocolID_AC
                                                                        ProtocolT.getProtocol_DatumType of
                                                                    [x] -> x
                                                                    _   -> traceError "Expected exactly one Protocol input ref"
                                                            ------------------
                                                            !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                                                            ------------------
                                                            -- si el fondo esta cerrado a la fuerza, las comisiones se calculan con la fecha de cierre
                                                            !deadline = case FundT.fdClosedAt fundDatum_In of
                                                                Just x  -> x
                                                                Nothing -> FundT.fdDeadline fundDatum_In
                                                            ------------------
                                                            getAvailable !getShare !getTaken !date =
                                                                FundHelpers.getCommissionsAvailable deadline fundHoldingDatum_In share taken date
                                                                where
                                                                    !share = getShare protocolDatum_In
                                                                    !taken = getTaken fundHoldingDatum_In
                                                            ------------------
                                                            getValueFor_FT_Commissions! withdraw = LedgerValue.assetClassValue fundFT_AC withdraw
                                                            getValueFor_FundHoldingDatum_Control_Without_FT_for_Commissions !withdraw = valueOf_FundHoldingDatum_In <> negate (getValueFor_FT_Commissions withdraw)
                                                            ------------------
                                                            isCommissionsAvailable :: Integer -> Integer -> Bool
                                                            isCommissionsAvailable !available !withdraw = available >= withdraw


----------------------------------------------------------------------------2

{-# INLINEABLE policyID #-}
policyID :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policyID params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyID||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params


{-# INLINABLE  mkWrappedPolicyID #-}
mkWrappedPolicyID :: BuiltinData ->  BuiltinData -> BuiltinData -> ()
mkWrappedPolicyID fundPolicy_CS = mkPolicyID params
    where
        params = T.PolicyParams
            { ppFundPolicy_CS  = PlutusTx.unsafeFromBuiltinData fundPolicy_CS
            }

policyIDCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
policyIDCode = $$( PlutusTx.compile [|| mkWrappedPolicyID ||])

----------------------------------------------------------------------------2

{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS fundPolicy_CS tokenEmergencyAdminPolicy_CS = mkValidator params
    where
        params = T.ValidatorParams
            {
            vpProtocolPolicyID_CS  = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS,
            vpFundPolicy_CS  = PlutusTx.unsafeFromBuiltinData fundPolicy_CS,
            vpTokenEmergencyAdminPolicy_CS = PlutusTx.unsafeFromBuiltinData tokenEmergencyAdminPolicy_CS
            }

validatorCode :: CompiledCode ( BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( PlutusTx.compile [|| mkWrappedValidator ||])

----------------------------------------------------------------------------2

