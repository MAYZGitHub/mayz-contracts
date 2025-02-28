{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
{- HLINT ignore "Use last"          -}
--------------------------------------------------------------------------------2

module Protocol.Fund.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger
import qualified Ledger.Ada as LedgerAda
import qualified Ledger.Value as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Constants as T
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.Constants as T
import qualified Protocol.Fund.Helpers as FundHelpers
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.InvestUnit.Types as InvestUnitT
import qualified Protocol.Fund.Types as T
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- Any change in the logic, datum or redeemer must change the version of the fundVersion on Protocol.Fund.Types

--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicy #-}
mkPolicy :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicy (T.PolicyParams !protocolPolicyID_CS !fundPolicy_TxOutRef !fundValidator_Hash) !redRaw !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        && validate
        then ()
        else error ()
    where
        ------------------
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !fundValidator_Address = Ledger.scriptHashAddress fundValidator_Hash
        ------------------
        !inputsRef_TxOuts =
            [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoReferenceInputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput)
            ]
        ------------------
        validate :: Bool
        !validate = case redeemer of
            (T.PolicyRedeemerMintID _) -> validateMintAndBurnID
            (T.PolicyRedeemerBurnID _) -> validateMintAndBurnID
            (T.PolicyRedeemerMintFT _) -> validateMintAndBurnFT
            (T.PolicyRedeemerBurnFT _) -> validateMintAndBurnFT
        ------------------
        !fundPolicy_CS = LedgerContextsV2.ownCurrencySymbol ctx
        ------------------
        validateMintAndBurnID :: Bool
        !validateMintAndBurnID =
            case redeemer of
                (T.PolicyRedeemerMintID _) ->
                    ---------------------
                    -- it runs alone
                    ---------------------
                    -- Que venga ProtocolDatum como ref
                    -- Para identificar el correcto ProtocolDatum necesita la póliza Protocol ID que está en los parámetros de esta póliza.
                    -- Que se genere salida out 0 con nuevo FundDatum en direcion correcta
                    -- Que se genere salida out 1 con nuevo InvestUnit en direcion correcta
                    -- Que el FundDatum sea correcto según límites y valores del ProtocolDatum
                    -- Que se minteen todos los Fund IDs con own póliza
                    -- solo se puede ejecutar una vez por que esta parametrizado con fundPolicy_TxOutRef
                    -- Que el FundDatum tenga el Fund ID
                    -- Que se generen, sean correctos, se minteen ID y que los tengan
                    -- Que se estén entregando los MAYZ correspondientes segun FundCategory del FundDatum y según valores del ProtocolDatum para esa clase
                    ---------------------
                    traceIfFalse "not isTxOutAnInput" (OnChainHelpers.isTxOutAnInput fundPolicy_TxOutRef info)
                        && traceIfFalse "not isMintingIDs" isMintingIDs
                        && traceIfFalse "not isCorrect_Output_Fund_Datum" isCorrect_Output_Fund_Datum
                        && traceIfFalse "not isCorrect_Output_CommissionsTable" isCorrect_Output_CommissionsTable
                        && traceIfFalse "not isCorrect_Output_Fund_Value" isCorrect_Output_Fund_Value
                        && traceIfFalse "not isCorrect_Output_InvestUnit_Datum" isCorrect_Output_InvestUnit_Datum
                        && traceIfFalse "not isCorrect_Output_InvestUnit_Value" isCorrect_Output_InvestUnit_Value
                    where
                        ---------------------
                        !inputRef_TxOut_And_ProtocolDatum =
                            case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                @ProtocolT.ValidatorDatum
                                @ProtocolT.ProtocolDatumType
                                ctx
                                inputsRef_TxOuts
                                protocolID_AC
                                ProtocolT.getProtocol_DatumType of
                                [x] -> x
                                _ -> traceError "Expected exactly one Protocol input ref"
                        ------------------
                        !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                        ------------------
                        !outputs_txOuts =
                            [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)
                            ]
                        ------------------
                        -- 0 out is the FundDatum
                        -- 1 out is the InvestUnitDatum
                        ------------------
                        !(outputs_txOuts_index0, outputs_txOuts_index1) =
                            if length outputs_txOuts < 2
                                then traceError "Expected at least two outputs to script addresses"
                                else (head outputs_txOuts, outputs_txOuts !! 1)
                        ------------------
                        !output_Own_TxOut_And_FundDatum =
                            fromMaybe
                                (traceError "Expected Fund at output index 0")
                                ( OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                    @T.ValidatorDatum
                                    @T.FundDatumType
                                    ctx
                                    outputs_txOuts_index0
                                    fundID_AC
                                    (Just fundValidator_Address)
                                    T.getFund_DatumType
                                )
                        ------------------
                        !fundDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundDatum
                        ------------------
                        !fundHoldingPolicyID_CS = T.fdFundHoldingPolicyID_CS fundDatum_Out
                        !fundHoldingValidator_Hash = T.fdFundHoldingValidator_Hash fundDatum_Out
                        !investUnitValidator_Hash = T.fdInvestUnitValidator_Hash fundDatum_Out
                        !investUnitValidator_Address = Ledger.scriptHashAddress investUnitValidator_Hash
                        ------------------
                        !output_TxOut_And_InvestUnitDatum =
                            fromMaybe
                                (traceError "Expected InvestUnit at output index 1")
                                ( OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                    @InvestUnitT.ValidatorDatum
                                    @InvestUnitT.InvestUnitDatumType
                                    ctx
                                    outputs_txOuts_index1
                                    investUnitID_AC
                                    (Just investUnitValidator_Address)
                                    InvestUnitT.getInvestUnit_DatumType
                                )
                        ------------------
                        !investUnitDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_TxOut_And_InvestUnitDatum
                        ------------------
                        !fundCategoryNumber = T.fdFundCategoryNumber fundDatum_Out
                        ---------------------
                        !fundCategories = ProtocolT.pdFundCategories protocolDatum_In
                        !fundLifeTime = ProtocolT.pdFundLifeTime protocolDatum_In
                        !commissionFund_PerYear_InBPx1e3 = ProtocolT.pdCommissionFund_PerYear_InBPx1e3 protocolDatum_In
                        !selectedFundCategory' = find (\fundCategory' -> ProtocolT.fcCategoryNumber fundCategory' == fundCategoryNumber) fundCategories
                        ---------------------
                        !tokenMAYZ_AC = ProtocolT.pdTokenMAYZ_AC protocolDatum_In
                        !requiredMAYZ = case selectedFundCategory' of
                            Nothing ->
                                traceError "Can't find Fund Category"
                            Just selectedFundCategory -> do
                                ProtocolT.fcRequiredMAYZ selectedFundCategory
                        ---------------------
                        !valueOf_RequiredMAYZ = LedgerValue.assetClassValue tokenMAYZ_AC requiredMAYZ
                        ---------------------
                        !valueFor_Mint_FundID = LedgerValue.assetClassValue fundID_AC 1
                        !valueFor_Mint_InvestUnitID = LedgerValue.assetClassValue investUnitID_AC 1
                        ---------------------
                        !valueFor_Mint_FundID_And_OtherIDs = valueFor_Mint_FundID <> valueFor_Mint_InvestUnitID
                        ---------------------
                        !minADA_For_FundDatum = T.fdMinADA fundDatum_Out
                        !value_MinADA_For_FundDatum = LedgerAda.lovelaceValueOf minADA_For_FundDatum
                        !valueFor_FundDatum_Out_Control = valueFor_Mint_FundID <> value_MinADA_For_FundDatum <> valueOf_RequiredMAYZ
                        ---------------------
                        !minADA_For_InvestUnitDatum = InvestUnitT.iudMinADA investUnitDatum_Out
                        !value_MinADA_For_InvestUnitDatum = LedgerAda.lovelaceValueOf minADA_For_InvestUnitDatum
                        !valueFor_InvestUnitDatum_Out_Control = valueFor_Mint_InvestUnitID <> value_MinADA_For_InvestUnitDatum
                        ---------------------
                        !fundFT_TN = T.fdFundFT_TN fundDatum_Out
                        !admins = T.fdAdmins fundDatum_Out
                        !tokenAdminPolicy_CS = T.fdTokenAdminPolicy_CS fundDatum_Out
                        !beginAt = T.fdBeginAt fundDatum_Out
                        !deadline = T.fdDeadline fundDatum_Out
                        !closedAt = Nothing
                        !commission_PerYear_InBPx1e3 = T.fdCommission_PerYear_InBPx1e3 fundDatum_Out
                        !commissions_Table_Numerator_1e6 = T.fdCommissions_Table_Numerator_1e6 fundDatum_Out
                        !holdingsCount = 0
                        !holdingsIndex = 0
                        !maxDepositAndWithdraw = T.fdMaxDepositAndWithdraw fundDatum_Out
                        ---------------------
                        !fundDatum_Out_Control =
                            T.mkFund_DatumType
                                fundPolicy_CS
                                fundFT_TN
                                fundValidator_Hash
                                fundHoldingPolicyID_CS
                                fundHoldingValidator_Hash
                                investUnitValidator_Hash
                                admins
                                tokenAdminPolicy_CS
                                fundCategoryNumber
                                beginAt
                                deadline
                                closedAt
                                commission_PerYear_InBPx1e3
                                commissions_Table_Numerator_1e6
                                holdingsCount
                                holdingsIndex
                                maxDepositAndWithdraw
                                tokenMAYZ_AC
                                requiredMAYZ
                                minADA_For_FundDatum
                        ---------------------
                        !investUnit = InvestUnitT.iudInvestUnit investUnitDatum_Out
                        !investUnitDatum_Out_Control = InvestUnitT.mkInvestUnit_DatumType fundPolicy_CS investUnit minADA_For_InvestUnitDatum
                        -----------------
                        !monthsRemaining = FundHelpers.getRemainingMonths deadline beginAt
                        -----------------
                        isMintingIDs :: Bool
                        !isMintingIDs = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_FundID_And_OtherIDs
                        -----------------
                        isCorrect_Output_Fund_Datum :: Bool
                        !isCorrect_Output_Fund_Datum =
                            fundDatum_Out
                                `OnChainHelpers.isUnsafeEqDatums` fundDatum_Out_Control
                                && traceIfFalse "not isDateNotReached deadline" (OnChainHelpers.isDateNotReached deadline info)
                                && traceIfFalse "not deadline > beginAt" (deadline > beginAt)
                                && traceIfFalse "not isInRange fundLifeTime" (ProtocolT.isInRange fundLifeTime (deadline - beginAt))
                                && traceIfFalse "not isInRange commissionFund_PerYear_InBPx1e3" (ProtocolT.isInRange commissionFund_PerYear_InBPx1e3 commission_PerYear_InBPx1e3)
                                && traceIfFalse "not (maxDepositAndWithdraw >= 0" (maxDepositAndWithdraw >= 0)
                        ------------------
                        isCorrect_Output_CommissionsTable :: Bool
                        !isCorrect_Output_CommissionsTable =
                            --     let
                            --         !monthsRemainingPlusOne = monthsRemaining + 1
                            --         -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120 000 000
                            --         den = 120_000_000
                            --         commissions_Table_Numerator_1e6_lastElement = OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den monthsRemainingPlusOne
                            --     in
                            --         -- the table contains motnhly commissions, from 0 remaining months, to the life of the fund plus 1 month
                            --         -- that is why there are 2 elements in the table more than the life of the fund
                            --         monthsRemainingPlusOne + 1 == length commissions_Table_Numerator_1e6 && head (reverse commissions_Table_Numerator_1e6) == commissions_Table_Numerator_1e6_lastElement
                            let
                                !monthsRemainingPlusOne = monthsRemaining + 1
                            in
                                -- the table contains motnhly commissions, from 0 remaining months, to the life of the fund plus 1 month
                                -- that is why there are 2 elements in the table more than the life of the fund

                                monthsRemainingPlusOne + 1 == length commissions_Table_Numerator_1e6
                        ------------------
                        isCorrect_Output_Fund_Value :: Bool
                        !isCorrect_Output_Fund_Value =
                            let
                                !valueOf_FundDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_FundDatum
                                !currentMAYZ = OnChainHelpers.getAmt_With_AC_InValue valueOf_FundDatum_Out tokenMAYZ_AC
                            in
                                traceIfFalse "not currentMAYZ == requiredMAYZ" (currentMAYZ == requiredMAYZ)
                                    && valueOf_FundDatum_Out
                                    `OnChainHelpers.isEqValue` valueFor_FundDatum_Out_Control
                        -----------------
                        isCorrect_Output_InvestUnit_Datum :: Bool
                        !isCorrect_Output_InvestUnit_Datum =
                            investUnitDatum_Out `OnChainHelpers.isUnsafeEqDatums` investUnitDatum_Out_Control
                        ------------------
                        isCorrect_Output_InvestUnit_Value :: Bool
                        !isCorrect_Output_InvestUnit_Value =
                            let
                                !valueOf_InvestUnitDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_TxOut_And_InvestUnitDatum
                            in
                                valueOf_InvestUnitDatum_Out `OnChainHelpers.isEqValue` valueFor_InvestUnitDatum_Out_Control
                ------------------
                (T.PolicyRedeemerBurnID _) ->
                    ------------------
                    -- it runs along with Fund Validator (ValidatorRedeemerDelete)
                    -- it runs along with InvestUnit Validator (ValidatorRedeemerDelete)
                    ------------------
                    -- Que se quemen todos los Fund IDs con own póliza
                    -- No hay control adicional sobre los MAYZ, se supone que quien hace la tx, es un Fund Admin y el recupera los MAYZ o los envia a cualquier lado
                    -- No hay control del redeemer del Fund Validator, por que se sobre entiende que de algun lado salen estos tokens
                    -- Y solo se pueden sacar consumiendo el Fund Datum y para eso solo el validador va a permitir con el redeemer correcto
                    ---------------------
                    traceIfFalse "not isBurningIDs" isBurningIDs
                    where
                        ------------------
                        !valueFor_Burn_FundID = LedgerValue.assetClassValue fundID_AC (negate 1)
                        !valueFor_Burn_InvestUnitID = LedgerValue.assetClassValue investUnitID_AC (negate 1)
                        ---------------------
                        !valueFor_Burn_FundID_And_OtherIDs = valueFor_Burn_FundID <> valueFor_Burn_InvestUnitID
                        ---------------------
                        isBurningIDs :: Bool
                        !isBurningIDs = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_FundID_And_OtherIDs
                -----------------
                _ -> False
            where
                ------------------
                !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
        ------------------
        validateMintAndBurnFT :: Bool
        validateMintAndBurnFT =
            case redeemer of
                (T.PolicyRedeemerMintFT _) ->
                    ------------------
                    -- it runs along with FundHolding Validator (ValidatorRedeemerDeposit)
                    ------------------
                    -- que se este consumiendo fundholding datum con el redeemer correcto
                    -- que se este minteando FT, no importa la cantidad, eso esta controlado en FundHolding Validator (ValidatorRedeemerDeposit)
                    -- que el fondo este abierto
                    ------------------
                    traceIfFalse "not isFundOpen" isFundOpen
                        && traceIfFalse "not isMintingFT" isMintingFT
                    where
                        ------------------
                        !redeemerFundHoldingDatum = get_Redeemer_FundHoldingDatum
                        !validatorRedeemerDepositType = case redeemerFundHoldingDatum of
                            FundHoldingT.ValidatorRedeemerDeposit x -> x
                            _ -> traceError "Expected FundHolding ValidatorRedeemerDeposit"
                        ------------------
                        !deposit = FundHoldingT.rdAmount validatorRedeemerDepositType
                        ------------------
                        isMintingFT :: Bool
                        !isMintingFT = OnChainHelpers.isOnlyToken_Minting_With_AC_AndAmt fundFT_AC deposit info
                        ------------------
                        isFundOpen :: Bool
                        !isFundOpen = OnChainHelpers.isDateReached (T.fdBeginAt fundDatum_In) info && OnChainHelpers.isDateNotReached (T.fdDeadline fundDatum_In) info && isNothing (T.fdClosedAt fundDatum_In)
                -----------------
                (T.PolicyRedeemerBurnFT _) ->
                    ------------------
                    -- it runs along with FundHolding Validator (ValidatorRedeemerWithdraw)
                    ------------------
                    -- no hay restricciones temporales
                    -- que se este consumiendo fundholding datum con el redeemer correcto
                    -- NOTE: no hace falta controlar este redeemer, no hay otra forma de conseguir FT que no sea consumiendo el fundHolding datum con el redeemer correcto
                    -- si no pongo esta condicion permitiría quemar FT por otros medios, si alguien quiere eliminarlos de su wallet por ejemplo
                    -- que se este quemando FT
                    ------------------
                    traceIfFalse "not isBurningFT" isBurningFT
                    where
                        ------------------
                        !redeemerFundHoldingDatum = get_Redeemer_FundHoldingDatum
                        !validatorRedeemerWithdrawType = case redeemerFundHoldingDatum of
                            FundHoldingT.ValidatorRedeemerWithdraw x -> x
                            _ -> traceError "Expected FundHolding ValidatorRedeemerWithdraw"
                        ------------------
                        !withdrawPlusComissions = FundHoldingT.rwAmountPlusComissions validatorRedeemerWithdrawType
                        ------------------
                        isBurningFT :: Bool
                        !isBurningFT = OnChainHelpers.isOnlyToken_Burning_With_AC_AndAmt fundFT_AC (negate withdrawPlusComissions) info
                ------------------
                _ -> False
            where
                ------------------
                !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                ------------------
                !input_TxOut_And_FundDatum =
                    case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                        @T.ValidatorDatum
                        @T.FundDatumType
                        ctx
                        inputsRef_TxOuts
                        fundID_AC
                        T.getFund_DatumType of
                        [x] -> x
                        _ -> traceError "Expected exactly one Fund input ref"
                ------------------
                !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundDatum
                ------------------
                !fundFT_TN = T.fdFundFT_TN fundDatum_In
                !fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS, fundFT_TN)
                ------------------
                get_Redeemer_FundHoldingDatum :: FundHoldingT.ValidatorRedeemer
                get_Redeemer_FundHoldingDatum =
                    let
                        !fundHoldingPolicyID_CS = T.fdFundHoldingPolicyID_CS fundDatum_In
                        ------------------
                        !inputs_TxOutRefs_TxOuts = [(LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | txInfoInput <- LedgerApiV2.txInfoInputs info]
                        ------------------
                        !input_TxOutRef_TxOut_And_FundHoldingDatum =
                            case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS
                                @FundHoldingT.ValidatorDatum
                                @FundHoldingT.FundHoldingDatumType
                                ctx
                                inputs_TxOutRefs_TxOuts
                                fundHoldingPolicyID_CS
                                FundHoldingT.getFundHolding_DatumType of
                                [x] -> x
                                _ -> traceError "Expected exactly one FundHolding input"
                        ------------------
                        !redeemerFor_FundHoldingDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef) input_TxOutRef_TxOut_And_FundHoldingDatum) info
                    in
                        case redeemerFor_FundHoldingDatum' of
                            Nothing -> traceError "Expected redeemer for FundHolding input"
                            Just fundHoldingRedeemerRaw ->
                                case LedgerApiV2.fromBuiltinData @FundHoldingT.ValidatorRedeemer $ LedgerApiV2.getRedeemer fundHoldingRedeemerRaw of
                                    Just x -> x
                                    _ -> traceError "Expected valid redeemer for FundHolding input"

--------------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator (T.ValidatorParams !protocolPolicyID_CS !tokenEmergencyAdminPolicy_CS) !datumRaw !redRaw !ctxRaw =
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
                _ -> False
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
                    && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
                    && traceIfFalse "Expected exactly one Fund input" (length inputs_Own_TxOuts == 1)
                    && ( validateAdminAction
                            && validateRedeemerAdmin
                       )
                    then ()
                    else error ()
                where
                    ------------------
                    !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
                    ------------------
                    !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
                    ------------------
                    !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
                    !fund_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
                    ------------------
                    !inputs_Own_TxOuts =
                        [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info, let
                                                                                                                        address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                                                    in
                                                                                                                        OnChainHelpers.isScriptAddress address && address == fund_Validator_Address
                        ]
                    ------------------
                    !fundDatum_In = T.getFund_DatumType datum
                    ------------------
                    !fundPolicy_CS = T.fdFundPolicy_CS fundDatum_In
                    !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                    ------------------
                    validateAdminAction :: Bool
                    !validateAdminAction =
                        ------------------
                        -- Que este el token de admin presente
                        -- o Que sea Fund Admin
                        ------------------
                        traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent" (OnChainHelpers.isSignedByAny admins info || isAdminTokenPresent)
                        where
                            !admins = T.getAdmins fundDatum_In
                            ------------------
                            isAdminTokenPresent :: Bool
                            isAdminTokenPresent = case LedgerApiV2.txInfoOutputs info of
                                [] -> False
                                -- search admin token in output 0
                                (output : _) -> OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue output) tokenAdmin_AC
                                where
                                    !tokenAdminPolicy_CS = T.getAdminToken_CS fundDatum_In
                                    !tokenAdmin_AC = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.fundTokenAdmin_TN)
                    ------------------
                    validateRedeemerAdmin :: Bool
                    validateRedeemerAdmin =
                        case redeemer of
                            -- separo el redeemer delete por que este no tiene output FundDatum
                            (T.ValidatorRedeemerDelete _) ->
                                ------------------
                                -- it runs along with Fund ID Policy  (PolicyRedeemerBurnID)
                                -- it runs along with InvestUnit Validator (ValidatorRedeemerDelete)
                                ------------------
                                -- Que sea Fund Admin
                                -- que el fondo tenga CERO holdings
                                -- Que se quemen todos los Fund IDs con la correcta póliza indicada en FundDatum siendo consumido
                                -- (no hace falta controlar que se quemen todos, con que se queme Fund ID esta bien, en la poliza se controla que el ID de investUnit tambien sea quemado)
                                ------------------
                                traceIfFalse "not isBurningFundID" isBurningFundID
                                    && traceIfFalse "not isZeroHoldings" isZeroHoldings
                                where
                                    ------------------
                                    isBurningFundID :: Bool
                                    !isBurningFundID = OnChainHelpers.isNFT_Burning_With_AC fundID_AC info
                                    ------------------
                                    isZeroHoldings :: Bool
                                    !isZeroHoldings = T.fdHoldingsCount fundDatum_In == 0
                            ------------------
                            _ -> case redeemer of
                                (T.ValidatorRedeemerDatumUpdate _) ->
                                    ---------------------
                                    -- it runs alone
                                    ---------------------
                                    -- solo es posible actualizar admin y token admin
                                    -- Que sea Fund Admin
                                    -- no hay restricciones temporales
                                    -- Que el FundDatum regrese a Fund Val (se hace automaticamente al buscar outputs en same address)
                                    ---------------------
                                    traceIfFalse "not isCorrect_Output_Fund_Datum_Updated" isCorrect_Output_Fund_Datum_Updated
                                        && traceIfFalse "not isCorrect_Output_Fund_Value_NotChanged" isCorrect_Output_Fund_Value_NotChanged
                                    where
                                        ---------------------
                                        !maxDepositAndWithdraw = T.fdMaxDepositAndWithdraw fundDatum_Out
                                        ---------------------
                                        isCorrect_Output_Fund_Datum_Updated :: Bool
                                        !isCorrect_Output_Fund_Datum_Updated =
                                            let
                                                !fundDatum_Out_Control = FundHelpers.mkUpdated_Fund_Datum_With_NormalChanges fundDatum_In (T.fdAdmins fundDatum_Out) (T.fdTokenAdminPolicy_CS fundDatum_Out) maxDepositAndWithdraw
                                            in
                                                fundDatum_Out
                                                    `OnChainHelpers.isUnsafeEqDatums` fundDatum_Out_Control
                                                    && traceIfFalse "not (maxDepositAndWithdraw >= 0" (maxDepositAndWithdraw >= 0)
                                ------------------
                                (T.ValidatorRedeemerFundHoldingAdd T.ValidatorRedeemerFundHoldingAddType) ->
                                    ------------------
                                    -- it runs along with Holding ID Policy (PolicyRedeemerMintID)
                                    ------------------
                                    -- Que sea Fund Admin
                                    -- Que se mintee Holding ID con la correcta póliza indicada en FundDatum
                                    -- Que el FundDatum regrese a Fund Val (se hace automaticamente al buscar outputs en same address)
                                    -- Que el FundDatum se actualiza con nuevo Holding
                                    -- Que el FundDatum value no cambie
                                    -- el datum FundHolding y la direccion a donde va estara controlado por la poliza FundHolding, que ya me aseguro que se ejecuta el controlar que se este minteando FundHolding ID
                                    -- que el fondo este abierto, o sea entre beginAt y deadline y no este cerrado closedAt
                                    ------------------
                                    traceIfFalse "not isCorrect_Output_Fund_Datum_With_HoldingAdded" isCorrect_Output_Fund_Datum_With_HoldingAdded
                                        && traceIfFalse "not isCorrect_Output_Fund_Value_NotChanged" isCorrect_Output_Fund_Value_NotChanged
                                        && traceIfFalse "not isMintingFundHoldingID" isMintingFundHoldingID
                                    where
                                        ------------------
                                        isCorrect_Output_Fund_Datum_With_HoldingAdded :: Bool
                                        !isCorrect_Output_Fund_Datum_With_HoldingAdded =
                                            let
                                                !fundDatum_Out_Control = FundHelpers.mkUpdated_Fund_Datum_With_HoldingAdded fundDatum_In
                                            in
                                                fundDatum_Out `OnChainHelpers.isUnsafeEqDatums` fundDatum_Out_Control
                                        ------------------
                                        isMintingFundHoldingID :: Bool
                                        !isMintingFundHoldingID =
                                            let
                                                !fundHoldingPolicyID_CS = T.fdFundHoldingPolicyID_CS fundDatum_In
                                            in
                                                OnChainHelpers.isNFT_Minting_With_CS fundHoldingPolicyID_CS info
                                ------------------
                                (T.ValidatorRedeemerFundHoldingDelete T.ValidatorRedeemerFundHoldingDeleteType) ->
                                    ------------------
                                    -- it runs along with FundHolding ID Policy (PolicyRedeemerBurnID)
                                    ------------------
                                    -- it runs along with FundHolding Validator (ValidatorRedeemerDelete)
                                    ------------------
                                    -- Que sea Fund Admin
                                    -- Que se queme Holding ID con la correcta póliza indicada en FundDatum
                                    -- Que el FundDatum regrese a Fund Val (se hace automaticamente al buscar outputs en same address)
                                    -- Que el FundDatum se actualiza con el Holding eliminado
                                    -- Que el FundDatum value no cambie
                                    -- no hay restricciones temporales
                                    ------------------
                                    traceIfFalse "not isCorrect_Output_Fund_Datum_With_HoldingDeleted" isCorrect_Output_Fund_Datum_With_HoldingDeleted
                                        && traceIfFalse "not isCorrect_Output_Fund_Value_NotChanged" isCorrect_Output_Fund_Value_NotChanged
                                        && traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
                                    where
                                        ------------------
                                        isCorrect_Output_Fund_Datum_With_HoldingDeleted :: Bool
                                        !isCorrect_Output_Fund_Datum_With_HoldingDeleted =
                                            let
                                                !fundDatum_Out_Control = FundHelpers.mkUpdated_Fund_Datum_With_HoldingDeleted fundDatum_In
                                            in
                                                fundDatum_Out `OnChainHelpers.isUnsafeEqDatums` fundDatum_Out_Control
                                        ------------------
                                        isBurningFundHoldingID :: Bool
                                        !isBurningFundHoldingID =
                                            let
                                                !fundHoldingPolicyID_CS = T.fdFundHoldingPolicyID_CS fundDatum_In
                                            in
                                                OnChainHelpers.isNFT_Burning_With_CS fundHoldingPolicyID_CS info
                                ------------------
                                (T.ValidatorRedeemerFinish (T.ValidatorRedeemerFinishType date)) ->
                                    ---------------------
                                    -- it runs alone
                                    ---------------------
                                    -- Que sea Fund Admin
                                    -- Que el FundDatum regrese a Fund Val (se hace automaticamente al buscar outputs en same address)
                                    -- Que el FundDatum se actualiza correctamente
                                    -- Que el FundDatum value no cambie
                                    -- Que el Fondo este abierto
                                    -- Que la fecha de cierre sea valida
                                    ------------------
                                    traceIfFalse "not isCorrect_Output_Fund_Datum_With_ClosedAt" isCorrect_Output_Fund_Datum_With_ClosedAt
                                        && traceIfFalse "not isCorrect_Output_Fund_Value_NotChanged" isCorrect_Output_Fund_Value_NotChanged
                                        && traceIfFalse "not isFundOpen" isFundOpen
                                        && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                                    where
                                        ------------------
                                        isCorrect_Output_Fund_Datum_With_ClosedAt :: Bool
                                        !isCorrect_Output_Fund_Datum_With_ClosedAt =
                                            let
                                                !fundDatum_Out_Control = FundHelpers.mkUpdated_Fund_Datum_With_ClosedAt fundDatum_In date
                                            in
                                                fundDatum_Out `OnChainHelpers.isUnsafeEqDatums` fundDatum_Out_Control
                                ------------------
                                (T.ValidatorRedeemerUpdateMinADA _) ->
                                    ---------------------
                                    -- it runs alone
                                    ---------------------
                                    -- Que sea Fund Admin
                                    -- Que el FundDatum regrese a Fund Val (se hace automaticamente al buscar outputs en same address)
                                    -- Que el FundDatum se actualiza correctamente
                                    -- Que el FundDatum value cambie con el min ADA nuevo
                                    -- no hay restricciones temporales
                                    ------------------
                                    traceIfFalse "not min ADA > 0" (newMinADA > 0)
                                        && traceIfFalse "not isCorrect_Output_Fund_Datum_UpdatedMinADA" isCorrect_Output_Fund_Datum_UpdatedMinADA
                                        && traceIfFalse "not isCorrect_Output_Fund_Value_With_MinADAChanged" isCorrect_Output_Fund_Value_With_MinADAChanged
                                    where
                                        ------------------
                                        !newMinADA = T.fdMinADA fundDatum_Out
                                        ------------------
                                        isCorrect_Output_Fund_Datum_UpdatedMinADA :: Bool
                                        !isCorrect_Output_Fund_Datum_UpdatedMinADA =
                                            let
                                                !fundDatum_Out_Control =
                                                    FundHelpers.mkUpdated_Fund_Datum_With_MinADAChanged
                                                        fundDatum_In
                                                        newMinADA
                                            in
                                                fundDatum_Out `OnChainHelpers.isUnsafeEqDatums` fundDatum_Out_Control
                                        ------------------
                                        isCorrect_Output_Fund_Value_With_MinADAChanged :: Bool
                                        !isCorrect_Output_Fund_Value_With_MinADAChanged =
                                            let
                                                !valueOf_FundDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_FundDatum
                                                !valueFor_FundDatum_Control = valueOf_FundDatum_In <> LedgerAda.lovelaceValueOf (newMinADA - T.fdMinADA fundDatum_In)
                                            in
                                                valueOf_FundDatum_Out `OnChainHelpers.isEqValue` valueFor_FundDatum_Control
                                _ -> False
                                where
                                    ------------------
                                    !outputs_txOuts =
                                        [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)
                                        ]
                                    ------------------
                                    !outputs_txOuts_index0 =
                                        if null outputs_txOuts
                                            then traceError "Expected at least one output to script addresses"
                                            else head outputs_txOuts
                                    ------------------
                                    !output_Own_TxOut_And_FundDatum =
                                        fromMaybe
                                            (traceError "Expected Fund at output index 0")
                                            ( OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                                @T.ValidatorDatum
                                                @T.FundDatumType
                                                ctx
                                                outputs_txOuts_index0
                                                fundID_AC
                                                (Just fund_Validator_Address)
                                                T.getFund_DatumType
                                            )
                                    ------------------
                                    !valueOf_FundDatum_In = LedgerApiV2.txOutValue input_TxOut_BeingValidated
                                    ------------------
                                    !fundDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundDatum
                                    ------------------
                                    isCorrect_Output_Fund_Value_NotChanged :: Bool
                                    isCorrect_Output_Fund_Value_NotChanged =
                                        let
                                            !valueOf_FundDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_FundDatum
                                            !valueFor_FundDatum_Out_Control = valueOf_FundDatum_In
                                        in
                                            valueOf_FundDatum_Out `OnChainHelpers.isEqValue` valueFor_FundDatum_Out_Control
                                    ------------------
                                    isFundOpen :: Bool
                                    !isFundOpen = OnChainHelpers.isDateReached (T.fdBeginAt fundDatum_In) info && OnChainHelpers.isDateNotReached (T.fdDeadline fundDatum_In) info && isNothing (T.fdClosedAt fundDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE policy #-}
policy :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policy params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicy||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

--------------------------------------------------------------------------------2

{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

--------------------------------------------------------------------------------2

{-# INLINEABLE mkWrappedPolicyID #-}
mkWrappedPolicyID :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyID protocolPolicyID_CS fundPolicy_TxHash fundPolicy_TxOutputIndex fundValidator_Hash = mkPolicy params
    where
        tid = PlutusTx.unsafeFromBuiltinData fundPolicy_TxHash :: BuiltinByteString
        txout =
            LedgerApiV2.TxOutRef
                { LedgerApiV2.txOutRefId = LedgerApiV2.TxId tid
                , LedgerApiV2.txOutRefIdx = PlutusTx.unsafeFromBuiltinData fundPolicy_TxOutputIndex
                }
        params =
            T.PolicyParams
                { ppProtocolPolicyID_CS = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
                , ppFundPolicy_TxOutRef = txout
                , ppFundValidator_Hash = PlutusTx.unsafeFromBuiltinData fundValidator_Hash
                }

policyCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyCode = $$(PlutusTx.compile [||mkWrappedPolicyID||])

--------------------------------------------------------------------------------2

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS tokenEmergencyAdminPolicy_CS = mkValidator params
    where
        params =
            T.ValidatorParams
                { vpProtocolPolicyID_CS = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
                , vpTokenEmergencyAdminPolicy_CS = PlutusTx.unsafeFromBuiltinData tokenEmergencyAdminPolicy_CS
                }

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(PlutusTx.compile [||mkWrappedValidator||])

--------------------------------------------------------------------------------2
