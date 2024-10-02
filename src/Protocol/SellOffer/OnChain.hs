{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.SellOffer.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger
import qualified Ledger.Ada                as LedgerAda
import qualified Ledger.Value              as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Constants         as T
import qualified Generic.OnChainHelpers    as OnChainHelpers
import qualified Protocol.Constants        as T
import qualified Protocol.Fund.Types       as FundT
import qualified Protocol.OnChainHelpers   as OnChainHelpers
import qualified Protocol.Protocol.Types   as ProtocolT
import qualified Protocol.SellOffer.Types  as T
import qualified Protocol.Types            as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID  (T.PolicyParams !protocolPolicyID_CS !sellOffer_Validator_Hash !tokenMAYZ_AC) !redRaw !ctxRaw =
    let
        ------------------
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !sellOffer_Validator_Address = Ledger.scriptHashAddress sellOffer_Validator_Hash
        ------------------
        !sellOfferPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !sellOfferID_AC = LedgerValue.AssetClass (sellOfferPolicyID_CS, T.sellOfferID_TN)
        ------------------
        !valueFor_Mint_SellOffer_ID = LedgerValue.assetClassValue sellOfferID_AC 1
        ------------------
    in  if traceIfFalse "" useThisToMakeScriptUnique
            &&
            case redeemer of
                T.PolicyRedeemerMintID _ ->
                        ---------------------
                        -- que se mintee ID de Sell Order, con esta poliza, 1 unidad, con nombre de token que venga en datum del protocolo
                        -- que se cree datum correcto (parametros dentro de rangos permitidos, totales en cero, seller que firma la tx)
                        -- que vaya a la direccion del contrato correcta. La direccion puede estar en el datum del protocolo
                        ---------------------
                        traceIfFalse "not isMintingSellOfferID" isMintingSellOfferID &&
                        traceIfFalse "not isCorrect_Output_SellOffer_Datum" isCorrect_Output_SellOffer_Datum &&
                        traceIfFalse "not isCorrect_Output_SellOffer_Value" isCorrect_Output_SellOffer_Value &&
                        traceIfFalse "expected zero SellOffer inputs" (null inputs_Own_TxOuts)
                        ---------------------
                    where
                        ------------------
                        !inputsRef_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoReferenceInputs info,
                            OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput )]
                        ------------------
                        !inputs_Own_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info, 
                                        let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput) 
                                        in  OnChainHelpers.isScriptAddress address && address == sellOffer_Validator_Address]
                        ------------------
                        !outputs_Own_TxOuts = [ txOut | txOut <- LedgerApiV2.txInfoOutputs info,
                                        let address = LedgerApiV2.txOutAddress txOut
                                        in  OnChainHelpers.isScriptAddress address && address == sellOffer_Validator_Address ]
                        ------------------
                        !output_Own_TxOut_And_SellOffer_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                            @T.ValidatorDatum @T.SellOffer_DatumType
                            ctx
                            outputs_Own_TxOuts
                            sellOfferID_AC
                            T.getSellOffer_DatumType  of
                            [x] -> x
                            _   -> traceError "Expected exactly one SellOffer output"
                        ------------------
                        !sellOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                        ------------------
                        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                        !fundID_AC = LedgerValue.AssetClass (T.sodFundPolicy_CS sellOffer_Datum_Out, T.fundID_TN)
                        ------------------
                        !inputRef_TxOut_And_ProtocolDatum =
                            case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                @ProtocolT.ValidatorDatum
                                @ProtocolT.ProtocolDatumType
                                ctx
                                inputsRef_TxOuts
                                protocolID_AC
                                ProtocolT.getProtocol_DatumType of
                            [x] -> x
                            _   -> traceError "Expected exactly one Protocol input ref"
                        ------------------
                        !inputRef_TxOut_And_FundDatum =
                            case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                @FundT.ValidatorDatum
                                @FundT.FundDatumType
                                ctx
                                inputsRef_TxOuts
                                fundID_AC
                                FundT.getFund_DatumType of
                            [x] -> x
                            _   -> traceError "Expected exactly one Fund input ref"
                        ------------------
                        !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                        ------------------
                        !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_FundDatum
                        ------------------
                        !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
                        !fundFT_TN_AC = LedgerValue.AssetClass (T.sodFundPolicy_CS sellOffer_Datum_Out, fundFT_TN)
                        ------------------
                        !commissionSellOffer_InBPx1e3 = ProtocolT.pdCommissionSellOffer_InBPx1e3 protocolDatum_In
                        ---------------------
                        !requiredMAYZ = ProtocolT.pdRequiredMAYZForSellOffer protocolDatum_In
                         ---------------------
                        !valueOf_RequiredMAYZ = LedgerValue.assetClassValue tokenMAYZ_AC requiredMAYZ
                        ---------------------
                        isMintingSellOfferID :: Bool
                        isMintingSellOfferID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_SellOffer_ID
                        -----------------
                        isCorrect_Output_SellOffer_Datum :: Bool
                        isCorrect_Output_SellOffer_Datum =
                            let 
                                isValidAllowOption option = option == T.sellOffer_AllowSell || option == T.sellOffer_NotAllowSell 
                                isValidStatus status = status == T.sellOffer_Status_Open || status == T.sellOffer_Status_Closed 
                                !sellOffer_Datum_Out_Control =
                                    T.mkSellOffer_DatumType
                                        sellOfferPolicyID_CS
                                        (T.sodFundPolicy_CS sellOffer_Datum_Out)
                                        (T.sodSellerPaymentPKH sellOffer_Datum_Out)
                                        (T.sodSellerStakePKH sellOffer_Datum_Out)
                                        (T.sodAskedCommission_InBPx1e3 sellOffer_Datum_Out)
                                        (T.sodAmount_FT_Available  sellOffer_Datum_Out)
                                        (T.sodAmount_ADA_Available sellOffer_Datum_Out)
                                        0
                                        0
                                        (T.sodOrder_AllowSellFT  sellOffer_Datum_Out)
                                        (T.sodOrder_AllowSellADA sellOffer_Datum_Out)
                                        (T.sodOrder_Status sellOffer_Datum_Out)
                                        requiredMAYZ
                                        (T.sodMinADA sellOffer_Datum_Out)
                            in  traceIfFalse "not isInRange commissionSellOffer_InBPx1e3" (ProtocolT.isInRange commissionSellOffer_InBPx1e3 (T.sodAskedCommission_InBPx1e3 sellOffer_Datum_Out))
                                && traceIfFalse "not sodAmount_FT_Available >= 0"  (T.sodAmount_FT_Available sellOffer_Datum_Out >= 0)
                                && traceIfFalse "not sodAmount_ADA_Available >= 0"  (T.sodAmount_ADA_Available sellOffer_Datum_Out >= 0)
                                && traceIfFalse "not isValidAllowOption sodOrder_AllowSellFT"  (isValidAllowOption $ T.sodOrder_AllowSellFT sellOffer_Datum_Out)
                                && traceIfFalse "not isValidAllowOption sodOrder_AllowSellADA"  (isValidAllowOption $ T.sodOrder_AllowSellADA sellOffer_Datum_Out)
                                && traceIfFalse "not isValidStatus sellOffer_Status_Open"  (isValidStatus $ T.sodOrder_Status sellOffer_Datum_Out)
                                && sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
                        ------------------
                        isCorrect_Output_SellOffer_Value :: Bool
                        isCorrect_Output_SellOffer_Value =
                            let
                                ---------------------
                                !amount_FT_Available_For_SellOffer_Datum = T.sodAmount_FT_Available  sellOffer_Datum_Out
                                !amount_ADA_Available_For_SellOffer_Datum = T.sodAmount_ADA_Available sellOffer_Datum_Out
                                ---------------------
                                !value_Amount_FT_Available_For_SellOffer_Datum = LedgerValue.assetClassValue fundFT_TN_AC amount_FT_Available_For_SellOffer_Datum
                                !value_Amount_ADA_Available_For_SellOffer_Datum = LedgerAda.lovelaceValueOf amount_ADA_Available_For_SellOffer_Datum
                                ---------------------
                                !minADA_For_SellOffer_Datum = T.sodMinADA sellOffer_Datum_Out
                                !value_MinADA_For_SellOffer_Datum = LedgerAda.lovelaceValueOf minADA_For_SellOffer_Datum
                                ---------------------
                                !valueFor_SellOffer_Datum_Out_Control = valueFor_Mint_SellOffer_ID <> value_MinADA_For_SellOffer_Datum <> value_Amount_FT_Available_For_SellOffer_Datum <> value_Amount_ADA_Available_For_SellOffer_Datum <> valueOf_RequiredMAYZ
                                ---------------------
                                !valueOf_SellOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                                 ---------------------
                                !currentMAYZ = OnChainHelpers.getAmt_With_AC_InValue valueOf_SellOffer_Out tokenMAYZ_AC
                                ---------------------
                            in  traceIfFalse "not currentMAYZ == requiredMAYZ" (currentMAYZ == requiredMAYZ) &&
                                valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Datum_Out_Control
                    ------------------
                T.PolicyRedeemerBurnID _ ->
                        ---------------------
                        -- que se queme ID del Sell Order, 1 unidad. Creo que con esto es suficiente.
                        -- que se este ejecutando validador correcto. No seria necesario. Si se quema es por que sale de algun lado.
                        ---------------------
                        traceIfFalse "not isBurningSellOfferID" isBurningSellOfferID
                        ---------------------
                    where
                        ------------------
                        !valueFor_Burn_SellOffer_ID = LedgerValue.assetClassValue sellOfferID_AC (negate 1)
                        ---------------------
                        isBurningSellOfferID :: Bool
                        isBurningSellOfferID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_SellOffer_ID
                        -----------------
            then ()
            else error ()

--------------------------------------------------------------------------------2


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
                if  traceIfFalse "" useThisToMakeScriptUnique
                    && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
                    && traceIfFalse "Expected exactly one SellOffer input" (length inputs_Own_TxOuts == 1)
                    && validateRedeemerDeleteAndOthers
                        then ()
                        else error ()
                where
                    ---------------------
                    !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
                    ------------------
                    !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
                    ------------------
                    !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
                    !sellOffer_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
                    ------------------
                    !inputs_Own_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info, 
                                    let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput) 
                                    in  OnChainHelpers.isScriptAddress address && address == sellOffer_Validator_Address]
                    ------------------
                    !sellOffer_Datum_In = T.getSellOffer_DatumType datum
                    ------------------
                    !fundPolicy_CS = T.sodFundPolicy_CS sellOffer_Datum_In
                    !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                    ------------------
                    !sellOfferPolicyID_CS = T.sodSellOfferPolicyID_CS sellOffer_Datum_In
                    !sellOfferID_AC = LedgerValue.AssetClass (sellOfferPolicyID_CS, T.sellOfferID_TN)
                    ------------------
                    !admin = T.sodSellerPaymentPKH sellOffer_Datum_In
                    ------------------
                    redeemerUpdateStatus = 1
                    redeemerUpdateAskedCommissionRate = 2
                    redeemerUpdateSellRestrictions = 3
                    redeemerUpdateMinADA = 4
                    redeemerDeposit = 5
                    redeemerWithdraw = 6
                    redeemerSwapFTxADA = 7
                    redeemerSwapADAxFT = 8
                    redeemerDelete = 9
                    ------------------
                    redeemerType :: Integer
                    !redeemerType = case redeemer of
                        (T.ValidatorRedeemerUpdateStatus _)              -> redeemerUpdateStatus
                        (T.ValidatorRedeemerUpdateAskedCommissionRate _) -> redeemerUpdateAskedCommissionRate
                        (T.ValidatorRedeemerUpdateSellRestrictions _)     -> redeemerUpdateSellRestrictions
                        (T.ValidatorRedeemerUpdateMinADA _)              -> redeemerUpdateMinADA
                        (T.ValidatorRedeemerDeposit _)                   -> redeemerDeposit
                        (T.ValidatorRedeemerWithdraw _)                  -> redeemerWithdraw
                        (T.ValidatorRedeemerSwapFTxADA _)                -> redeemerSwapFTxADA
                        (T.ValidatorRedeemerSwapADAxFT _)                -> redeemerSwapADAxFT
                        (T.ValidatorRedeemerDelete _)                    -> redeemerDelete
                        _                                                -> 0
                    ------------------
                    validateRedeemerDeleteAndOthers  :: Bool
                    validateRedeemerDeleteAndOthers
                        | redeemerType == redeemerDelete = validateAdminAction && validateDelete
                        | otherwise = validateAllButDelete
                    ------------------
                    validateAdminAction :: Bool
                    validateAdminAction  =
                        traceIfFalse "not txSignedBy admin" (LedgerContextsV2.txSignedBy info admin)
                    ------------------
                    validateDelete :: Bool
                    validateDelete = traceIfFalse "not isBurningSellOfferID" isBurningSellOfferID
                            ------------------
                            ---- get back all FT and ADA and delete datum utxo. Burn order ID. only admin can do it
                            -- check that there is one input and zero output in this contract
                            -- check that ID is burning
                            ------------------
                        where
                            ------------------
                            isBurningSellOfferID :: Bool
                            isBurningSellOfferID  = OnChainHelpers.isNFT_Burning_With_AC sellOfferID_AC info
                    ------------------
                    validateAllButDelete ::Bool
                    validateAllButDelete
                            ------------------
                            | redeemerType == redeemerUpdateStatus = validateAdminAction && validateUpdateStatus redeemer
                            | redeemerType == redeemerUpdateAskedCommissionRate  = validateAdminAction && validateUpdateAskedCommissionRate redeemer
                            | redeemerType == redeemerUpdateSellRestrictions  = validateAdminAction && validateUpdateSellRestrictions redeemer
                            | redeemerType == redeemerUpdateMinADA = validateAdminAction && validateUpdateMinADA redeemer
                            | redeemerType == redeemerDeposit = validateAdminAction && validateDeposit redeemer
                            | redeemerType == redeemerWithdraw = validateAdminAction && validateWithdraw redeemer
                            | redeemerType == redeemerSwapFTxADA || redeemerType == redeemerSwapADAxFT = validateSwaps
                            | otherwise = False
                            ------------------
                        where
                            ------------------
                            !inputsRef_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoReferenceInputs info,
                                OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput )]
                            ------------------
                            !outputs_Own_TxOuts = [ txOut | txOut <- LedgerApiV2.txInfoOutputs info,
                                                    let address = LedgerApiV2.txOutAddress txOut
                                                    in  OnChainHelpers.isScriptAddress address && address == sellOffer_Validator_Address ]
                            ------------------
                            !inputRef_TxOut_And_FundDatum =
                                case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                    @FundT.ValidatorDatum
                                    @FundT.FundDatumType
                                    ctx
                                    inputsRef_TxOuts
                                    fundID_AC
                                    FundT.getFund_DatumType of
                                [x] -> x
                                _   -> traceError "Expected exactly one Fund input ref"
                            ------------------
                            !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_FundDatum
                            ------------------
                            !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
                            !fundFT_TN_AC = LedgerValue.AssetClass (fundPolicy_CS, fundFT_TN)
                            ------------------
                            !output_Own_TxOut_And_SellOffer_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                    @T.ValidatorDatum @T.SellOffer_DatumType
                                    ctx
                                    outputs_Own_TxOuts
                                    sellOfferID_AC
                                    T.getSellOffer_DatumType  of
                                    [x] -> x
                                    _   -> traceError "Expected exactly one SellOffer output"
                            ------------------
                            !sellOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                            ------------------
                            !valueOf_SellOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                            ----------------
                            isOrderOpen :: Bool
                            !isOrderOpen = T.sodOrder_Status sellOffer_Datum_In == T.sellOffer_Status_Open
                            ------------------
                            isCorrect_Output_SellOffer_Value_NotChanged :: Bool
                            !isCorrect_Output_SellOffer_Value_NotChanged =
                                let !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated
                                in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
                            ----------------
                            getLazyProtocolDatum_In :: ProtocolT.ProtocolDatumType
                            getLazyProtocolDatum_In = 
                                let 
                                    ------------------
                                    !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                                    ------------------
                                    !inputRef_TxOut_And_ProtocolDatum =
                                        case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                            @ProtocolT.ValidatorDatum
                                            @ProtocolT.ProtocolDatumType
                                            ctx
                                            inputsRef_TxOuts
                                            protocolID_AC
                                            ProtocolT.getProtocol_DatumType of
                                        [x] -> x
                                        _   -> traceError "Expected exactly one Protocol input ref"
                                    ------------------
                                    !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                                    ------------------
                                in  protocolDatum_In
                            ----------------
                            validateUpdateStatus :: T.ValidatorRedeemer  -> Bool
                            validateUpdateStatus (T.ValidatorRedeemerUpdateStatus (T.ValidatorRedeemerUpdateStatusType !newStatus))  =
                                    ------------------
                                    ---- change status to open or close. Only admin can do it
                                    -- check that there is one input and one output in this contract
                                    -- check datum update with new status
                                    -- check value not changed
                                    ------------------
                                    traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_StatusChanged" isCorrect_Output_SellOffer_Datum_With_StatusChanged
                                    && traceIfFalse "not isCorrect_Output_SellOffer_Value_NotChanged" isCorrect_Output_SellOffer_Value_NotChanged
                                    ------------------
                                where
                                    ------------------
                                    isCorrect_Output_SellOffer_Datum_With_StatusChanged:: Bool
                                    !isCorrect_Output_SellOffer_Datum_With_StatusChanged =
                                        let !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_StatusChanged sellOffer_Datum_In newStatus
                                        in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
                                    ------------------
                            validateUpdateStatus _   = False
                            ------------------
                            validateUpdateAskedCommissionRate :: T.ValidatorRedeemer  ->  Bool
                            validateUpdateAskedCommissionRate (T.ValidatorRedeemerUpdateAskedCommissionRate (T.ValidatorRedeemerUpdateAskedCommissionRateType !newCommissionRate))  =
                                    ------------------
                                    ---- change commission rate. Only admin can do it. Must be in the range of protocol datum commissionRateForSellOffersRange
                                    -- check that there is one input and one output in this contract
                                    -- check datum update with new commissions rate. Must be in the range of protocol datum commissionRateForSellOffersRange
                                    -- check value not changed
                                    ------------------
                                    traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_CommissionChanged" isCorrect_Output_SellOffer_Datum_With_CommissionChanged
                                    && traceIfFalse "not isCorrect_Output_SellOffer_Value_NotChanged" isCorrect_Output_SellOffer_Value_NotChanged 
                                    && traceIfFalse "not isInRange commissionSellOffer_InBPx1e3" (ProtocolT.isInRange commissionSellOffer_InBPx1e3 (T.sodAskedCommission_InBPx1e3 sellOffer_Datum_Out))
                                where
                                    ------------------
                                    !commissionSellOffer_InBPx1e3 = ProtocolT.pdCommissionSellOffer_InBPx1e3 getLazyProtocolDatum_In
                                    ---------------------
                                    isCorrect_Output_SellOffer_Datum_With_CommissionChanged:: Bool
                                    !isCorrect_Output_SellOffer_Datum_With_CommissionChanged =
                                        let !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_CommissionChanged sellOffer_Datum_In newCommissionRate
                                        in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
                                    ------------------
                            validateUpdateAskedCommissionRate _   = False
                            ------------------
                            validateUpdateSellRestrictions :: T.ValidatorRedeemer  ->  Bool
                            validateUpdateSellRestrictions (T.ValidatorRedeemerUpdateSellRestrictions (T.ValidatorRedeemerUpdateSellRestrictionsType !rusrAllowSellFT !rusrAllowSellADA))  =
                                    ------------------
                                    -- check value not changed
                                    ------------------
                                    traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_RestrictionsChanged" isCorrect_Output_SellOffer_Datum_With_RestrictionsChanged
                                    && traceIfFalse "not isCorrect_Output_SellOffer_Value_NotChanged" isCorrect_Output_SellOffer_Value_NotChanged 
                                where
                                    ------------------
                                    isCorrect_Output_SellOffer_Datum_With_RestrictionsChanged:: Bool
                                    !isCorrect_Output_SellOffer_Datum_With_RestrictionsChanged =
                                        let !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_RestrictionsChanged sellOffer_Datum_In rusrAllowSellFT rusrAllowSellADA
                                        in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
                                    ------------------
                            validateUpdateSellRestrictions _   = False
                            ------------------
                            validateUpdateMinADA :: T.ValidatorRedeemer  ->  Bool
                            validateUpdateMinADA _ =
                                    ------------------
                                    ---- change min ada value. Only admin can do it
                                    -- check that there is one input and one output in this contract
                                    -- check datum update with new minAda
                                    -- check value changed ADA
                                    ------------------
                                    traceIfFalse "not min ADA > 0" (newMinADA > 0) 
                                    && traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_MinADAChanged" isCorrect_Output_SellOffer_Datum_With_MinADAChanged
                                    && traceIfFalse "not isCorrect_Output_SellOffer_Value_With_MinADAChanged" isCorrect_Output_SellOffer_Value_With_MinADAChanged
                                    ------------------
                                where
                                    ------------------
                                    !newMinADA = T.sodMinADA sellOffer_Datum_Out
                                    ------------------
                                    isCorrect_Output_SellOffer_Datum_With_MinADAChanged:: Bool
                                    !isCorrect_Output_SellOffer_Datum_With_MinADAChanged =
                                        let !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_MinADAChanged sellOffer_Datum_In newMinADA
                                        in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
                                    ------------------
                                    isCorrect_Output_SellOffer_Value_With_MinADAChanged :: Bool
                                    !isCorrect_Output_SellOffer_Value_With_MinADAChanged =
                                        let !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated  <> LedgerAda.lovelaceValueOf (newMinADA - T.sodMinADA sellOffer_Datum_In)
                                        in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
                            ----------------
                            validateDeposit :: T.ValidatorRedeemer  ->  Bool
                            validateDeposit (T.ValidatorRedeemerDeposit (T.ValidatorRedeemerDepositType !newDeposit_FT !newDeposit_ADA))  =
                                    ------------------
                                    ---- add some FT or ADA. Only admin can do it.
                                    -- check that there is one input and one output in this contract
                                    -- check datum update with deposit ... me parece que no hay cambios que se hagan en el datum en esta tx
                                    -- check value changed with deposit
                                    ------------------
                                    traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_Deposit" isCorrect_Output_SellOffer_Datum_With_Deposit
                                    && traceIfFalse "not isCorrect_Output_SellOffer_Value_With_Deposit" isCorrect_Output_SellOffer_Value_With_Deposit
                                ------------------
                                where
                                    ------------------
                                    isCorrect_Output_SellOffer_Datum_With_Deposit:: Bool
                                    !isCorrect_Output_SellOffer_Datum_With_Deposit  =
                                        let !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_Deposit sellOffer_Datum_In newDeposit_FT newDeposit_ADA
                                        in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
                                    ------------------
                                    isCorrect_Output_SellOffer_Value_With_Deposit :: Bool
                                    !isCorrect_Output_SellOffer_Value_With_Deposit =
                                        let !value_Deposit_FT = LedgerValue.assetClassValue fundFT_TN_AC newDeposit_FT
                                            !value_Deposit_ADA = LedgerAda.lovelaceValueOf newDeposit_ADA
                                            ------------------
                                            !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> value_Deposit_FT <> value_Deposit_ADA
                                        in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
                                    ----------------
                            validateDeposit _   = False
                            ------------------
                            validateWithdraw :: T.ValidatorRedeemer  ->  Bool
                            validateWithdraw (T.ValidatorRedeemerWithdraw (T.ValidatorRedeemerWithdrawType !newWithdraw_FT !newWithdraw_ADA) )  =
                                    ------------------
                                    ---- get back some FT or ADA. Only admin can do it.
                                    -- check that there is one input and one output in this contract
                                    -- check datum update with withdraw ... me parece que no hay cambios que se hagan en el datum en esta tx
                                    -- check value changed with withdraw
                                    ------------------
                                    traceIfFalse    "not isCorrect_Output_SellOffer_Datum_With_Withdraw" isCorrect_Output_SellOffer_Datum_With_Withdraw
                                    && traceIfFalse "not isCorrect_Output_SellOffer_Value_With_Withdraw" isCorrect_Output_SellOffer_Value_With_Withdraw
                                    ------------------
                                where
                                    ------------------
                                    isCorrect_Output_SellOffer_Datum_With_Withdraw::Bool
                                    !isCorrect_Output_SellOffer_Datum_With_Withdraw =
                                        let !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_Withdraw sellOffer_Datum_In newWithdraw_FT newWithdraw_ADA
                                        in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
                                    ------------------
                                    isCorrect_Output_SellOffer_Value_With_Withdraw :: Bool
                                    !isCorrect_Output_SellOffer_Value_With_Withdraw =
                                        let !value_Withdraw_FT = LedgerValue.assetClassValue fundFT_TN_AC newWithdraw_FT
                                            !value_Withdraw_ADA = LedgerAda.lovelaceValueOf newWithdraw_ADA
                                            ------------------
                                            !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> negate value_Withdraw_FT <> negate value_Withdraw_ADA
                                        in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
                                    ----------------
                            validateWithdraw _   = False
                            ------------------
                            validateSwaps :: Bool
                            validateSwaps
                                    ------------------
                                    | redeemerType == redeemerSwapFTxADA = validateSwapFTxADA redeemer
                                    | redeemerType == redeemerSwapADAxFT = validateSwapADAxFT redeemer
                                    | otherwise = False
                                    ------------------
                                where
                                    ------------------
                                    validateSwapFTxADA :: T.ValidatorRedeemer  ->  Bool
                                    validateSwapFTxADA (T.ValidatorRedeemerSwapFTxADA (T.ValidatorRedeemerSwapFTxADAType !rsfxaAmount_FT !rsfxaAmount_ADA !rsfxaCommission_ADA !rsfxaOracle_Data !rsfxaOracle_Signature))  =
                                            ------------------
                                            ---- if order is open, user give FT and get ADA. Use a price for conversion provided by oracle. Must check signatura and validity time
                                            -- check that there is one input and one output in this contract
                                            -- check datum update with swap. Totals calculated
                                            -- check commissions
                                            -- check value changed FT and ADA
                                            -- check price, validity time and signature
                                            ------------------
                                            traceIfFalse "not isOrderOpen" isOrderOpen
                                            && traceIfFalse "isOrderRestrictedForSellingADA" (not isOrderRestrictedForSellingADA)
                                            && traceIfFalse "not isCorrect_Oracle_Signature" (isCorrect_Oracle_Signature rsfxaOracle_Data rsfxaOracle_Signature)
                                            && traceIfFalse "not isCorrect_Oracle_InRangeTime" (isCorrect_Oracle_InRangeTime rsfxaOracle_Data)
                                            && traceIfFalse "not isCorrect_Conversion" (isCorrect_Conversion rsfxaOracle_Data rsfxaAmount_FT rsfxaAmount_ADA)
                                            && traceIfFalse "not isCorrect_Commission" (isCorrect_Commission rsfxaAmount_ADA rsfxaCommission_ADA)
                                            && traceIfFalse "not isAmount_ADA_Available" (isAmount_ADA_Available (rsfxaAmount_ADA-rsfxaCommission_ADA))
                                            && traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_SwapFTxADA" (isCorrect_Output_SellOffer_Datum_With_SwapFTxADA rsfxaAmount_FT rsfxaAmount_ADA rsfxaCommission_ADA)
                                            && traceIfFalse "not isCorrect_Output_SellOffer_Value_With_SwapFTxADA" (isCorrect_Output_SellOffer_Value_With_SwapFTxADA rsfxaAmount_FT rsfxaAmount_ADA rsfxaCommission_ADA)
                                            ------------------
                                        where
                                            ------------------
                                            isOrderRestrictedForSellingADA :: Bool
                                            !isOrderRestrictedForSellingADA = T.sodOrder_AllowSellADA sellOffer_Datum_In == T.sellOffer_NotAllowSell
                                            ------------------
                                            isCorrect_Output_SellOffer_Datum_With_SwapFTxADA:: Integer -> Integer -> Integer -> Bool
                                            isCorrect_Output_SellOffer_Datum_With_SwapFTxADA !amount_FT !amount_ADA !commission_ADA =
                                                let !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_SwapFTxADA sellOffer_Datum_In amount_FT amount_ADA commission_ADA
                                                in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
                                            ------------------
                                            isCorrect_Output_SellOffer_Value_With_SwapFTxADA ::  Integer -> Integer -> Integer -> Bool
                                            isCorrect_Output_SellOffer_Value_With_SwapFTxADA !amount_FT !amount_ADA !commission_ADA =
                                                let !value_Amount_FT = LedgerValue.assetClassValue fundFT_TN_AC amount_FT
                                                    !value_Amount_ADA = LedgerAda.lovelaceValueOf (amount_ADA - commission_ADA)
                                                    ------------------
                                                    !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> value_Amount_FT <> negate value_Amount_ADA
                                                in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
                                            ----------------
                                    validateSwapFTxADA _   = False
                                    ------------------
                                    validateSwapADAxFT :: T.ValidatorRedeemer  ->  Bool
                                    validateSwapADAxFT (T.ValidatorRedeemerSwapADAxFT (T.ValidatorRedeemerSwapADAxFTType !rsaxfAmount_ADA !rsaxfAmount_FT !rsaxfCommission_FT !rsaxfOracle_Data !rsaxfOracle_Signature ))  =
                                            ------------------
                                            ---- if order is open, user give ADA and get FT. Use a price for conversion provided by oracle. Must check signatura and validity time
                                            -- check that there is one input and one output in this contract
                                            -- check datum update with swap. Totals calculated
                                            -- check commissions
                                            -- check value changed FT and ADA
                                            -- check price, validity time and signature
                                            ------------------
                                            traceIfFalse "not isOrderOpen" isOrderOpen
                                            && traceIfFalse "isOrderRestrictedForSellingFT" (not isOrderRestrictedForSellingFT)
                                            && traceIfFalse "not isCorrect_Oracle_Signature" (isCorrect_Oracle_Signature rsaxfOracle_Data rsaxfOracle_Signature)
                                            && traceIfFalse "not isCorrect_Oracle_InRangeTime" (isCorrect_Oracle_InRangeTime rsaxfOracle_Data )
                                            && traceIfFalse "not isCorrect_Conversion" (isCorrect_Conversion rsaxfOracle_Data rsaxfAmount_FT rsaxfAmount_ADA )
                                            && traceIfFalse "not isCorrect_Commission" (isCorrect_Commission rsaxfAmount_FT rsaxfCommission_FT)
                                            && traceIfFalse "not isAmount_FT_Available" (isAmount_FT_Available (rsaxfAmount_FT-rsaxfCommission_FT))
                                            && traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_SwapADAxFT" (isCorrect_Output_SellOffer_Datum_With_SwapADAxFT rsaxfAmount_ADA rsaxfAmount_FT rsaxfCommission_FT)
                                            && traceIfFalse "not isCorrect_Output_SellOffer_Value_With_SwapADAxFT" (isCorrect_Output_SellOffer_Value_With_SwapADAxFT rsaxfAmount_ADA rsaxfAmount_FT rsaxfCommission_FT)
                                            ------------------
                                        where
                                            ------------------
                                            isOrderRestrictedForSellingFT :: Bool
                                            !isOrderRestrictedForSellingFT = T.sodOrder_AllowSellFT sellOffer_Datum_In == T.sellOffer_NotAllowSell
                                            ------------------
                                            isCorrect_Output_SellOffer_Datum_With_SwapADAxFT:: Integer -> Integer -> Integer -> Bool
                                            isCorrect_Output_SellOffer_Datum_With_SwapADAxFT !amount_ADA !amount_FT !commission_FT =
                                                let !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_SwapADAxFT sellOffer_Datum_In amount_ADA amount_FT commission_FT
                                                in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
                                            ------------------
                                            isCorrect_Output_SellOffer_Value_With_SwapADAxFT ::  Integer -> Integer -> Integer -> Bool
                                            isCorrect_Output_SellOffer_Value_With_SwapADAxFT !amount_ADA !amount_FT !commission_FT =
                                                let !value_Amount_ADA = LedgerAda.lovelaceValueOf amount_ADA
                                                    !value_Amount_FT = LedgerValue.assetClassValue fundFT_TN_AC (amount_FT - commission_FT)
                                                    ------------------
                                                    !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> value_Amount_ADA <> negate value_Amount_FT
                                                in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
                                            ----------------
                                    validateSwapADAxFT _   = False
                                    ----------------
                                    isCorrect_Oracle_Signature :: T.Oracle_Data -> Ledger.Signature ->  Bool
                                    isCorrect_Oracle_Signature oracle_Data oracle_Signature=
                                        let
                                            ------------------
                                            !oraclePaymentPubKey = ProtocolT.pdOraclePaymentPubKey getLazyProtocolDatum_In
                                            ------------------
                                            !priceData = OnChainHelpers.oracleDataToBBS oracle_Data
                                            ------------------
                                            checkSignature :: Ledger.PaymentPubKey
                                                -- ^ The public key of the signatory
                                                -> LedgerApiV2.BuiltinByteString
                                                -- ^ The message
                                                -> Ledger.Signature
                                                -- ^ The signed message
                                                -> Bool
                                            checkSignature !paymentPubKey !signedMsgBBS !signature =
                                                    let
                                                        !pubKey= Ledger.unPaymentPubKey paymentPubKey
                                                        !lb = Ledger.getPubKey pubKey
                                                        !bbs = LedgerApiV2.getLedgerBytes lb
                                                        !sig = Ledger.getSignature signature
                                                    in  verifyEd25519Signature bbs signedMsgBBS sig
                                            ------------------
                                        in  checkSignature oraclePaymentPubKey priceData oracle_Signature 
                                    ------------------
                                    isCorrect_Oracle_InRangeTime :: T.Oracle_Data -> Bool
                                    isCorrect_Oracle_InRangeTime oracle_Data =
                                        let
                                            ------------------
                                            validRange = LedgerApiV2.txInfoValidRange info
                                            ------------------
                                            newLowerLimitValue :: LedgerApiV2.POSIXTime
                                            newLowerLimitValue = case Ledger.ivFrom validRange of
                                                Ledger.LowerBound (Ledger.Finite a) True -> a - T.oracleData_Valid_Time
                                                _                                        -> traceError "Interval has no lower bound"
                                            ------------------
                                            newInterval = Ledger.Interval (Ledger.LowerBound (Ledger.Finite newLowerLimitValue) True) (Ledger.ivTo validRange )
                                            -- TODO: la valides de la transaccion hay que ponerla en 3 minutos
                                        in
                                            T.odTime oracle_Data `Ledger.member` newInterval
                                    ------------------
                                    isCorrect_Conversion :: T.Oracle_Data -> Integer -> Integer -> Bool
                                    isCorrect_Conversion oracle_Data !amount_FT !amount_ADA =
                                        let
                                            !(cs, tn, priceADAx1e6) = head $ T.iuValues $ T.odFTPriceADA1xe6 oracle_Data
                                            !price_FT_in_ADA =
                                                if cs == fundPolicy_CS && tn == fundFT_TN
                                                    then priceADAx1e6
                                                    else traceError "FT Price ADA not found in Oracle Data"
                                            --------
                                            amount_ADA' = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp amount_FT price_FT_in_ADA
                                            --------
                                        in
                                            amount_ADA == amount_ADA'
                                    ------------------
                                    isAmount_FT_Available:: Integer -> Bool
                                    isAmount_FT_Available !amount_FT =
                                        let
                                            !amount_FT_Available = T.sodAmount_FT_Available sellOffer_Datum_In
                                        in
                                            amount_FT_Available >= amount_FT
                                    ------------------
                                    isAmount_ADA_Available:: Integer -> Bool
                                    isAmount_ADA_Available !amount_ADA =
                                        let
                                            !amount_ADA_Available = T.sodAmount_ADA_Available sellOffer_Datum_In
                                        in
                                            amount_ADA_Available >= amount_ADA
                                    ------------------
                                    isCorrect_Commission :: Integer -> Integer -> Bool
                                    isCorrect_Commission !swap_Amount !commission_Payed  =
                                        -- las comisiones son en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
                                        -- eso significa que al valor de Commission_InBPx1e3 tengo que dividirlo por
                                        -- 10e2 para pasarlo a bp
                                        -- 100 para pasarlo a porcentaje normal del 1 al 100
                                        -- 100 para pasarlo a porcentaje del 0 al 1
                                        -- den = 1e3 * 100 * 100 = 1000 * 100 * 100 = 10_000_000
                                        let
                                            !commissionRate = T.sodAskedCommission_InBPx1e3 sellOffer_Datum_In
                                            --------
                                            commission' = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundUp swap_Amount commissionRate 
                                            --------
                                        in
                                            commission' == commission_Payed
                            ------------------

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_StatusChanged #-}
mkUpdated_SellOffer_Datum_With_StatusChanged :: T.SellOffer_DatumType -> Integer -> T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_StatusChanged !sellOffer_Datum_In !newStatus =
    sellOffer_Datum_In { T.sodOrder_Status = newStatus}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_CommissionChanged #-}
mkUpdated_SellOffer_Datum_With_CommissionChanged :: T.SellOffer_DatumType -> Integer -> T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_CommissionChanged !sellOffer_Datum_In !newCommissionRate =
    sellOffer_Datum_In { T.sodAskedCommission_InBPx1e3 = newCommissionRate}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_RestrictionsChanged #-}
mkUpdated_SellOffer_Datum_With_RestrictionsChanged :: T.SellOffer_DatumType -> Integer -> Integer ->  T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_RestrictionsChanged !sellOffer_Datum_In !newAllowSellFT !newAllowSellADA =
    sellOffer_Datum_In { T.sodOrder_AllowSellADA= newAllowSellADA, T.sodOrder_AllowSellFT = newAllowSellFT}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_MinADAChanged #-}
mkUpdated_SellOffer_Datum_With_MinADAChanged :: T.SellOffer_DatumType -> Integer -> T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_MinADAChanged !sellOffer_Datum_In !newMinADA =
    sellOffer_Datum_In { T.sodMinADA = newMinADA}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_Deposit #-}
mkUpdated_SellOffer_Datum_With_Deposit :: T.SellOffer_DatumType -> Integer -> Integer ->  T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_Deposit !sellOffer_Datum_In !newDeposit_FT !newDeposit_ADA =
    sellOffer_Datum_In {
        T.sodAmount_FT_Available = T.sodAmount_FT_Available sellOffer_Datum_In + newDeposit_FT,
        T.sodAmount_ADA_Available = T.sodAmount_ADA_Available sellOffer_Datum_In + newDeposit_ADA
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_Withdraw #-}
mkUpdated_SellOffer_Datum_With_Withdraw :: T.SellOffer_DatumType -> Integer -> Integer -> T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_Withdraw !sellOffer_Datum_In !newWithdraw_FT !newWithdraw_ADA =
    sellOffer_Datum_In {
        T.sodAmount_FT_Available = T.sodAmount_FT_Available sellOffer_Datum_In - newWithdraw_FT,
        T.sodAmount_ADA_Available = T.sodAmount_ADA_Available sellOffer_Datum_In - newWithdraw_ADA
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_SwapFTxADA #-}
mkUpdated_SellOffer_Datum_With_SwapFTxADA :: T.SellOffer_DatumType -> Integer ->Integer -> Integer ->T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_SwapFTxADA !sellOffer_Datum_In !amount_FT !amount_ADA !commission_ADA =
    sellOffer_Datum_In {
        T.sodAmount_FT_Available = T.sodAmount_FT_Available sellOffer_Datum_In + amount_FT,
        T.sodAmount_ADA_Available = T.sodAmount_ADA_Available sellOffer_Datum_In - (amount_ADA - commission_ADA),
        T.sodTotal_ADA_Earned = T.sodTotal_ADA_Earned sellOffer_Datum_In + commission_ADA
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_SwapADAxFT #-}
mkUpdated_SellOffer_Datum_With_SwapADAxFT :: T.SellOffer_DatumType -> Integer -> Integer ->Integer -> T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_SwapADAxFT !sellOffer_Datum_In !amount_ADA !amount_FT !commission_FT =
    sellOffer_Datum_In {
        T.sodAmount_FT_Available = T.sodAmount_FT_Available sellOffer_Datum_In - (amount_FT - commission_FT),
        T.sodAmount_ADA_Available = T.sodAmount_ADA_Available sellOffer_Datum_In + amount_ADA,
        T.sodTotal_FT_Earned = T.sodTotal_FT_Earned sellOffer_Datum_In + commission_FT
    }

----------------------------------------------------------------------------

{-# INLINEABLE policyID #-}
policyID :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policyID params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyID||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params


{-# INLINABLE  mkWrappedPolicyID #-}
mkWrappedPolicyID :: BuiltinData -> BuiltinData ->BuiltinData ->  BuiltinData -> BuiltinData ->BuiltinData ->()
mkWrappedPolicyID protocolPolicyID_CS sellOffer_Validator_Hash tokenMAYZ_CS tokenMAYZ_TN = mkPolicyID params
    where
        params = T.PolicyParams
            {
                ppProtocolPolicyID_CS   =PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS,
                ppSellOffer_Validator_Hash =PlutusTx.unsafeFromBuiltinData sellOffer_Validator_Hash,
                ppTokenMAYZ_AC =LedgerValue.AssetClass (PlutusTx.unsafeFromBuiltinData tokenMAYZ_CS, PlutusTx.unsafeFromBuiltinData tokenMAYZ_TN)
            }

policyIDCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->BuiltinData ->BuiltinData -> ())
policyIDCode = $$( PlutusTx.compile [|| mkWrappedPolicyID ||])

----------------------------------------------------------------------------

{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS tokenEmergencyAdminPolicy_CS = mkValidator params
    where
        params =
            T.ValidatorParams
                { vpProtocolPolicyID_CS = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
                , vpTokenEmergencyAdminPolicy_CS = PlutusTx.unsafeFromBuiltinData tokenEmergencyAdminPolicy_CS
                }

validatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(PlutusTx.compile [||mkWrappedValidator||])

------------------------------------------------------------------------------
