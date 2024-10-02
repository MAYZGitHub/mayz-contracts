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

module Protocol.BuyOrder.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Maybe                as DataMaybe
import qualified Ledger
import qualified Ledger.Ada                as LedgerAda
import qualified Ledger.Value              as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap         as TxAssocMap
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Constants         as T
import qualified Generic.OnChainHelpers    as OnChainHelpers
import qualified Protocol.BuyOrder.Types   as T
import qualified Protocol.Constants        as T
import qualified Protocol.Fund.Types       as FundT
import qualified Protocol.OnChainHelpers   as OnChainHelpers
import qualified Protocol.Protocol.Types   as ProtocolT
import qualified Protocol.Types            as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID  (T.PolicyParams !protocolPolicyID_CS !buyOrder_Validator_Hash !tokenMAYZ_AC) !redRaw !ctxRaw =
    let
        ------------------
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !buyOrder_Validator_Address = Ledger.scriptHashAddress buyOrder_Validator_Hash
        ------------------
        !buyOrderPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !buyOrder_ID_AC = LedgerValue.AssetClass (buyOrderPolicyID_CS, T.buyOrderID_TN)
        ------------------
        !valueFor_Mint_BuyOrder_ID = LedgerValue.assetClassValue buyOrder_ID_AC 1
        ------------------
    in  if
            traceIfFalse "" useThisToMakeScriptUnique
            && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
            &&
            case redeemer of
                T.PolicyRedeemerMintID _ ->
                        ---------------------
                        -- it runs alone
                        ---------------------
                        traceIfFalse "not isMintingBuyOrderID" isMintingBuyOrderID &&
                        traceIfFalse "not isCorrect_Output_BuyOrder_Datum" isCorrect_Output_BuyOrder_Datum &&
                        traceIfFalse "not isCorrect_Output_BuyOrder_Value" isCorrect_Output_BuyOrder_Value &&
                        traceIfFalse "expected zero BuyOrder inputs" (null inputs_Own_TxOuts)
                        ---------------------
                    where
                        ------------------
                        !inputsRef_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoReferenceInputs info ,
                                        OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput )]
                        ------------------
                        !inputs_Own_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info,
                                        let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                        in  OnChainHelpers.isScriptAddress address && address == buyOrder_Validator_Address]
                        ------------------
                        !outputs_Own_TxOuts = [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info,
                                        let address = LedgerApiV2.txOutAddress txOut
                                        in  OnChainHelpers.isScriptAddress address && address == buyOrder_Validator_Address ]
                        ------------------
                        !output_Own_TxOut_And_BuyOrder_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                @T.ValidatorDatum @T.BuyOrder_DatumType
                                ctx
                                outputs_Own_TxOuts
                                buyOrder_ID_AC
                                T.getBuyOrder_DatumType of
                                [x] -> x
                                _   -> traceError "Expected exactly one BuyOrder output"
                        ------------------
                        !buyOrder_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                        ---------------------
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
                        !commissionBuyOrder_InBPx1e3 = ProtocolT.pdCommissionBuyOrder_InBPx1e3 protocolDatum_In
                        ---------------------
                        !requiredMAYZ = ProtocolT.pdRequiredMAYZForBuyOrder protocolDatum_In
                        ---------------------
                        !valueOf_RequiredMAYZ = LedgerValue.assetClassValue tokenMAYZ_AC requiredMAYZ
                        ---------------------
                        isMintingBuyOrderID :: Bool
                        !isMintingBuyOrderID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_BuyOrder_ID
                        -----------------
                        isCorrect_Output_BuyOrder_Datum :: Bool
                        !isCorrect_Output_BuyOrder_Datum =
                            let !buyOrder_Datum_Out_Control =
                                    T.mkBuyOrder_DatumType
                                        buyOrderPolicyID_CS
                                        (T.bodFundPolicy_CS buyOrder_Datum_Out)
                                        (T.bodBuyerPaymentPKH buyOrder_Datum_Out)
                                        (T.bodBuyerStakePKH buyOrder_Datum_Out)
                                        (T.bodOfferedCommission_InBPx1e3 buyOrder_Datum_Out)
                                        0
                                        0
                                        T.buyOrder_Status_Open
                                        (T.bodMinADA buyOrder_Datum_Out)
                            in  buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
                                && traceIfFalse "not isInRange commissionBuyOrder_InBPx1e3" (ProtocolT.isInRange commissionBuyOrder_InBPx1e3 (T.bodOfferedCommission_InBPx1e3 buyOrder_Datum_Out))
                        ------------------
                        isCorrect_Output_BuyOrder_Value :: Bool
                        !isCorrect_Output_BuyOrder_Value =
                            let
                                !minADA_For_BuyOrder_Datum = T.bodMinADA buyOrder_Datum_Out
                                !value_MinADA_For_BuyOrder_Datum = LedgerAda.lovelaceValueOf minADA_For_BuyOrder_Datum
                                ---------------------
                                !valueFor_BuyOrder_Datum_Out_Control = valueFor_Mint_BuyOrder_ID <> value_MinADA_For_BuyOrder_Datum <> valueOf_RequiredMAYZ
                                ---------------------
                                !valueOf_BuyOrder_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                                ---------------------
                                !currentMAYZ = OnChainHelpers.getAmt_With_AC_InValue valueOf_BuyOrder_Out tokenMAYZ_AC
                                ---------------------
                            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Datum_Out_Control
                                && traceIfFalse "not currentMAYZ == requiredMAYZ" (currentMAYZ == requiredMAYZ)
                        ------------------
                T.PolicyRedeemerBurnID _ ->
                        ---------------------
                        -- it runs along with Buy Order Validator (ValidatorRedeemerDelete)
                        ---------------------
                        traceIfFalse "not isBurningBuyOrderID" isBurningBuyOrderID
                        ---------------------
                    where
                        ------------------
                        !valueFor_Burn_BuyOrder_ID = LedgerValue.assetClassValue buyOrder_ID_AC (negate 1)
                        ---------------------
                        isBurningBuyOrderID :: Bool
                        isBurningBuyOrderID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_BuyOrder_ID
                        -----------------
            then ()
            else error ()


--------------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator (T.ValidatorParams !protocolPolicyID_CS) !datumRaw !redRaw !ctxRaw =
    if  traceIfFalse "" useThisToMakeScriptUnique
        && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
        && traceIfFalse "Expected exactly one BuyOrder input" (length inputs_Own_TxOuts == 1)
        && validateRedeemerDeleteAndOthers
            then ()
            else error ()
    where
        ---------------------
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
        ------------------
        !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
        !buyOrder_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
        ------------------
        !inputs_Own_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info,
                        let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                        in  OnChainHelpers.isScriptAddress address && address == buyOrder_Validator_Address]
        ------------------
        !buyOrder_Datum_In = T.getBuyOrder_DatumType datum
        ------------------
        !fundPolicy_CS = T.bodFundPolicy_CS buyOrder_Datum_In
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        ---------------------
        !buyOrderPolicyID_CS = T.bodBuyOrderPolicyID_CS buyOrder_Datum_In
        !buyOrderID_AC = LedgerValue.AssetClass (buyOrderPolicyID_CS, T.buyOrderID_TN)
        ------------------
        !admin = T.bodBuyerPaymentPKH buyOrder_Datum_In
        ------------------
        !redeemerUpdateStatus = 1
        !redeemerUpdateOfferedCommissionRate = 2
        !redeemerUpdateMinADA = 3
        !redeemerDeposit = 4
        !redeemerWithdraw = 5
        !redeemerFillOrder = 6
        !redeemerDelete = 7
        ------------------
        redeemerType :: Integer
        !redeemerType = case redeemer of
            (T.ValidatorRedeemerUpdateStatus _)                -> redeemerUpdateStatus
            (T.ValidatorRedeemerUpdateOfferedCommissionRate _) -> redeemerUpdateOfferedCommissionRate
            (T.ValidatorRedeemerUpdateMinADA _)                -> redeemerUpdateMinADA
            (T.ValidatorRedeemerDeposit _)                     -> redeemerDeposit
            (T.ValidatorRedeemerWithdraw _)                    -> redeemerWithdraw
            (T.ValidatorRedeemerFillOrder _)                   -> redeemerFillOrder
            (T.ValidatorRedeemerDelete _)                      -> redeemerDelete
        ------------------
        validateRedeemerDeleteAndOthers :: Bool
        !validateRedeemerDeleteAndOthers
            | redeemerType == redeemerDelete = validateAdminAction && validateRedeemerDelete
            | otherwise = validateAllRedeemersButDelete
        ------------------
        validateAdminAction :: Bool
        validateAdminAction = traceIfFalse "not txSignedBy admin" (LedgerContextsV2.txSignedBy info admin)
        ------------------
        validateRedeemerDelete :: Bool
        validateRedeemerDelete =
                ---------------------
                -- it runs along with Buy Order Policy ID (PolicyRedeemerBurnID)
                ---------------------
                traceIfFalse "not isBurningBuyOrderID" isBurningBuyOrderID
                ------------------
            where
                ------------------
                isBurningBuyOrderID :: Bool
                isBurningBuyOrderID  = OnChainHelpers.isNFT_Burning_With_AC buyOrderID_AC info
                ------------------
        validateAllRedeemersButDelete :: Bool
        validateAllRedeemersButDelete
                ------------------
                | redeemerType == redeemerUpdateStatus = validateAdminAction && validateRedeemerUpdateStatus redeemer
                | redeemerType == redeemerUpdateOfferedCommissionRate  = validateAdminAction && validatRedeemerUpdateOfferedCommissionRate redeemer
                | redeemerType == redeemerUpdateMinADA = validateAdminAction && validateRedeemerUpdateMinADA redeemer
                | redeemerType == redeemerDeposit = validateAdminAction && validateRedeemerDeposit redeemer
                | redeemerType == redeemerWithdraw = validateAdminAction && validateRedeemerWithdraw redeemer
                | redeemerType == redeemerFillOrder = validateRedeemerFillOrder redeemer
                | otherwise = False
                ------------------
            where
                ------------------
                !inputsRef_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoReferenceInputs info,
                        OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput )]
                ------------------
                !outputs_Own_TxOuts = [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info,
                                        let address = LedgerApiV2.txOutAddress txOut
                                        in  OnChainHelpers.isScriptAddress address && address == buyOrder_Validator_Address ]
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
                !output_Own_TxOut_And_BuyOrder_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                        @T.ValidatorDatum @T.BuyOrder_DatumType
                        ctx
                        outputs_Own_TxOuts
                        buyOrderID_AC
                        T.getBuyOrder_DatumType  of
                        [x] -> x
                        _   -> traceError "Expected exactly one BuyOrder output"
                ------------------
                !buyOrder_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                ------------------
                !valueOf_BuyOrder_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                ------------------
                getLazyProtocolDatum_In :: ProtocolT.ProtocolDatumType
                getLazyProtocolDatum_In =
                    -- is not executed now, only if needed in some redeemers
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
                    in protocolDatum_In
                ------------------
                isOrderOpen :: Bool
                !isOrderOpen = T.bodOrder_Status buyOrder_Datum_In == T.buyOrder_Status_Open
                ----------------
                isCorrect_Output_BuyOrder_Value_NotChanged :: Bool
                isCorrect_Output_BuyOrder_Value_NotChanged =
                    let !valueFor_BuyOrder_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated
                    in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Out_Control
                ----------------
                isCorrect_Output_BuyOrder_Datum_NotChanged :: Bool
                isCorrect_Output_BuyOrder_Datum_NotChanged = buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_In
                ------------------
                convertToMap :: T.InvestUnit -> TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer)
                convertToMap (T.InvestUnit tokens) = foldl insertToken TxAssocMap.empty tokens
                    where
                        insertToken acc (cs, tn, i) =
                            let innerMap = DataMaybe.fromMaybe TxAssocMap.empty (TxAssocMap.lookup cs acc)
                                updatedInnerMap = TxAssocMap.insert tn (maybe i (+ i) (TxAssocMap.lookup tn innerMap)) innerMap
                            in TxAssocMap.insert cs updatedInnerMap acc
                ------------------
                convertToValue :: T.InvestUnit -> LedgerValue.Value
                convertToValue iu = LedgerValue.Value (convertToMap iu)
                ------------------
                validateRedeemerUpdateStatus ::  T.ValidatorRedeemer  -> Bool
                validateRedeemerUpdateStatus (T.ValidatorRedeemerUpdateStatus (T.ValidatorRedeemerUpdateStatusType !newStatus))  =
                        ---------------------
                        -- it runs alone
                        ---------------------
                        traceIfFalse "not isCorrect_Output_BuyOrder_Datum_With_StatusChanged" isCorrect_Output_BuyOrder_Datum_With_StatusChanged
                        && traceIfFalse "not isCorrect_Output_BuyOrder_Value_NotChanged" isCorrect_Output_BuyOrder_Value_NotChanged
                        ------------------
                    where
                        ------------------
                        isCorrect_Output_BuyOrder_Datum_With_StatusChanged :: Bool
                        !isCorrect_Output_BuyOrder_Datum_With_StatusChanged  =
                            let !buyOrder_Datum_Out_Control = mkUpdated_BuyOrder_Datum_With_StatusChanged buyOrder_Datum_In newStatus
                            in   buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
                        ------------------
                validateRedeemerUpdateStatus _   = False
                ------------------
                validatRedeemerUpdateOfferedCommissionRate ::  T.ValidatorRedeemer  -> Bool
                validatRedeemerUpdateOfferedCommissionRate (T.ValidatorRedeemerUpdateOfferedCommissionRate (T.ValidatorRedeemerUpdateOfferedCommissionRateType !newCommissionRate))  =
                        ---------------------
                        -- it runs alone
                        ---------------------
                        traceIfFalse "not isCorrect_Output_BuyOrder_Datum_With_CommissionChanged" isCorrect_Output_BuyOrder_Datum_With_CommissionChanged
                        && traceIfFalse "not isCorrect_Output_BuyOrder_Value_NotChanged" isCorrect_Output_BuyOrder_Value_NotChanged
                        && traceIfFalse "not isInRange commissionBuyOrder_InBPx1e3" (ProtocolT.isInRange commissionBuyOrder_InBPx1e3 (T.bodOfferedCommission_InBPx1e3 buyOrder_Datum_Out))
                        ------------------
                    where
                        ------------------
                        !commissionBuyOrder_InBPx1e3 = ProtocolT.pdCommissionBuyOrder_InBPx1e3 getLazyProtocolDatum_In
                        ---------------------
                        isCorrect_Output_BuyOrder_Datum_With_CommissionChanged :: Bool
                        !isCorrect_Output_BuyOrder_Datum_With_CommissionChanged =
                            let !buyOrder_Datum_Out_Control = mkUpdated_BuyOrder_Datum_With_CommissionChanged buyOrder_Datum_In newCommissionRate
                            in   buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
                        ------------------
                validatRedeemerUpdateOfferedCommissionRate _   = False
                ------------------
                validateRedeemerUpdateMinADA ::  T.ValidatorRedeemer  -> Bool
                validateRedeemerUpdateMinADA (T.ValidatorRedeemerUpdateMinADA _)  =
                        ---------------------
                        -- it runs alone
                        ---------------------
                        traceIfFalse "not isCorrect_Output_BuyOrder_Datum_With_MinADAChanged" isCorrect_Output_BuyOrder_Datum_With_MinADAChanged
                        && traceIfFalse "not isCorrect_Output_BuyOrder_Value_With_MinADAChanged" isCorrect_Output_BuyOrder_Value_With_MinADAChanged
                        ------------------
                    where
                        ------------------
                        !newMinADA = T.bodMinADA buyOrder_Datum_Out
                        ------------------
                        isCorrect_Output_BuyOrder_Datum_With_MinADAChanged:: Bool
                        isCorrect_Output_BuyOrder_Datum_With_MinADAChanged  =
                            let !buyOrder_Datum_Out_Control = mkUpdated_BuyOrder_Datum_With_MinADAChanged buyOrder_Datum_In newMinADA
                            in   buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
                        ------------------
                        isCorrect_Output_BuyOrder_Value_With_MinADAChanged ::  Bool
                        isCorrect_Output_BuyOrder_Value_With_MinADAChanged  =
                            let !valueFor_BuyOrder_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> LedgerAda.lovelaceValueOf (newMinADA - T.bodMinADA buyOrder_Datum_In)
                            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Out_Control
                validateRedeemerUpdateMinADA _   = False
                ------------------
                validateRedeemerDeposit :: T.ValidatorRedeemer  -> Bool
                validateRedeemerDeposit  (T.ValidatorRedeemerDeposit (T.ValidatorRedeemerDepositType !newDeposit))  =
                        ---------------------
                        -- it runs alone
                        ---------------------
                        traceIfFalse "not isCorrect_Output_BuyOrder_Datum_NotChanged" isCorrect_Output_BuyOrder_Datum_NotChanged
                        && traceIfFalse "not isCorrect_Output_BuyOrder_Value_With_Deposit" isCorrect_Output_BuyOrder_Value_With_Deposit
                        ------------------
                    where
                        ------------------
                        isCorrect_Output_BuyOrder_Value_With_Deposit :: Bool
                        isCorrect_Output_BuyOrder_Value_With_Deposit =
                            let !value_Deposit_Tokens = convertToValue newDeposit
                            ------------------
                                !valueFor_BuyOrder_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> value_Deposit_Tokens
                            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Out_Control
                        ----------------
                validateRedeemerDeposit _   = False
                ------------------
                validateRedeemerWithdraw ::  T.ValidatorRedeemer  -> Bool
                validateRedeemerWithdraw (T.ValidatorRedeemerWithdraw (T.ValidatorRedeemerWithdrawType !newWithdraw))  =
                        ---------------------
                        -- it runs alone
                        ---------------------
                        traceIfFalse "not isCorrect_Output_BuyOrder_Datum_NotChanged" isCorrect_Output_BuyOrder_Datum_NotChanged
                        && traceIfFalse "not isCorrect_Output_BuyOrder_Value_With_Withdraw" isCorrect_Output_BuyOrder_Value_With_Withdraw
                        ------------------
                    where
                        ------------------
                        isCorrect_Output_BuyOrder_Value_With_Withdraw :: Bool
                        isCorrect_Output_BuyOrder_Value_With_Withdraw =
                            let !value_Withdraw_Tokens = convertToValue newWithdraw
                            ------------------
                                !valueFor_BuyOrder_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> negate value_Withdraw_Tokens
                            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Out_Control
                        ----------------
                validateRedeemerWithdraw _   = False
                ------------------
                validateRedeemerFillOrder ::  T.ValidatorRedeemer  -> Bool
                validateRedeemerFillOrder (T.ValidatorRedeemerFillOrder (T.ValidatorRedeemerFillOrderType !rfoAmount_Tokens !rfoAmount_FT !rfoCommission_FT !rfoOracle_Data !rfoOracle_Signature))  =
                        ---------------------
                        -- it runs alone
                        ---------------------
                        traceIfFalse "not isOrderOpen" isOrderOpen
                        && traceIfFalse "not isCorrect_Oracle_Signature" (OnChainHelpers.isCorrect_Oracle_Signature priceData oraclePaymentPubKey rfoOracle_Signature)
                        && traceIfFalse "not isCorrect_Oracle_InRangeTime" (OnChainHelpers.isCorrect_Oracle_InRangeTime info (T.odTime rfoOracle_Data) )
                        && traceIfFalse "not isCorrect_Conversion" (isCorrect_Conversion rfoOracle_Data rfoAmount_Tokens rfoAmount_FT )
                        && traceIfFalse "not isCorrect_Commission" (isCorrect_Commission rfoAmount_FT rfoCommission_FT)
                        && traceIfFalse "not isAmount_Tokens_Available" (isAmount_Tokens_Available rfoAmount_Tokens)
                        && traceIfFalse "not isCorrect_Output_BuyOrder_Datum_With_FillOrder" (isCorrect_Output_BuyOrder_Datum_With_FillOrder rfoAmount_FT rfoAmount_Tokens rfoCommission_FT)
                        && traceIfFalse "not isCorrect_Output_BuyOrder_Value_With_FillOrder" (isCorrect_Output_BuyOrder_Value_With_FillOrder rfoAmount_FT rfoAmount_Tokens rfoCommission_FT)
                        ------------------
                    where
                        ------------------
                        isCorrect_Output_BuyOrder_Datum_With_FillOrder:: Integer -> T.InvestUnit -> Integer -> Bool
                        isCorrect_Output_BuyOrder_Datum_With_FillOrder !amount_FT !amount_Tokens !commission_FT =
                            let !buyOrder_Datum_Out_Control = mkUpdated_BuyOrder_Datum_With_FillOrder buyOrder_Datum_In amount_FT amount_Tokens commission_FT
                            in  buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
                        ----------------
                        isCorrect_Output_BuyOrder_Value_With_FillOrder ::  Integer ->  T.InvestUnit ->  Integer -> Bool
                        isCorrect_Output_BuyOrder_Value_With_FillOrder !amount_FT  !amount_Tokens !commission_FT  =
                            let !value_FillOrder_Tokens = convertToValue amount_Tokens
                                !value_Amount_FT = LedgerValue.assetClassValue fundFT_TN_AC (amount_FT - commission_FT)
                            ------------------
                                !valueFor_BuyOrder_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> value_Amount_FT <> negate value_FillOrder_Tokens
                            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Out_Control
                        ------------------
                        !priceData = OnChainHelpers.oracleDataToBBS rfoOracle_Data
                        ------------------
                        !oraclePaymentPubKey = ProtocolT.pdOraclePaymentPubKey getLazyProtocolDatum_In
                        ----------------
                        -- Calculates the total price of swapTokens based on oraclePrices
                        totalSwapPrice :: TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer) -- ^ Oracle prices
                                    -> TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer) -- ^ Swap tokens
                                    -> Integer -- ^ Total price
                        totalSwapPrice oraclePrices swapTokens =
                            PlutusTx.Prelude.foldl (\acc cs ->
                                PlutusTx.Prelude.foldl (\innerAcc (tn, amount) ->
                                    let price = DataMaybe.fromMaybe 0 (TxAssocMap.lookup cs oraclePrices >>= TxAssocMap.lookup tn)
                                    in innerAcc + (price * amount)
                                ) 0 (TxAssocMap.toList $ DataMaybe.fromMaybe TxAssocMap.empty (TxAssocMap.lookup cs swapTokens)) + acc
                            ) 0 (TxAssocMap.keys swapTokens)
                        ------------------
                        -- Looks up the price of a specific token in the oracle prices
                        lookupPrice :: LedgerApiV2.CurrencySymbol
                                    -> LedgerApiV2.TokenName
                                    -> TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer) -- ^ Oracle prices
                                    -> Maybe Integer
                        lookupPrice cs tn oraclePrices =
                            TxAssocMap.lookup cs oraclePrices >>= TxAssocMap.lookup tn
                        ------------------
                        isCorrect_Conversion :: T.Oracle_Data -> T.InvestUnit -> Integer -> Bool
                        isCorrect_Conversion oracle_Data !amount_Tokens !amount_FT =
                            let
                                oraclePrices = convertToMap $ T.odFTPriceADA1xe6 oracle_Data
                                swapTokens = convertToMap amount_Tokens
                                price_Tokens_in_ADA = totalSwapPrice oraclePrices swapTokens
                                price_FT_in_ADA = case lookupPrice fundPolicy_CS fundFT_TN oraclePrices of
                                        Just priceADA -> priceADA
                                        _             -> traceError "FT Price ADA not found in Oracle Data"
                            in
                                price_Tokens_in_ADA == amount_FT * price_FT_in_ADA
                        ------------------
                        isCorrect_Commission :: Integer -> Integer -> Bool
                        isCorrect_Commission !amount_FT !commission_FT_Payed  =
                            -- las comisiones son en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
                            -- eso significa que al valor de Commission_InBPx1e3 tengo que dividirlo por
                            -- 10e2 para pasarlo a bp
                            -- 100 para pasarlo a porcentaje normal del 1 al 100
                            -- 100 para pasarlo a porcentaje del 0 al 1
                            -- den = 1e3 * 100 * 100 = 1000 * 100 * 100 = 10_000_000
                            let
                                com = T.bodOfferedCommission_InBPx1e3 buyOrder_Datum_In
                                den = 10_000_000
                            in
                                (amount_FT * com) `divide` den  == commission_FT_Payed
                        ------------------
                        isAmount_Tokens_Available :: T.InvestUnit -> Bool
                        isAmount_Tokens_Available !amount_Tokens =
                            let
                                !value_Swap_Tokens = convertToValue amount_Tokens
                            in
                                value_Swap_Tokens `OnChainHelpers.isIncludeValue` LedgerApiV2.txOutValue input_TxOut_BeingValidated
                validateRedeemerFillOrder _   = False
                ------------------


----------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_BuyOrder_Datum_With_StatusChanged #-}
mkUpdated_BuyOrder_Datum_With_StatusChanged :: T.BuyOrder_DatumType -> Integer -> T.BuyOrder_DatumType
mkUpdated_BuyOrder_Datum_With_StatusChanged !buyOrder_Datum_In !newStatus =
    buyOrder_Datum_In { T.bodOrder_Status = newStatus }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_BuyOrder_Datum_With_CommissionChanged #-}
mkUpdated_BuyOrder_Datum_With_CommissionChanged :: T.BuyOrder_DatumType -> Integer -> T.BuyOrder_DatumType
mkUpdated_BuyOrder_Datum_With_CommissionChanged !buyOrder_Datum_In !newCommissionRate =
    buyOrder_Datum_In { T.bodOfferedCommission_InBPx1e3 = newCommissionRate }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_BuyOrder_Datum_With_MinADAChanged #-}
mkUpdated_BuyOrder_Datum_With_MinADAChanged :: T.BuyOrder_DatumType -> Integer -> T.BuyOrder_DatumType
mkUpdated_BuyOrder_Datum_With_MinADAChanged !buyOrder_Datum_In !newMinADA =
    buyOrder_Datum_In { T.bodMinADA = newMinADA }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_BuyOrder_Datum_With_FillOrder #-}
mkUpdated_BuyOrder_Datum_With_FillOrder :: T.BuyOrder_DatumType -> Integer ->T.InvestUnit->   Integer ->T.BuyOrder_DatumType
mkUpdated_BuyOrder_Datum_With_FillOrder !buyOrder_Datum_In  !amount_FT _ !commission_FT =
    buyOrder_Datum_In { T.bodFT_Received = T.bodFT_Received buyOrder_Datum_In + (amount_FT - commission_FT),
                        T.bodFT_PayedAsCommission = T.bodFT_PayedAsCommission buyOrder_Datum_In + commission_FT }

--------------------------------------------------------------------------------2

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
mkWrappedPolicyID protocolPolicyID_CS buyOrder_Validator_Hash tokenMAYZ_CS tokenMAYZ_TN = mkPolicyID params
    where
        params = T.PolicyParams
            {
                ppProtocolPolicyID_CS   =PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS,
                ppBuyOrder_Validator_Hash =PlutusTx.unsafeFromBuiltinData buyOrder_Validator_Hash,
                ppTokenMAYZ_AC =LedgerValue.AssetClass (PlutusTx.unsafeFromBuiltinData tokenMAYZ_CS, PlutusTx.unsafeFromBuiltinData tokenMAYZ_TN)
            }

policyIDCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->BuiltinData ->BuiltinData -> ())
policyIDCode = $$( PlutusTx.compile [|| mkWrappedPolicyID ||])

--------------------------------------------------------------------------------2


{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS  = mkValidator params
    where
        params = T.ValidatorParams
            {
            vpProtocolPolicyID_CS  = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
            }

validatorCode :: PlutusTx.CompiledCode ( BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( PlutusTx.compile [|| mkWrappedValidator ||])


------------------------------------------------------------------------------
