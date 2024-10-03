{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.InvestUnit.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger.Ada                  as LedgerAda
import qualified Ledger.Value                as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts   as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Constants           as T
import qualified Generic.OnChainHelpers      as OnChainHelpers
import qualified Protocol.Constants          as T
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.Types         as FundT
import qualified Protocol.InvestUnit.Types   as T
import qualified Protocol.OnChainHelpers     as OnChainHelpers
import qualified Protocol.Protocol.Types     as ProtocolT
import qualified Protocol.Types              as T

--------------------------------------------------------------------------------2
-- Modulo
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
                    && traceIfFalse "Expected exactly one InvestUnit input" (length inputs_Own_TxOuts == 1)
                    && validateAdminAction && validateRedeemerAdmin
                    then ()
                    else error ()
                where
                        ------------------
                        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
                        ------------------
                        !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
                        ------------------
                        !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
                        !investUnit_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
                        ------------------
                        !inputs_Own_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info,
                            let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                            in  OnChainHelpers.isScriptAddress address && address == investUnit_Validator_Address]
                        ------------------
                        !investUnitDatum_In = T.getInvestUnit_DatumType datum
                        ------------------
                        !fundPolicy_CS = T.iudFundPolicy_CS investUnitDatum_In
                        ------------------
                        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
                        ------------------
                        !valueOf_InvestUnitDatum_In = LedgerApiV2.txOutValue input_TxOut_BeingValidated
                        ------------------
                        !inputsRef_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput| txInfoInput <- LedgerApiV2.txInfoReferenceInputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput )]
                        ------------------
                        !inputs_TxOutRefs_TxOuts = [(LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | !txInfoInput <- LedgerApiV2.txInfoInputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput )]
                        ------------------
                        !outputs_txOuts = [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)  ]
                        ------------------
                        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                        ------------------
                        getFundDatumAndAdmins :: T.ValidatorRedeemer -> FundT.FundDatumType
                        getFundDatumAndAdmins redeemer'  =
                            case redeemer' of
                                T.ValidatorRedeemerDelete _ -> 
                                    let !inputs_Others_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info,
                                            let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                            in  OnChainHelpers.isScriptAddress address && address /= investUnit_Validator_Address]
                                        input_TxOut_And_FundDatum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                            @FundT.ValidatorDatum @FundT.FundDatumType
                                            ctx
                                            inputs_Others_TxOuts
                                            fundID_AC
                                            FundT.getFund_DatumType of
                                                [x] -> x
                                                _   -> traceError "Expected exactly one Fund input"
                                        fundDatum_In' = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundDatum
                                    in fundDatum_In'
                                _ -> 
                                    let inputRef_TxOut_And_FundDatum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                            @FundT.ValidatorDatum @FundT.FundDatumType
                                            ctx
                                            inputsRef_TxOuts
                                            fundID_AC
                                            FundT.getFund_DatumType of
                                                [x] -> x
                                                _   -> traceError "Expected exactly one Fund input ref"
                                        fundDatum_In' = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_FundDatum
                                    in fundDatum_In'
                        ------------------
                        !fundDatum_In = getFundDatumAndAdmins redeemer
                        ------------------
                        validateAdminAction :: Bool
                        !validateAdminAction =
                                traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent" (OnChainHelpers.isSignedByAny admins info || isAdminTokenPresent)
                            where
                                ------------------
                                !admins = T.getAdmins fundDatum_In
                                ------------------
                                isAdminTokenPresent :: Bool
                                isAdminTokenPresent = case LedgerApiV2.txInfoOutputs info of
                                    []         -> False
                                    -- search admin token in output 0
                                    (output:_) -> OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue output) tokenAdmin_AC
                                    where
                                        !tokenAdminPolicy_CS = T.getAdminToken_CS fundDatum_In
                                        !tokenAdmin_AC = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.fundTokenAdmin_TN)
                        ------------------
                        validateRedeemerAdmin :: Bool
                        !validateRedeemerAdmin =
                            case redeemer of
                                (T.ValidatorRedeemerReIndexing (T.ValidatorRedeemerReIndexingType !riuriTokensToAdd !riuriTokensToRemove !riuriOracleReIdx_Data !riuriOracleSignature)) ->
                                        ------------------
                                        -- it runs along with FundHolding Validator (ValidatorRedeemerReIndexing)
                                        ------------------
                                        traceIfFalse "not isCorrect_Redeemer_FundHolding" isCorrect_Redeemer_FundHolding
                                        && traceIfFalse "not isCorrect_Oracle_Signature" (OnChainHelpers.isCorrect_Oracle_Signature priceData oraclePaymentPubKey riuriOracleSignature)
                                        && traceIfFalse "not isCorrect_Oracle_InRangeTime" (OnChainHelpers.isCorrect_Oracle_InRangeTime info (T.oridTime riuriOracleReIdx_Data))
                                        && traceIfFalse "not isCorrect_Exchange_WithSamePriceADA" isCorrect_Exchange_WithSamePriceADA
                                        && traceIfFalse "not isCorrect_Output_InvestUnit_Datum_WithTokensExchanged" isCorrect_Output_InvestUnit_Datum_WithTokensExchanged
                                        && traceIfFalse "not isCorrect_Output_InvestUnit_Value_NotChanged" isCorrect_Output_InvestUnit_Value_NotChanged
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
                                        !fundHoldingPolicyID_CS = FundT.fdFundHoldingPolicyID_CS fundDatum_In
                                        ------------------
                                        !input_TxOutRef_TxOut_And_FundHoldingDatum =
                                            case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS
                                                @FundHoldingT.ValidatorDatum @FundHoldingT.FundHoldingDatumType
                                                ctx
                                                inputs_TxOutRefs_TxOuts
                                                fundHoldingPolicyID_CS
                                                FundHoldingT.getFundHolding_DatumType of
                                                [x] -> x
                                                _   -> traceError "Expected exactly one FundHolding input"
                                        ------------------
                                        -- 0 out is the FundHoldingDatum
                                        -- 1 out is the InvestUnitDatum
                                        ------------------
                                        !outputs_txOuts_index1 = if length outputs_txOuts < 2
                                            then traceError "Expected at least two outputs to script addresses"
                                            else outputs_txOuts!!1
                                        ------------------
                                        !output_Own_TxOut_And_InvestUnitDatum = fromMaybe
                                                (traceError "Expected InvestUnit at output index 1")
                                                (OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                                @T.ValidatorDatum @T.InvestUnitDatumType
                                                ctx
                                                outputs_txOuts_index1
                                                investUnitID_AC
                                                (Just investUnit_Validator_Address)
                                                T.getInvestUnit_DatumType)
                                        ------------------
                                        !investUnitTokensToAdd = riuriTokensToAdd
                                        !investUnitTokensToRemove = riuriTokensToRemove
                                        ------------------
                                        !investUnit_In = T.iudInvestUnit investUnitDatum_In
                                        !investUnitTokens_In = T.iuValues investUnit_In
                                        ------------------
                                        !tokensToAdd = T.iuValues investUnitTokensToAdd
                                        !tokensToRemove = T.iuValues investUnitTokensToRemove
                                        ------------------
                                        !investUnit_Out' = OnChainHelpers.flattenValueAdd investUnitTokens_In tokensToAdd
                                        !investUnit_Out = OnChainHelpers.flattenValueSub investUnit_Out' tokensToRemove
                                        ------------------
                                        !valueFor_InvestUnitDatum_Control_NotChanged = valueOf_InvestUnitDatum_In
                                        ------------------
                                        !investUnitDatum_Control = T.mkInvestUnit_DatumType (T.iudFundPolicy_CS investUnitDatum_In) (T.InvestUnit investUnit_Out) (T.iudMinADA investUnitDatum_In)
                                        ------------------
                                        !tokensPricesADA =  T.iuValues $ T.oridTokensPriceADA riuriOracleReIdx_Data
                                        -- ------------------
                                        !priceADAOf_TokensForTokensToAdd = sum (findPriceADA False <$> tokensToAdd)
                                        -------------------
                                        !priceADAOf_TokensForTokensToRemove = sum (findPriceADA True <$> tokensToRemove)
                                        ------------------
                                        findPriceADA :: Bool -> T.InvestUnitToken -> Integer
                                        findPriceADA swAcceptZero (!cs, !tn, !amt)  =
                                            case find (\(cs', tn', _) -> cs' == cs && tn' == tn) tokensPricesADA of
                                            Nothing            -> traceError "No price found for token"
                                            Just (_, _, price) ->
                                                if not swAcceptZero && price == 0
                                                    then traceError "Price is zero"
                                                    else amt * price
                                        ------------------
                                        !priceData = OnChainHelpers.oracleReIdxDataToBBS riuriOracleReIdx_Data
                                        ------------------
                                        !oraclePaymentPubKey = ProtocolT.pdOraclePaymentPubKey protocolDatum_In
                                        -------------------
                                        isCorrect_Exchange_WithSamePriceADA :: Bool
                                        !isCorrect_Exchange_WithSamePriceADA =
                                            -- NOTE: Se espera que el precio de los tokens a agregar sea mayor o igual al precio de los tokens a quitar
                                            priceADAOf_TokensForTokensToAdd >= priceADAOf_TokensForTokensToRemove
                                        ------------------
                                        isCorrect_Output_InvestUnit_Datum_WithTokensExchanged :: Bool
                                        !isCorrect_Output_InvestUnit_Datum_WithTokensExchanged =
                                            let
                                                !investUnitDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_InvestUnitDatum
                                            in  investUnitDatum_Out `OnChainHelpers.isUnsafeEqDatums` investUnitDatum_Control
                                        ------------------
                                        isCorrect_Output_InvestUnit_Value_NotChanged :: Bool
                                        isCorrect_Output_InvestUnit_Value_NotChanged =
                                            let
                                                !valueOf_InvestUnitDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_InvestUnitDatum
                                            in  valueOf_InvestUnitDatum_Out `OnChainHelpers.isEqValue` valueFor_InvestUnitDatum_Control_NotChanged
                                        -------------------
                                        isCorrect_Redeemer_FundHolding :: Bool
                                        !isCorrect_Redeemer_FundHolding  =
                                            let !redeemerFor_FundHoldingDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef ) input_TxOutRef_TxOut_And_FundHoldingDatum) info
                                            in  case redeemerFor_FundHoldingDatum' of
                                                        Nothing -> False
                                                        Just redeemerFor_FundHoldingDatum ->
                                                            case LedgerApiV2.fromBuiltinData @FundHoldingT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_FundHoldingDatum of
                                                                Just (FundHoldingT.ValidatorRedeemerReIndexing (FundHoldingT.ValidatorRedeemerReIndexingType !rriTokensToAdd !rriTokensToRemove)) ->
                                                                    riuriTokensToAdd == rriTokensToAdd && riuriTokensToRemove == rriTokensToRemove
                                                                _ -> False
                                    ------------------
                                (T.ValidatorRedeemerUpdateMinADA _) ->
                                        ------------------
                                        -- Que el InvestUnitDatum regrese a InvestUnit Val
                                        -- Que el InvestUnitDatum se actualiza correctamente
                                        -- Que el InvestUnitDatum value cambie con el min ADA nuevo
                                        ------------------
                                        traceIfFalse "not min ADA > 0" (newMinADA > 0)
                                        && traceIfFalse "not isCorrect_Output_InvestUnit_Datum_UpdatedMinADA" isCorrect_Output_InvestUnit_Datum_UpdatedMinADA
                                        && traceIfFalse "not isCorrect_Output_InvestUnit_Value_With_MinADAChanged" isCorrect_Output_InvestUnit_Value_With_MinADAChanged
                                    where
                                        ------------------
                                        -- 0 out is the InvestUnitDatum
                                        ------------------
                                        !outputs_txOuts_index0 =
                                            if null outputs_txOuts
                                                then traceError "Expected at least one output to script addresses"
                                                else head outputs_txOuts
                                        ------------------
                                        !output_Own_TxOut_And_InvestUnitDatum =
                                            fromMaybe
                                                (traceError "Expected InvestUnit at output index 0")
                                                (OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                                @T.ValidatorDatum @T.InvestUnitDatumType
                                                ctx
                                                outputs_txOuts_index0
                                                investUnitID_AC
                                                (Just investUnit_Validator_Address)
                                                T.getInvestUnit_DatumType)
                                        ------------------
                                        !investUnitDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_InvestUnitDatum
                                        ------------------
                                        !newMinADA = T.iudMinADA investUnitDatum_Out
                                        ------------------
                                        isCorrect_Output_InvestUnit_Datum_UpdatedMinADA :: Bool
                                        !isCorrect_Output_InvestUnit_Datum_UpdatedMinADA =
                                            let
                                                !investUnitDatum_Out_Control = mkUpdated_InvestUnit_Datum_With_MinADAChanged
                                                    investUnitDatum_In
                                                    newMinADA
                                            in investUnitDatum_Out `OnChainHelpers.isUnsafeEqDatums` investUnitDatum_Out_Control
                                        ------------------
                                        isCorrect_Output_InvestUnit_Value_With_MinADAChanged :: Bool
                                        !isCorrect_Output_InvestUnit_Value_With_MinADAChanged =
                                            let !valueOf_InvestUnitDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_InvestUnitDatum
                                                !valueFor_InvestUnitDatum_Control = valueOf_InvestUnitDatum_In <> LedgerAda.lovelaceValueOf (newMinADA - T.iudMinADA investUnitDatum_In)
                                            in valueOf_InvestUnitDatum_Out `OnChainHelpers.isEqValue` valueFor_InvestUnitDatum_Control

                                (T.ValidatorRedeemerDelete _) ->
                                        ------------------
                                        -- it runs along with Fund ID Policy  (PolicyRedeemerBurnID)
                                        -- it runs along with Fund Validator (ValidatorRedeemerDelete)
                                        ------------------
                                    traceIfFalse "not isBurningInvestUnitID" isBurningInvestUnitID
                                    where
                                        ------------------
                                        isBurningInvestUnitID :: Bool
                                        !isBurningInvestUnitID = OnChainHelpers.isNFT_Burning_With_AC investUnitID_AC info
                                        ------------------
                                _ -> False

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_InvestUnit_Datum_With_MinADAChanged #-}
mkUpdated_InvestUnit_Datum_With_MinADAChanged :: T.InvestUnitDatumType -> Integer -> T.InvestUnitDatumType
mkUpdated_InvestUnit_Datum_With_MinADAChanged !investUnitDatum_In !newMinADA =
    investUnitDatum_In {
        T.iudMinADA = newMinADA
    }

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
mkWrappedValidator :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS tokenEmergencyAdminPolicy_CS = mkValidator params
    where
        params = T.ValidatorParams
            {
            vpProtocolPolicyID_CS  = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS,
            vpTokenEmergencyAdminPolicy_CS = PlutusTx.unsafeFromBuiltinData tokenEmergencyAdminPolicy_CS
            }

validatorCode :: PlutusTx.CompiledCode ( BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( PlutusTx.compile [|| mkWrappedValidator ||])


--------------------------------------------------------------------------------2
