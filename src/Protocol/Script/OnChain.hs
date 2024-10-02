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

module Protocol.Script.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

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
import qualified Generic.Types             as T
import qualified Protocol.Constants        as T
import qualified Protocol.Fund.Types       as FundT
import qualified Protocol.Protocol.Types   as ProtocolT
import qualified Protocol.Script.Types     as T
import qualified Protocol.Types            as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID  (T.PolicyParams !protocolPolicyID_CS) !redRaw !ctxRaw =
    let
        ------------------
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
    in  if traceIfFalse "" useThisToMakeScriptUnique
            &&  case redeemer of
                T.PolicyRedeemerMintID _ ->
                        ---------------------
                        -- it runs alone
                        ---------------------
                        traceIfFalse "not isCorrectMint_And_Outputs" isCorrectMint_And_Outputs
                        ------------------
                    where
                        isCorrectMint_And_Outputs :: Bool
                        !isCorrectMint_And_Outputs =
                            let !flattenValueOf_Script_IDs = OnChainHelpers.getUnsafeOwnMintingTokenNameAndAmt ctx
                                !cs = LedgerContextsV2.ownCurrencySymbol ctx
                                ------------------
                                !outputs_txOuts = LedgerApiV2.txInfoOutputs info
                                !outputs_TxOuts_And_ScriptDatum = OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
                                    @T.ValidatorDatum
                                    @T.ScriptDatumType
                                    ctx
                                    outputs_txOuts
                                    cs
                                    T.getScript_DatumType
                                ------------------
                                isCorrectAmt_And_TN :: (LedgerApiV2.TokenName, Integer) -> (LedgerContextsV2.TxOut, T.ScriptDatumType) -> Bool
                                isCorrectAmt_And_TN (!tn, !amt) (!txOut, !dat) =
                                    amt == 1 &&
                                    isCorrectTN tn txOut &&
                                    traceIfFalse "not isCorrect_Output_ScriptDatum" (isCorrect_Output_ScriptDatum tn (txOut, dat))
                                ------------------
                                isCorrectTN :: LedgerApiV2.TokenName -> LedgerApiV2.TxOut -> Bool
                                isCorrectTN tn txOut = LedgerApiV2.getScriptHash (OnChainHelpers.fromJust $ LedgerApiV2.txOutReferenceScript txOut) == LedgerApiV2.unTokenName tn
                                 ------------------
                                isCorrect_Output_ScriptDatum :: LedgerApiV2.TokenName -> (LedgerContextsV2.TxOut, T.ScriptDatumType) -> Bool
                                isCorrect_Output_ScriptDatum tn txOut =
                                    let !scriptDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum txOut
                                        !scriptDatum_Out_Control = scriptDatum_Out { T.sdScriptHash = LedgerApiV2.unTokenName tn }
                                    in  scriptDatum_Out `OnChainHelpers.isUnsafeEqDatums` scriptDatum_Out_Control
                                ------------------
                            in  OnChainHelpers.compareWithFunctionWhileRemoving flattenValueOf_Script_IDs outputs_TxOuts_And_ScriptDatum isCorrectAmt_And_TN
                            ------------------
                T.PolicyRedeemerBurnID _ ->
                       ------------------
                        -- it runs along with Script Validator (ValidatorRedeemerDelete)
                            -- traceIfFalse "not isSignedByAdmin nor isAdminTokenPresent" isSignedByAdminOrIsAdminTokenPresent
                            -- && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
                            -- && traceIfFalse "not isBurning_ScriptIDs" isBurning_ScriptIDs
                            -- && traceIfFalse "not Correct Withdraw Amount_SendBackToAdmin" isCorrectAmount_SendBackToAdmin
                        ------------------
                        -- la policy permite quemar todos los tokens de la policy
                        -- se sobre entiende que esos tokens estan en utxo en el validador de scripts y que alli se ejecutará la lógica adicional
                        ------------------
                        traceIfFalse "not isBurningAllTokenOwnCSAnyAmount" (OnChainHelpers.isBurningAllTokenOwnCSAnyAmount ctx)
                        ------------------
            then ()
            else error ()

--------------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator (T.ValidatorParams !scriptPolicyID_CS !protocolPolicyID_CS)  _ !redRaw !ctxRaw =
    let
        ------------------
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
        !script_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
        ------------------
    in  if
        traceIfFalse "" useThisToMakeScriptUnique
        && case redeemer of
            T.ValidatorRedeemerDelete ->
                    ------------------
                    -- it runs along with Script ID Policy (PolicyRedeemerBurnID)
                        -- traceIfFalse "not isBurningAllTokenOwnCSAnyAmount" (OnChainHelpers.isBurningAllTokenOwnCSAnyAmount ctx)
                    ------------------
                       traceIfFalse "not isSignedByAdmin nor isAdminTokenPresent" isSignedByAdminOrIsAdminTokenPresent
                    && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
                    && traceIfFalse "not isBurning_ScriptIDs" isBurning_ScriptIDs
                    && traceIfFalse "not Correct Withdraw Amount_SendBackToAdmin" isCorrectAmount_SendBackToAdmin
                where
                    ------------------
                    !inputsRef_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoReferenceInputs info,
                        OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput )]
                    ------------------
                    !inputs_Own_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info,
                                    let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                    in  OnChainHelpers.isScriptAddress address && address == script_Validator_Address]
                    ------------------
                    !inputs_Own_TxOuts_And_ScriptDatums = OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
                        @T.ValidatorDatum @T.ScriptDatumType
                        ctx
                        inputs_Own_TxOuts
                        scriptPolicyID_CS
                        T.getScript_DatumType
                    ------------------
                    !scriptDatums_In = OnChainHelpers.getDatum_In_TxOut_And_Datum <$> inputs_Own_TxOuts_And_ScriptDatums
                    ------------------
                    isSignedByAdminOrIsAdminTokenPresent :: Bool
                    !isSignedByAdminOrIsAdminTokenPresent =
                        let
                            ------------------
                            !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                            ------------------
                            !inputRef_TxOut_And_ProtocolDatum' = OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                @ProtocolT.ValidatorDatum
                                @ProtocolT.ProtocolDatumType
                                ctx
                                inputsRef_TxOuts
                                protocolID_AC
                                ProtocolT.getProtocol_DatumType
                            ------------------
                            !isSignedEach =
                                [ let  !(admins, tokenAdmin_AC) =
                                            case T.sdFundPolicy_CS scriptDatum_In of
                                                ------------------
                                                -- si esta seteado sdFundPolicy_CS significa que es un script de fund, si no es un script de protocol
                                                Nothing ->
                                                    -- es un script de protocol el que quiero consumir, para eso necesito encontrar el protocol datum en input refs
                                                    case inputRef_TxOut_And_ProtocolDatum' of
                                                                [inputRef_TxOut_And_ProtocolDatum] ->
                                                                    let !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                                                                        !tokenAdminPolicy_CS = T.getAdminToken_CS protocolDatum_In
                                                                        !tokenAdmin_AC_ = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.protocolTokenAdmin_TN)
                                                                    in  (T.getAdmins protocolDatum_In, tokenAdmin_AC_)
                                                                _   -> traceError "Expected exactly Protocol input ref"
                                                ------------------
                                                Just fundPolicyID_CS ->
                                                    -- es un script de fondo el que quiero consumir, para eso necesito encontrar el fund datum en input refs
                                                    ------------------
                                                    let !fundID_AC = LedgerValue.AssetClass (fundPolicyID_CS, T.fundID_TN)
                                                    ------------------
                                                        !inputRef_TxOut_And_FundDatum' = OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                                            @FundT.ValidatorDatum @FundT.FundDatumType
                                                            ctx
                                                            inputsRef_TxOuts
                                                            fundID_AC
                                                            FundT.getFund_DatumType
                                                    in case inputRef_TxOut_And_FundDatum' of
                                                            [inputRef_TxOuts_And_FundDatum] ->
                                                                    let !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOuts_And_FundDatum
                                                                        !tokenAdminPolicy_CS = T.getAdminToken_CS fundDatum_In
                                                                        !tokenAdmin_AC_ = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.fundTokenAdmin_TN)
                                                                    in  (T.getAdmins fundDatum_In, tokenAdmin_AC_)
                                                            _   -> traceError "Expected exactly one Fund input ref"
                                    in OnChainHelpers.isSignedByAny admins info || isAdminTokenPresent tokenAdmin_AC
                                  | scriptDatum_In <- scriptDatums_In
                                ]
                            ------------------
                            isAdminTokenPresent :: LedgerValue.AssetClass -> Bool
                            isAdminTokenPresent tokenAdmin_AC = OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue $ head (LedgerApiV2.txInfoOutputs info)) tokenAdmin_AC
                        ------------------
                        in  all (== True) isSignedEach
                    ------------------
                    isBurning_ScriptIDs :: Bool
                    !isBurning_ScriptIDs =
                        let !valueOf_Inputs_Own_TxOuts_And_ScriptDatums = mconcat $ OnChainHelpers.getValue_In_TxOut_And_Datum <$> inputs_Own_TxOuts_And_ScriptDatums
                        ------------------
                            !flattenValueOf_Script_IDs = OnChainHelpers.flattenValue $ LedgerValue.noAdaValue valueOf_Inputs_Own_TxOuts_And_ScriptDatums
                        in  all
                                ( \(cs, tn, amt) ->
                                    let scriptID_AC = LedgerValue.AssetClass (cs, tn)
                                    in  OnChainHelpers.isToken_Minting_With_AC_AndAmt scriptID_AC (negate amt) info
                                )
                                flattenValueOf_Script_IDs
                    ------------------
                    isCorrectAmount_SendBackToAdmin :: Bool
                    !isCorrectAmount_SendBackToAdmin =
                        let joinSameAdmin :: [(T.WalletPaymentPKH, LedgerApiV2.Value)] -> [(T.WalletPaymentPKH, LedgerApiV2.Value, LedgerApiV2.Value)]
                            joinSameAdmin = joinSameAdminHelper []
                                where
                                    joinSameAdminHelper :: [(T.WalletPaymentPKH, LedgerApiV2.Value, LedgerApiV2.Value)] -> [(T.WalletPaymentPKH, LedgerApiV2.Value)] -> [(T.WalletPaymentPKH, LedgerApiV2.Value, LedgerApiV2.Value)]
                                    joinSameAdminHelper seen [] = seen
                                    joinSameAdminHelper seen ((admin_To_SendBack, valueOf_ScriptDatum) : xs) =
                                        let !admin' = OnChainHelpers.find' (\(m, _, _) -> m == admin_To_SendBack) seen
                                        in  case admin' of
                                                Nothing ->
                                                    let !valueFor_Admin_Real = LedgerContextsV2.valuePaidTo info admin_To_SendBack
                                                        !elemet = (admin_To_SendBack, valueOf_ScriptDatum, valueFor_Admin_Real)
                                                    in  joinSameAdminHelper (elemet : seen) xs
                                                Just (_, v1, v2) ->
                                                    let !elemet = (admin_To_SendBack, v1 <> valueOf_ScriptDatum, v2)
                                                        !seen_filter = OnChainHelpers.filter' (\(m', _, _) -> m' /= admin_To_SendBack) seen
                                                    in  joinSameAdminHelper (elemet : seen_filter) xs
                            ------------------
                            !values_For_Each_Admin =
                                [ let !scriptDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_Own_TxOut_And_ScriptDatum
                                      !admin_To_SendBack = T.sdAdminPaymentPKH scriptDatum_In
                                      !valueFor_Admin = LedgerValue.adaOnlyValue $ OnChainHelpers.getValue_In_TxOut_And_Datum input_Own_TxOut_And_ScriptDatum
                                  in  (admin_To_SendBack, valueFor_Admin)
                                  | input_Own_TxOut_And_ScriptDatum <- inputs_Own_TxOuts_And_ScriptDatums
                                ]
                            ------------------
                            !values_For_Each_Admin_Accumulated = joinSameAdmin values_For_Each_Admin
                        in  all (\(_, v1, v2) -> OnChainHelpers.isIncludeValue' v2 v1) values_For_Each_Admin_Accumulated
            then ()
            else error ()

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
mkWrappedPolicyID :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyID protocolPolicyID_CS   = mkPolicyID params
    where
        params = T.PolicyParams
            {
                ppProtocolPolicyID_CS   =PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
            }

policyIDCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
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

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS scriptPolicyID_CS = mkValidator params
    where
        params = T.ValidatorParams
            {
            vpProtocolPolicyID_CS  = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS,
            vpScriptPolicyID_CS    = PlutusTx.unsafeFromBuiltinData scriptPolicyID_CS
            }

validatorCode :: PlutusTx.CompiledCode ( BuiltinData -> BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( PlutusTx.compile [|| mkWrappedValidator ||])



------------------------------------------------------------------------------
