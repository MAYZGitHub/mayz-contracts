{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.Protocol.OffChain where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Constraints.ValidityInterval as LedgerValidityInterval
import qualified Ledger.Value                        as LedgerValue
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Text.Printf                         as TextPrintf (printf)

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Generic.Constants                   as T
import qualified Generic.OffChainHelpers             as OffChainHelpers
import qualified Generic.OnChainHelpers              as OnChainHelpers
import qualified Protocol.Constants                  as T
import qualified Protocol.OffChainHelpers            as OffChainHelpers
import qualified Protocol.PABTypes                   as T
import qualified Protocol.Protocol.Helpers           as ProtocolHelpers
import qualified Protocol.Protocol.Types             as T

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

endPointProtocolPrepare :: T.PABProtocolPrepareParams -> PlutusContract.Contract w s DataText.Text ()
endPointProtocolPrepare T.PABProtocolPrepareParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Protocol Prepare"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let -- !user = Ledger.unPaymentPubKeyHash userPPKH
        -- !userAdds = Ledger.pubKeyHashAddress userPPKH Nothing
        -- !userAddressStakingCredential = case OffChainHelpers.getStakePubKeyHash userAdds of
        --     Nothing              -> Nothing
        --     Just stakePubKeyHash -> Just $ Ledger.unStakePubKeyHash stakePubKeyHash
    ---------------------
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = ppppProtocolPABParams
    ---------------------
    let !protocolPolicyID = T.pppProtocolPolicyID protocolPABParams
        !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Hash = T.pppProtocolValidator_Hash protocolPABParams
    ---------------------
    let !protocolPolicyID_Params = T.pppProtocolPolicyID_Params protocolPABParams
        !protocolPolicyID_TxOutRef = T.ppProtocolPolicyID_TxOutRef protocolPolicyID_Params
    ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Hash = T.pppScriptValidator_Hash protocolPABParams
    ---------------------
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
    ---------------------
    case find (\(txOutRef, _) -> txOutRef == protocolPolicyID_TxOutRef) (DataMap.toList uTxOsAtUser) of
        Nothing ->
            PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "%s : Can't find uTxO for mint ProtocolPolicyID" nameEndPoint
        Just _ -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "%s : uTxO for mint ProtocolPolicyID found!" nameEndPoint
    ---------------------
    let protocolPolicyID_TxOut = head [(t, ci) | (t, ci) <- DataMap.toList uTxOsAtUser, t == protocolPolicyID_TxOutRef]
    ---------------------
    let !valueFor_Mint_ProtocolID = LedgerValue.assetClassValue protocolID_AC 1
    ---------------------
        !valueFor_ProtocolDatum' = valueFor_Mint_ProtocolID
        !minADA_For_ProtocolDatum = OnChainHelpers.calculateMinADAOfValue valueFor_ProtocolDatum' True
        !value_MinADA_For_ProtocolDatum = LedgerAda.lovelaceValueOf minADA_For_ProtocolDatum
        !valueFor_ProtocolDatum = valueFor_ProtocolDatum' <> value_MinADA_For_ProtocolDatum <> LedgerAda.lovelaceValueOf 5000000 --TODO: problema con datum grande
    ---------------------
        !admins = ppppAdmins
        !tokenAdminPolicy_CS = ppppTokenAdminPolicy_CS
        !oraclePaymentPubKey = ppppOraclePaymentPubKey
        !fundCategories = ppppFundCategories
        !fundLifeTime = ppppFundLifeTime
        !requiredMAYZForSellOffer = ppppRequiredMAYZForSellOffer
        !requiredMAYZForBuyOrder = ppppRequiredMAYZForBuyOrder
        !commissionFund_PerYear_InBPx1e3 = ppppCommissionFund_PerYear_InBPx1e3
        !commissionSellOffer_InBPx1e3 = ppppCommissionSellOffer_InBPx1e3
        !commissionBuyOrder_InBPx1e3 = ppppCommissionBuyOrder_InBPx1e3
        !share_InBPx1e2_Protocol = ppppShare_InBPx1e2_Protocol
        !share_InBPx1e2_Delegators = ppppShare_InBPx1e2_Delegators
        !share_InBPx1e2_Managers = ppppShare_InBPx1e2_Managers
        !delegatorsAdmins = ppppDelegatorsAdmins
    ---------------------
        !minADA = minADA_For_ProtocolDatum
    ---------------------
        !protocolDatum_Out =
            T.mkProtocolDatum
                scriptPolicyID_CS
                scriptValidator_Hash
                oraclePaymentPubKey
                admins
                tokenAdminPolicy_CS
                fundCategories
                fundLifeTime
                requiredMAYZForSellOffer
                requiredMAYZForBuyOrder
                commissionFund_PerYear_InBPx1e3
                commissionSellOffer_InBPx1e3
                commissionBuyOrder_InBPx1e3
                share_InBPx1e2_Protocol
                share_InBPx1e2_Delegators
                share_InBPx1e2_Managers
                delegatorsAdmins
                minADA
    -------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    -------------------
    do
        let (lookupsTx_Mint_ProtocolID, tx_Mint_ProtocolID) = OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Mint_ProtocolID (Just protocolPolicyID_TxOut) (Nothing :: Maybe LedgerApiV2.Redeemer) Nothing (Just protocolPolicyID)
            lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> lookupsTx_Mint_ProtocolID
            tx =
                    tx_Mint_ProtocolID
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum protocolValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData protocolDatum_Out) valueFor_ProtocolDatum
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams Nothing lookupsTx tx

--------------------------------------------------------------------------------2

endPointProtocolUpdate :: T.PABProtocolUpdateParams -> PlutusContract.Contract w s DataText.Text ()
endPointProtocolUpdate T.PABProtocolUpdateParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    let nameEndPoint = "Protocol Update"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = ppupProtocolPABParams
    ---------------------
    let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator = T.pppProtocolValidator protocolPABParams
        !protocolValidator_Hash = T.pppProtocolValidator_Hash protocolPABParams
        !protocolValidator_ScriptHash = OffChainHelpers.hashScriptValidator protocolValidator
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
        ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
    ---------------------
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    ---------------------
    !scriptRef_With_ProtocolValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "ProtocolValidator" protocolValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let !valueOf_ProtocolDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_ProtocolDatum
        !valueFor_ProtocolDatum_Out = valueOf_ProtocolDatum_In
        !protocolDatum_In = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
        ---------------------
    let !oraclePaymentPubKey = ppupOraclePaymentPubKey
        !admins = ppupAdmins
        !tokenAdminPolicy_CS = ppupTokenAdminPolicy_CS
        !fundCategories = ppupFundCategories
        !fundLifeTime = ppupFundLifeTime
        !requiredMAYZForSellOffer = ppupRequiredMAYZForSellOffer
        !requiredMAYZForBuyOrder = ppupRequiredMAYZForBuyOrder
        !commissionFund_PerYear_InBPx1e3 = ppupCommissionFund_PerYear_InBPx1e3
        !commissionSellOffer_InBPx1e3 = ppupCommissionSellOffer_InBPx1e3
        !commissionBuyOrder_InBPx1e3 = ppupCommissionBuyOrder_InBPx1e3
        !share_InBPx1e2_Protocol = ppupShare_InBPx1e2_Protocol
        !share_InBPx1e2_Delegators = ppupShare_InBPx1e2_Delegators
        !share_InBPx1e2_Managers = ppupShare_InBPx1e2_Managers
        !delegatorsAdmins = ppupDelegatorsAdmins
        ---------------------
        !protocolDatum_Out =
            T.ProtocolDatum $ ProtocolHelpers.mkUpdated_Protocol_Datum_With_NormalChanges
                protocolDatum_In
                oraclePaymentPubKey
                admins
                tokenAdminPolicy_CS
                fundCategories
                fundLifeTime
                requiredMAYZForSellOffer
                requiredMAYZForBuyOrder
                commissionFund_PerYear_InBPx1e3
                commissionSellOffer_InBPx1e3
                commissionBuyOrder_InBPx1e3
                share_InBPx1e2_Protocol
                share_InBPx1e2_Delegators
                share_InBPx1e2_Managers
                delegatorsAdmins
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "protocolDatum_In: %s" (P.show protocolDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "protocolDatum_Out: %s" (P.show protocolDatum_Out)
    ---------------------
    let !redeemer_For_Consuming_ProtocolDatum = T.ValidatorRedeemerDatumUpdate T.ValidatorRedeemerDatumUpdateType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_ProtocolDatum: %s" (P.show redeemer_For_Consuming_ProtocolDatum)
    ---------------------
    let !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let (lookupsTx_Consume_ProtocolDatum, tx_Consume_ProtocolDatum) =
                    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_ProtocolDatum redeemer_For_Consuming_ProtocolDatum scriptRef_With_ProtocolValidator' (Just protocolValidator)
        let lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> lookupsTx_Consume_ProtocolDatum
            tx =
                    tx_Consume_ProtocolDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum protocolValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData protocolDatum_Out) valueFor_ProtocolDatum_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams Nothing lookupsTx tx

--------------------------------------------------------------------------------2

-- endPointProtocolEmergency :: T.PABProtocolEmergencyParams -> PlutusContract.Contract w s DataText.Text ()
-- endPointProtocolEmergency T.PABProtocolEmergencyParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
--     let nameEndPoint = "Protocol Emergency"
--     OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
--     ---------------------
--     (now, _) <- PlutusContract.currentNodeClientTimeRange
--     PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
--     PlutusContract.logInfo @P.String "--------------------------------"
--     ---------------------
--     !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
--     !userAddsCardano <- PlutusContract.ownAddress
--     !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
--     ---------------------
--     OffChainHelpers.checkCollateral uTxOsAtUser
--     ---------------------
--     let !protocolPABParams = ppepProtocolPABParams
--     ---------------------
--     let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
--         !protocolValidator = T.pppProtocolValidator protocolPABParams
--         !protocolValidator_Hash = T.pppProtocolValidator_Hash protocolPABParams
--         !protocolValidator_ScriptHash = OffChainHelpers.hashScriptValidator protocolValidator
--         !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
--         !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
--     ---------------------
--     let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
--         !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
--         !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
--     ---------------------
--     let !adminsWhotSigns = ppepAdmins
--     ---------------------
--     let !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
--     ---------------------
--     !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
--     !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
--     ---------------------
--     !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
--     ---------------------
--     !scriptRef_With_ProtocolValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "ProtocolValidator" protocolValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
--     ---------------------
--     let !valueOf_ProtocolDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_ProtocolDatum
--         !valueFor_ProtocolDatum_Out = valueOf_ProtocolDatum_In
--     ---------------------
--         !protocolDatum_In = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
--     ---------------------
--         !isEmergency = negate $ T.pdInEmergency protocolDatum_In
--         !protocolDatum_Out = T.ProtocolDatum $ ProtocolHelpers.mkUpdated_ProtocolDatum_With_Emergency protocolDatum_In isEmergency
--     ---------------------
--         !redeemer_For_Consuming_ProtocolDatum = T.ValidatorRedeemerEmergency $ T.ValidatorRedeemerEmergencyType adminsWhotSigns
--     ---------------------
--         !intervalOffset1 = 1000
--         !intervalOffset2 = T.validTimeRange - 1000
--         !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
--     ---------------------
--     do
--         let (lookupsTx_Consume_ProtocolDatum, tx_Consume_ProtocolDatum) =
--                     OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_ProtocolDatum redeemer_For_Consuming_ProtocolDatum scriptRef_With_ProtocolValidator' (Just protocolValidator)
--         let lookupsTx =
--                     LedgerConstraints.unspentOutputs uTxOsAtUser
--                     P.<> lookupsTx_Consume_ProtocolDatum
--             tx =
--                     tx_Consume_ProtocolDatum
--                     P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum protocolValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData protocolDatum_Out) valueFor_ProtocolDatum_Out
--                     P.<> LedgerConstraints.mustValidateInTimeRange validityRange
--                     P.<> LedgerConstraints.mustBeSignedBy userPPKH
--         ---------------------
--         OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams Nothing lookupsTx tx

--------------------------------------------------------------------------------2
