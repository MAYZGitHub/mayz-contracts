{-# LANGUAGE TypeApplications #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.Transactions where

-- Non-IOG imports
import qualified Data.List                 as DataList (isInfixOf)
import           Prelude
import qualified Test.Tasty                as Tasty
import qualified Test.Tasty.QuickCheck     as TastyQC
-- IOG imports
import qualified Ledger.Ada                as LedgerAda
import qualified Ledger.Value              as LedgerValue
import qualified Plutus.Model              as PlutusSimpleModel
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified PlutusTx
import qualified PlutusTx.Builtins         as TxBuiltins

-- Project imports
import qualified Generic.Constants         as T
import qualified Generic.OffChainHelpers   as OffChainHelpers
import qualified Generic.OnChainHelpers    as OnChainHelpers
import           Helpers.Data
import qualified Protocol.Constants        as T
import qualified Protocol.Protocol.Helpers as ProtocolT
import qualified Protocol.Protocol.Types   as ProtocolT
import           TestUtils.Helpers
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------2

-- Helper function that mimics testNoErrors but returns a Property
testNoErrorsProperty :: LedgerApiV2.Value ->  PlutusSimpleModel.MockConfig -> PlutusSimpleModel.Run a -> TastyQC.Property
testNoErrorsProperty funds cfg act =  do
    let (result, mock) = PlutusSimpleModel.runMock (act >> PlutusSimpleModel.checkErrors) (PlutusSimpleModel.initMock (PlutusSimpleModel.warnLimits cfg) funds)
    let mockLog = "\nBlockchain log :\n----------------\n" <> PlutusSimpleModel.ppMockEvent (PlutusSimpleModel.mockNames mock) (PlutusSimpleModel.getLog mock)
    let tfmLog= PlutusSimpleModel.filterSlot (> 0)
    let limitLog = "\nResources log :\n----------------\n" <>  PlutusSimpleModel.ppLimitInfo (PlutusSimpleModel.mockNames mock)  (tfmLog $ PlutusSimpleModel.mockTxs mock)
    case result of
        Nothing  -> TastyQC.property True
        Just err ->  TastyQC.counterexample (mockLog  <> "\nError: " <> err) $ TastyQC.property False

testErrorsProperty :: LedgerApiV2.Value ->  PlutusSimpleModel.MockConfig -> PlutusSimpleModel.Run a -> String -> TastyQC.Property
testErrorsProperty funds cfg act expectedError =  do
    let (result, mock) = PlutusSimpleModel.runMock (act >> PlutusSimpleModel.checkErrors) (PlutusSimpleModel.initMock (PlutusSimpleModel.warnLimits cfg) funds)
    let mockLog = "\nBlockchain log :\n----------------\n" <> PlutusSimpleModel.ppMockEvent (PlutusSimpleModel.mockNames mock) (PlutusSimpleModel.getLog mock)
    let tfmLog= PlutusSimpleModel.filterSlot (> 0)
    let limitLog = "\nResources log :\n----------------\n" <>  PlutusSimpleModel.ppLimitInfo (PlutusSimpleModel.mockNames mock) (tfmLog $ PlutusSimpleModel.mockTxs mock)
    case result of
        Nothing  -> TastyQC.counterexample (mockLog  <> "\nError: An error was expected") $ TastyQC.property False
        Just err ->   TastyQC.counterexample (mockLog <> "\nError: An error was expected of type: " <> expectedError <> " got " <> err  ) $  TastyQC.property (expectedError `DataList.isInfixOf` err )

goodCase2 :: TxDeployParams -> PlutusSimpleModel.Run a -> TastyQC.Property
goodCase2 txDeployParams = do
    let protocolTokenEmergencyAdmin_AC = LedgerValue.AssetClass (tdpTokenEmergencyAdminPolicy_CS txDeployParams, T.protocolTokenEmergencyAdmin_TN)
        protocolTokenEmergencyAdmin_Value = LedgerValue.assetClassValue protocolTokenEmergencyAdmin_AC 100
        protocolTokenAdmin_AC = LedgerValue.AssetClass (tdpTokenAdminPolicy_CS txDeployParams, T.protocolTokenAdmin_TN)
        protocolTokenAdmin_Value = LedgerValue.assetClassValue  protocolTokenAdmin_AC 100
    testNoErrorsProperty (PlutusSimpleModel.adaValue 1_000_000_000 <> protocolTokenEmergencyAdmin_Value <> protocolTokenAdmin_Value )  PlutusSimpleModel.defaultBabbage

failCase2 ::  TxDeployParams  -> PlutusSimpleModel.Run a -> String -> TastyQC.Property
failCase2 txDeployParams = do
    let protocolTokenEmergencyAdmin_AC = LedgerValue.AssetClass (tdpTokenEmergencyAdminPolicy_CS txDeployParams, T.protocolTokenEmergencyAdmin_TN)
        protocolTokenEmergencyAdmin_Value = LedgerValue.assetClassValue protocolTokenEmergencyAdmin_AC 100
        protocolTokenAdmin_AC = LedgerValue.AssetClass (tdpTokenAdminPolicy_CS txDeployParams, T.protocolTokenAdmin_TN)
        protocolTokenAdmin_Value = LedgerValue.assetClassValue  protocolTokenAdmin_AC 100
    testErrorsProperty  (PlutusSimpleModel.adaValue 1_000_000_000 <> protocolTokenEmergencyAdmin_Value <> protocolTokenAdmin_Value )  PlutusSimpleModel.defaultBabbage

_IS_VALID = True
_IS_NOT_VALID = False
_IS_VALID_TX_RANGE = True
_IS_NOT_VALID_TX_RANGE = False
_IS_VALID_DATUM = True
_IS_NOT_VALID_DATUM = False
_IS_VALID_REDEEMER = True
_IS_NOT_VALID_REDEEMER = False
_IS_VALID_SIGNATURE = True
_IS_NOT_VALID_SIGNATURE = False
_USE_ADMIN_TOKEN_TOKEN= True
_NOT_USE_ADMIN_TOKEN_TOKEN = False
_USE_EMERGENCY_TOKEN_TOKEN= True
_NOT_USE_EMERGENCY_TOKEN_TOKEN = False

-----------------------------------------------------------------

protocolTransactionsTests :: TestCompiledCodeScripts ->    Tasty.TestTree
protocolTransactionsTests   testCompiledCodeScripts = do
    ----------------
    let txDeployConfig = TxDeployConfig _IS_VALID _IS_VALID_TX_RANGE _IS_VALID_DATUM
    let txUpdateConfig = TxUpdateConfig _IS_VALID _IS_VALID_TX_RANGE _IS_VALID_DATUM _IS_VALID_REDEEMER _IS_VALID_SIGNATURE _NOT_USE_ADMIN_TOKEN_TOKEN _NOT_USE_EMERGENCY_TOKEN_TOKEN
    ----------------
    Tasty.testGroup
        "Protocol Transactions Tests"
        [
            Tasty.testGroup
                "Deploy Tests"
                [
                    -- TastyQC.testProperty "Deploy must success" (protocolDeployProperty testCompiledCodeScripts txDeployConfig),
                    --  do
                    --     let txDeployConfig' = txDeployConfig{tdcIsValid = _IS_NOT_VALID, tdcUseValidTxTimeRange = _IS_NOT_VALID_TX_RANGE}
                    --     TastyQC.testProperty "Deploy with Invalid Range must fail" (protocolDeployProperty testCompiledCodeScripts txDeployConfig'),

                    -- do
                    --     let txDeployConfig' = txDeployConfig{tdcIsValid = _IS_NOT_VALID, tdcUseValidDatum = _IS_NOT_VALID_DATUM}
                    --     TastyQC.testProperty "Deploy with Invalid Datum must fail" (protocolDeployProperty testCompiledCodeScripts txDeployConfig')
                ],
            Tasty.testGroup
                "Update Tests"
                [
                    -- TastyQC.testProperty "Update must success" (protocolUpdateProperty testCompiledCodeScripts txDeployConfig txUpdateConfig),
                    --  do
                    --     let txUpdateConfig' = txUpdateConfig{tucIsValid = _IS_NOT_VALID, tucUseValidTxTimeRange = _IS_NOT_VALID_TX_RANGE}
                    --     TastyQC.testProperty "Update with Invalid Range must fail" (protocolUpdateProperty testCompiledCodeScripts txDeployConfig txUpdateConfig'),

                    -- do
                    --     let txUpdateConfig' = txUpdateConfig{tucIsValid = _IS_NOT_VALID, tucUseValidDatum = _IS_NOT_VALID_DATUM}
                    --     TastyQC.testProperty "Update with Invalid Datum must fail" (protocolUpdateProperty testCompiledCodeScripts txDeployConfig txUpdateConfig'),
                    do
                        let txUpdateConfig' = txUpdateConfig{tucIsValid = _IS_NOT_VALID, tucUseValidSignature= _IS_NOT_VALID_SIGNATURE}
                        TastyQC.testProperty "Update with Invalid Signmature must fail" (protocolUpdateProperty testCompiledCodeScripts txDeployConfig txUpdateConfig')
                    ,
                    do
                        let txUpdateConfig' = txUpdateConfig{tucIsValid = _IS_VALID, tucUseValidSignature= _IS_NOT_VALID_SIGNATURE, tucUseAdminToken= _USE_ADMIN_TOKEN_TOKEN}
                        TastyQC.testProperty "Update with Invalid Signmature but token admin must success" (protocolUpdateProperty testCompiledCodeScripts txDeployConfig txUpdateConfig')
                    , do
                         let txUpdateConfig' = txUpdateConfig{tucIsValid = _IS_VALID, tucUseValidSignature= _IS_NOT_VALID_SIGNATURE, tucUseEmergencyToken = _USE_EMERGENCY_TOKEN_TOKEN}
                         TastyQC.testProperty "Update with Invalid Signmature but token emergency must success" (protocolUpdateProperty testCompiledCodeScripts txDeployConfig txUpdateConfig')
                     ,
                     do
                         let txUpdateConfig' = txUpdateConfig{tucIsValid = _IS_VALID, tucUseValidDatum = _IS_NOT_VALID_DATUM, tucUseEmergencyToken = _USE_EMERGENCY_TOKEN_TOKEN}
                         TastyQC.testProperty "Update with Invalid Datum but token emergency must success" (protocolUpdateProperty testCompiledCodeScripts txDeployConfig txUpdateConfig')

                    -- do
                    --     let txUpdateConfig' = txUpdateConfig{tucIsValid = _IS_NOT_VALID, tucUseValidRedeemer = _IS_NOT_VALID_REDEEMER}
                    --     TastyQC.testProperty "Update with Invalid Redeemer must fail" (protocolUpdateProperty testCompiledCodeScripts txDeployConfig txUpdateConfig')
                ]
        ]

-----------------------------------------------------------------

protocolCreateScriptsRun ::TestCompiledCodeScripts ->  TxDeployParams -> [LedgerApiV2.PubKeyHash] -> PlutusSimpleModel.Run ProtocolData
protocolCreateScriptsRun testCompiledCodeScripts txDeployParams users = do
    --------------------
    PlutusSimpleModel.logInfo "CREATE SCRIPTS...."
    --------------------
    let user = head users
    utxos <- PlutusSimpleModel.utxoAt user
    let (protocolPolicyID_TxOutRef, out) = head utxos
    --------------------
    let tokenEmergencyAdminPolicy_CS = tdpTokenEmergencyAdminPolicy_CS txDeployParams
        tokenMAYZ_CS = tdpTokenMAYZ_CS txDeployParams
        tokenMAYZ_TN = tdpTokenMAYZ_TN txDeployParams
        protocolAdmins = [user]
        delegatorsAdmins = [user, head $ tail users]
    --------------------
    let protocolPolicyID =
            do
                let
                    code = tccsProtocolPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleProtocolPolicyParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.getTxId $ LedgerApiV2.txOutRefId protocolPolicyID_TxOutRef
                    param2 = TxBuiltins.mkI $ LedgerApiV2.txOutRefIdx protocolPolicyID_TxOutRef
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkMintingPolicyScript appliedCode
        protocolPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy protocolPolicyID
    let
        protocolValidator =
            do
                let
                    code = tccsProtocolValidator_Pre testCompiledCodeScripts
                    -- params = exampleProtocolValidatorParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkValidatorScript appliedCode
    let
        protocolValidator_Hash = OffChainHelpers.hashValidator protocolValidator

    let
        scriptPolicyID =
            do
                let
                    code = tccsScriptPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleScriptPolicyParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1
                LedgerApiV2.mkMintingPolicyScript appliedCode
    let
        scriptPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy scriptPolicyID

    let
        scriptValidator =
            do
                let
                    code = tccsScriptValidator_Pre testCompiledCodeScripts
                    -- params = exampleScriptValidatorParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol scriptPolicyID_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkValidatorScript appliedCode
    let
        scriptValidator_Hash = OffChainHelpers.hashValidator scriptValidator

    let
        sellOfferValidator =
            do
                let
                    code = tccsSellOfferValidator_Pre testCompiledCodeScripts
                    -- params = exampleSellOfferValidParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkValidatorScript appliedCode
    let
        sellOfferValidator_Hash = OffChainHelpers.hashValidator sellOfferValidator

    let
        sellOfferPolicyID =
            do
                let
                    code = tccsSellOfferPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleSellOfferPolicyParams
                    -- protocolPolicyID_CS sellOffer_Validator_Hash tokenMAYZ_CS tokenMAYZ_TN
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    (LedgerApiV2.ValidatorHash hash) = sellOfferValidator_Hash
                    param2 = TxBuiltins.mkB hash
                    param3 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenMAYZ_CS
                    param4 = TxBuiltins.mkB $ LedgerApiV2.unTokenName tokenMAYZ_TN
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2 `PlutusTx.applyCode` PlutusTx.liftCode param3 `PlutusTx.applyCode` PlutusTx.liftCode param4
                LedgerApiV2.mkMintingPolicyScript appliedCode
    let
        sellOfferPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy sellOfferPolicyID

    let
        buyOrderValidator =
            do
                let
                    code = tccsBuyOrderValidator_Pre testCompiledCodeScripts
                    -- params = exampleBuyOrderValidaParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1
                LedgerApiV2.mkValidatorScript appliedCode
    let
        buyOrderValidator_Hash = OffChainHelpers.hashValidator buyOrderValidator

    let
        buyOrderPolicyID =
            do
                let
                    code = tccsBuyOrderPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleSellOfferPolicyParams
                    -- protocolPolicyID_CS buyOrder_Validator_Hash tokenMAYZ_CS tokenMAYZ_TN
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    (LedgerApiV2.ValidatorHash hash) = buyOrderValidator_Hash
                    param2 = TxBuiltins.mkB hash
                    param3 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenMAYZ_CS
                    param4 = TxBuiltins.mkB $ LedgerApiV2.unTokenName tokenMAYZ_TN
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2 `PlutusTx.applyCode` PlutusTx.liftCode param3 `PlutusTx.applyCode` PlutusTx.liftCode param4
                LedgerApiV2.mkMintingPolicyScript appliedCode
    let
        buyOrderPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy buyOrderPolicyID

    let
        delegationValidator =
            do
                let
                    code = tccsDelegationValidator_Pre testCompiledCodeScripts
                    -- params = exampleDelegationValiParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenMAYZ_CS
                    param3 = TxBuiltins.mkB $ LedgerApiV2.unTokenName tokenMAYZ_TN
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2 `PlutusTx.applyCode` PlutusTx.liftCode param3
                LedgerApiV2.mkValidatorScript appliedCode
    let
        delegationValidator_Hash = OffChainHelpers.hashValidator delegationValidator

    let
        delegationPolicyID =
            do
                let
                    code = tccsDelegationPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleSellOfferPolicyParams
                    --  protocolPolicyID_CS delegation_Validator_Hash tokenMAYZ_CS tokenMAYZ_TN
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    (LedgerApiV2.ValidatorHash hash) = delegationValidator_Hash
                    param2 = TxBuiltins.mkB hash
                    param3 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenMAYZ_CS
                    param4 = TxBuiltins.mkB $ LedgerApiV2.unTokenName tokenMAYZ_TN
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2 `PlutusTx.applyCode` PlutusTx.liftCode param3 `PlutusTx.applyCode` PlutusTx.liftCode param4
                LedgerApiV2.mkMintingPolicyScript appliedCode
    let
        delegationPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy delegationPolicyID

    let
        investUnitValidator =
            do
                let
                    code = tccsInvestUnitValidator_Pre testCompiledCodeScripts
                    -- params = exampleInvestUnitValiParams
                    -- protocolPolicyID_CS tokenEmergencyAdminPolicy_CS
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkValidatorScript appliedCode
    let
        investUnitValidator_Hash = OffChainHelpers.hashValidator investUnitValidator
    ct <- PlutusSimpleModel.currentTime
    --------------------
    PlutusSimpleModel.logInfo $ "TIME: " ++ show ct
    --------------------
    let protocolScripts   = ProtocolScripts
                    { psProtocolPolicyID = protocolPolicyID
                    , psProtocolPolicyID_CS = protocolPolicyID_CS
                    , psProtocolValidator = protocolValidator
                    , psProtocolValidator_Hash = protocolValidator_Hash
                    , psScriptPolicyID = scriptPolicyID
                    , psScriptPolicyID_CS = scriptPolicyID_CS
                    , psScriptValidator = scriptValidator
                    , psScriptValidator_Hash = scriptValidator_Hash
                    , psSellOfferPolicyID = sellOfferPolicyID
                    , psSellOfferPolicyID_CS = sellOfferPolicyID_CS
                    , psSellOfferValidator = sellOfferValidator
                    , psSellOfferValidator_Hash = sellOfferValidator_Hash
                    , psBuyOrderPolicyID = buyOrderPolicyID
                    , psBuyOrderPolicyID_CS = buyOrderPolicyID_CS
                    , psBuyOrderValidator = buyOrderValidator
                    , psBuyOrderValidator_Hash = buyOrderValidator_Hash
                    , psDelegationPolicyID = delegationPolicyID
                    , psDelegationPolicyID_CS = delegationPolicyID_CS
                    , psDelegationValidator = delegationValidator
                    , psDelegationValidator_Hash = delegationValidator_Hash
                    , psInvestUnitValidator = investUnitValidator
                    , psInvestUnitValidator_Hash = investUnitValidator_Hash }
    --------------------
    return (ProtocolData protocolScripts user protocolAdmins delegatorsAdmins)

------------------------------------------------------------

protocolDeployTxRun ::   TxDeployConfig ->  TxDeployParams -> ProtocolData ->  PlutusSimpleModel.Run ()
protocolDeployTxRun txDeployConfig txDeployParams protocolData = do
    --------------------
    let
        scripts = pdProtocolScripts protocolData
        protocolCreator   = pdProtocolCreator protocolData
    --------------------
        protocolPolicyID  =  psProtocolPolicyID scripts
        protocolPolicyID_PSM = policyPSM protocolPolicyID
        protocolValidator=  psProtocolValidator scripts
        protocolValidator_PSM = validatorPSM protocolValidator
    --------------------
        oraclePaymentPubKey = tdpOraclePaymentPubKey txDeployParams
        admins = pdProtocolAdmins protocolData
        tokenAdminPolicy_CS = tdpTokenAdminPolicy_CS txDeployParams
        fundCategories = tdpFundCategories txDeployParams
        fundLifeTime = tdpFundLifeTime txDeployParams
        requiredMAYZForSellOffer = tdpRequiredMAYZForSellOffer txDeployParams
        requiredMAYZForBuyOrder = tdpRequiredMAYZForBuyOrder txDeployParams
        commissionFund_PerYear_InBPx1e3 = tdpCommissionFund_PerYear_InBPx1e3 txDeployParams
        commissionSellOffer_InBPx1e3 = tdpCommissionSellOffer_InBPx1e3 txDeployParams
        commissionBuyOrder_InBPx1e3 = tdpCommissionBuyOrder_InBPx1e3 txDeployParams
        share_InBPx1e2_Protocol = tdpShare_InBPx1e2_Protocol txDeployParams
        share_InBPx1e2_Delegators = tdpShare_InBPx1e2_Delegators txDeployParams
        share_InBPx1e2_Managers = tdpShare_InBPx1e2_Managers txDeployParams
        delegatorsAdmins = pdDelegatorsAdmins protocolData
    --------------------
        protocolPolicy_CS = PlutusSimpleModel.scriptCurrencySymbol protocolPolicyID_PSM
        valueFor_Mint_ProtocolID = LedgerApiV2.singleton protocolPolicy_CS T.protocolID_TN 1
        valueFor_ProtocolDatum' = valueFor_Mint_ProtocolID
        minADA_For_ProtocolDatum = OnChainHelpers.calculateMinADAOfValue valueFor_ProtocolDatum' True
        value_MinADA_For_ProtocolDatum = LedgerAda.lovelaceValueOf minADA_For_ProtocolDatum
        valueFor_ProtocolDatum_Out = valueFor_ProtocolDatum' <> value_MinADA_For_ProtocolDatum
    --------------------
        protocolDatum_Out =
            ProtocolT.mkProtocolDatumType
                (psScriptPolicyID_CS scripts)
                (psScriptValidator_Hash scripts)
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
                minADA_For_ProtocolDatum
    --------------------
    spUTXO <- PlutusSimpleModel.spend protocolCreator value_MinADA_For_ProtocolDatum
    --------------------
    let tx = mconcat
            [
             PlutusSimpleModel.userSpend spUTXO
             , PlutusSimpleModel.mintValue protocolPolicyID_PSM () valueFor_Mint_ProtocolID
             , PlutusSimpleModel.payToScript protocolValidator_PSM (PlutusSimpleModel.InlineDatum (ProtocolT.ProtocolDatum protocolDatum_Out)) valueFor_ProtocolDatum_Out

            ]
    --------------------
    tx' <- do
            if tdcUseValidTxTimeRange txDeployConfig then do
                ct <- PlutusSimpleModel.currentTimeRad T.validTimeRange
                PlutusSimpleModel.validateIn ct tx
            else
                return tx
    --------------------
    PlutusSimpleModel.logInfo "DEPLOY TX"
    --------------------
    PlutusSimpleModel.submitTx protocolCreator tx'

------------------------------------------------------------


protocolUpdateTxRun ::   TxUpdateConfig ->  TxDeployParams ->  TxUpdateParams -> ProtocolData ->  [LedgerApiV2.PubKeyHash] -> PlutusSimpleModel.Run ProtocolData
protocolUpdateTxRun txUpdateConfig txDeployParams txUpdateParams protocolData users = do
    --------------------
    let
        scripts = pdProtocolScripts protocolData
        protocolCreator   = pdProtocolCreator protocolData
    --------------------
        user = if tucUseValidSignature txUpdateConfig then do
            protocolCreator
        else do
            let anotherUser = last users
            anotherUser
    --------------------
        protocolPolicyID  =  psProtocolPolicyID scripts
        protocolPolicyID_PSM = policyPSM protocolPolicyID
        protocolValidator=  psProtocolValidator scripts
        protocolValidator_PSM = validatorPSM @ProtocolT.ValidatorDatum  @ProtocolT.ValidatorRedeemer protocolValidator
    --------------------
        oraclePaymentPubKey = tupOraclePaymentPubKey txUpdateParams
        admins = pdProtocolAdmins protocolData
        tokenAdminPolicy_CS = tupTokenAdminPolicy_CS txUpdateParams
        fundCategories = tupFundCategories txUpdateParams
        fundLifeTime = tupFundLifeTime txUpdateParams
        requiredMAYZForSellOffer = tupRequiredMAYZForSellOffer txUpdateParams
        requiredMAYZForBuyOrder = tupRequiredMAYZForBuyOrder txUpdateParams
        commissionFund_PerYear_InBPx1e3 = tupCommissionFund_PerYear_InBPx1e3 txUpdateParams
        commissionSellOffer_InBPx1e3 = tupCommissionSellOffer_InBPx1e3 txUpdateParams
        commissionBuyOrder_InBPx1e3 = tupCommissionBuyOrder_InBPx1e3 txUpdateParams
        share_InBPx1e2_Protocol = tupShare_InBPx1e2_Protocol txUpdateParams
        share_InBPx1e2_Delegators = tupShare_InBPx1e2_Delegators txUpdateParams
        share_InBPx1e2_Managers = tupShare_InBPx1e2_Managers txUpdateParams
        delegatorsAdmins = pdDelegatorsAdmins protocolData
    --------------------
    utxos <- PlutusSimpleModel.utxoAt protocolValidator_PSM
    --------------------
    let (txOutRef, txOut) = head utxos -- We know there is only one UTXO (the one we created before)
    --------------------
    let protocolDatum_In = case OnChainHelpers.getInlineDatum_From_TxOut @ProtocolT.ValidatorDatum txOut of
            Just (ProtocolT.ProtocolDatum x) -> x
            _                                -> error "getInlineDatum_From_TxOut"
    --------------------
    let protocolDatum_Out =
            ProtocolT.mkUpdated_Protocol_Datum_With_NormalChanges
                protocolDatum_In
                oraclePaymentPubKey
                (ProtocolT.pdAdmins protocolDatum_In)
                (ProtocolT.pdTokenAdminPolicy_CS protocolDatum_In)
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
                (ProtocolT.pdDelegatorsAdmins protocolDatum_In)
        valueOf_ProtocolDatum_In =  LedgerApiV2.txOutValue txOut
        valueFor_ProtocolDatum_Out = valueOf_ProtocolDatum_In
        redeemerConsume
          | tucUseEmergencyToken txUpdateConfig = do
            ProtocolT.ValidatorRedeemerDatumUpdate ProtocolT.ValidatorRedeemerDatumUpdateType
          | tucUseValidRedeemer txUpdateConfig = do
            ProtocolT.ValidatorRedeemerDatumUpdate ProtocolT.ValidatorRedeemerDatumUpdateType
          | otherwise =
            ProtocolT.ValidatorRedeemerDatumUpdate ProtocolT.ValidatorRedeemerDatumUpdateType
    --------------------
    utxos <- PlutusSimpleModel.utxoAt user
    --------------------
    let protocolTokenEmergencyAdmin_AC = LedgerValue.AssetClass (tdpTokenEmergencyAdminPolicy_CS txDeployParams, T.protocolTokenEmergencyAdmin_TN)
        protocolTokenEmergencyAdmin_Value = LedgerValue.assetClassValue protocolTokenEmergencyAdmin_AC 1
        protocolTokenAdmin_AC = LedgerValue.AssetClass (tdpTokenAdminPolicy_CS txDeployParams, T.protocolTokenAdmin_TN)
        protocolTokenAdmin_Value = LedgerValue.assetClassValue  protocolTokenAdmin_AC 1
    --------------------
    let userValue
          | tucUseEmergencyToken txUpdateConfig =
            protocolTokenEmergencyAdmin_Value
          | tucUseAdminToken txUpdateConfig =
            protocolTokenAdmin_Value
          | otherwise =
            LedgerAda.lovelaceValueOf 1
    --------------------
    let spendUtxos = filter (\utxo -> OnChainHelpers.isIncludeValue'  (LedgerApiV2.txOutValue (snd utxo)) userValue ) utxos
    --------------------
    let tx = mconcat
            [
                if tucUseAdminToken txUpdateConfig || tucUseEmergencyToken txUpdateConfig
                    then mconcat
                        [ PlutusSimpleModel.spendPubKey (fst $ head spendUtxos)
                        , PlutusSimpleModel.payToKey user (LedgerApiV2.txOutValue (snd $ head spendUtxos))
                        ]
                    else mempty ,
                PlutusSimpleModel.spendScript protocolValidator_PSM txOutRef redeemerConsume (ProtocolT.ProtocolDatum protocolDatum_In)
             , PlutusSimpleModel.payToScript protocolValidator_PSM (PlutusSimpleModel.InlineDatum (ProtocolT.ProtocolDatum protocolDatum_Out)) valueFor_ProtocolDatum_Out

            ]
    PlutusSimpleModel.logInfo $ "UPDATE TX2 " ++ show tx

    --------------------
    tx' <- do
            if tucUseValidTxTimeRange txUpdateConfig then do
                ct <- PlutusSimpleModel.currentTimeRad T.validTimeRange
                PlutusSimpleModel.validateIn ct tx
            else
                return tx
    --------------------
    PlutusSimpleModel.logInfo "UPDATE TX"
    --------------------
    PlutusSimpleModel.submitTx user tx'
    --------------------
    return protocolData

------------------------------------------------------------

protocolDeployRun ::  TestCompiledCodeScripts -> TxDeployConfig ->  TxDeployParams -> [LedgerApiV2.PubKeyHash]  -> PlutusSimpleModel.Run ProtocolData
protocolDeployRun testCompiledCodeScripts txDeployConfig txDeployParams users = do
    --------------------
    users' <- if null users then setupUsers txDeployParams else return users
    --------------------
    protocolData <- protocolCreateScriptsRun testCompiledCodeScripts txDeployParams users'
    --------------------
    PlutusSimpleModel.waitUntil waitBeforeConsumingTx
    --------------------
    protocolDeployTxRun txDeployConfig txDeployParams protocolData
    --------------------
    PlutusSimpleModel.waitUntil waitBeforeConsumingTx
    --------------------
    return protocolData

------------------------------------------------------------

protocolUpdateRun ::  TestCompiledCodeScripts -> TxDeployConfig ->  TxUpdateConfig ->   TxDeployParams -> TxUpdateParams ->  [LedgerApiV2.PubKeyHash] -> PlutusSimpleModel.Run ProtocolData
protocolUpdateRun testCompiledCodeScripts txDeployConfig txUpdateConfig txDeployParams txUpdateParams users = do
    --------------------
    users' <- if null users then setupUsers txDeployParams  else return  users
    --------------------
    protocolData <- protocolDeployRun testCompiledCodeScripts txDeployConfig txDeployParams users'
    --------------------
    PlutusSimpleModel.waitUntil waitBeforeConsumingTx
    --------------------
    protocolData' <- protocolUpdateTxRun txUpdateConfig txDeployParams txUpdateParams protocolData  users'
    --------------------
    PlutusSimpleModel.waitUntil waitBeforeConsumingTx
    --------------------
    return protocolData'

------------------------------------------------------------

-- Function to determine expected error based on TxConfig
determineTxDeployExpectedError :: TxDeployConfig -> String
determineTxDeployExpectedError TxDeployConfig {tdcUseValidTxTimeRange = False} = "not isValidRange"
determineTxDeployExpectedError TxDeployConfig {tdcUseValidDatum = False}       = "not isCorrect_Output_Protocol_Datum"
determineTxDeployExpectedError _                                               =  "NOT DEFINED ERROR MESSAGE TO LOOKUP"

protocolDeployProperty :: TestCompiledCodeScripts -> TxDeployConfig ->TastyQC.Property
protocolDeployProperty testCompiledCodeScripts txDeployConfig =
    TastyQC.forAll  (txDeployParamsGen txDeployConfig ) $ \txDeployParams -> do
        if tdcIsValid txDeployConfig then
            goodCase2  txDeployParams (protocolDeployRun testCompiledCodeScripts txDeployConfig txDeployParams [])
        else do
            let errorExpected = determineTxDeployExpectedError txDeployConfig
            failCase2 txDeployParams  (protocolDeployRun testCompiledCodeScripts txDeployConfig txDeployParams []) errorExpected

------------------------------------------------------------

-- Function to determine expected error based on TxConfig
determineTxUpdateExpectedError :: TxUpdateConfig -> String
determineTxUpdateExpectedError TxUpdateConfig {tucUseValidTxTimeRange = False} = "not isValidRange"
determineTxUpdateExpectedError TxUpdateConfig {tucUseValidDatum = False}       = "not isCorrect_Output_Protocol_Datum_Updated"
determineTxUpdateExpectedError TxUpdateConfig {tucUseValidRedeemer = False}    = "not isCorrect_Output_Protocol_Datum_Updated"
determineTxUpdateExpectedError TxUpdateConfig {tucUseValidSignature = False}   = "not isSignedByAny admins nor isAdminTokenPresent"
determineTxUpdateExpectedError _                                               =  "NOT DEFINED ERROR MESSAGE TO LOOKUP"

protocolUpdateProperty :: TestCompiledCodeScripts -> TxDeployConfig -> TxUpdateConfig -> TastyQC.Property
protocolUpdateProperty testCompiledCodeScripts txDeployConfig txUpdateConfig =
    TastyQC.forAll (txDeployParamsGen txDeployConfig ) $ \txDeployParams -> do
        TastyQC.forAll (txUpdateParamsGen txUpdateConfig ) $ \txUpdateParams -> do
            if tucIsValid txUpdateConfig then
                goodCase2 txDeployParams (protocolUpdateRun testCompiledCodeScripts txDeployConfig txUpdateConfig txDeployParams txUpdateParams [])
            else do
                let errorExpected = determineTxUpdateExpectedError txUpdateConfig
                failCase2 txDeployParams (protocolUpdateRun testCompiledCodeScripts txDeployConfig txUpdateConfig txDeployParams txUpdateParams []) errorExpected

------------------------------------------------------------

