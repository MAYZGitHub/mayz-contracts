--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
module Helpers.Data where

-- Non-IOG imports
import qualified Control.Monad           as ControlMonad (replicateM)
import qualified Data.Word               as DataWord (Word8)
import           Prelude as P
import qualified Test.Tasty.QuickCheck as TastyQC
import qualified Data.ByteString         as BS

-- IOG imports
import qualified Ledger
import qualified Ledger.Address          as LedgerAddress
import qualified Ledger.Crypto           as LedgerCrypo
import qualified Plutus.Model            as PlutusSimpleModel
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import qualified PlutusTx.Builtins       as TxBuiltins
import qualified PlutusTx.Builtins.Class as TxBuiltinsClass
import qualified Wallet.Emulator.Types   as WalletEmulatorTypes
import qualified Ledger.Value              as LedgerValue

-- Project imports
import qualified Generic.OffChainHelpers as OffChainHelpers
import qualified Generic.Types           as T
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Constants        as T
import           TestUtils.TypesMAYZ --NOTE: se necesita para la isntancia de show de TxDeployParams

-----------------------------------------------------------------

data TxDeployConfig
    = TxDeployConfig
          { tdcIsValid                :: Bool
          , tdcUseValidTxTimeRange    :: Bool
          , tdcUseValidDatum          :: Bool
          }
    deriving (Show)

data TxUpdateConfig
    = TxUpdateConfig
          { tucIsValid                :: Bool
          , tucUseValidTxTimeRange    :: Bool
          , tucUseValidDatum          :: Bool
          , tucUseValidRedeemer     :: Bool
          , tucUseValidSignature     :: Bool
          , tucUseAdminToken     :: Bool
          , tucUseEmergencyToken     :: Bool
          }
    deriving (Show)

data TxDeployParams
    = TxDeployParams
          {
            tdpTokenEmergencyAdminPolicy_CS    :: LedgerApiV2.CurrencySymbol
          , tdpTokenAdminPolicy_CS             :: LedgerApiV2.CurrencySymbol
          , tdpTokenMAYZ_CS                    :: LedgerApiV2.CurrencySymbol
          , tdpTokenMAYZ_TN                    :: LedgerApiV2.TokenName
          , tdpOraclePrivateKey                :: WalletEmulatorTypes.XPrv
          , tdpOraclePaymentPubKey             :: LedgerAddress.PaymentPubKey
          , tdpFundCategories                  :: [ProtocolT.FundCategory]
          , tdpFundLifeTime                    :: ProtocolT.MinMaxDef LedgerApiV2.POSIXTime
          , tdpRequiredMAYZForSellOffer        :: Integer
          , tdpRequiredMAYZForBuyOrder         :: Integer
          , tdpCommissionFund_PerYear_InBPx1e3 :: ProtocolT.MinMaxDef Integer
          , tdpCommissionSellOffer_InBPx1e3    :: ProtocolT.MinMaxDef Integer
          , tdpCommissionBuyOrder_InBPx1e3     :: ProtocolT.MinMaxDef Integer
          , tdpShare_InBPx1e2_Protocol          :: Integer
          , tdpShare_InBPx1e2_Delegators        :: Integer
          , tdpShare_InBPx1e2_Managers          :: Integer
          }
    deriving (P.Show)

data TxUpdateParams
    = TxUpdateParams
          {
            tupTokenAdminPolicy_CS             :: LedgerApiV2.CurrencySymbol
          , tupTokenMAYZ_CS                    :: LedgerApiV2.CurrencySymbol
          , tupTokenMAYZ_TN                    :: LedgerApiV2.TokenName
          , tupOraclePrivateKey                :: WalletEmulatorTypes.XPrv
          , tupOraclePaymentPubKey             :: LedgerAddress.PaymentPubKey
          , tupFundCategories                  :: [ProtocolT.FundCategory]
          , tupFundLifeTime                    :: ProtocolT.MinMaxDef LedgerApiV2.POSIXTime
          , tupRequiredMAYZForSellOffer        :: Integer
          , tupRequiredMAYZForBuyOrder         :: Integer
          , tupCommissionFund_PerYear_InBPx1e3 :: ProtocolT.MinMaxDef Integer
          , tupCommissionSellOffer_InBPx1e3    :: ProtocolT.MinMaxDef Integer
          , tupCommissionBuyOrder_InBPx1e3     :: ProtocolT.MinMaxDef Integer
          , tupShare_InBPx1e2_Protocol          :: Integer
          , tupShare_InBPx1e2_Delegators        :: Integer
          , tupShare_InBPx1e2_Managers          :: Integer
          }
    deriving (Show)

data ProtocolScripts
    = ProtocolScripts
          { psProtocolPolicyID         :: LedgerApiV2.MintingPolicy
          , psProtocolPolicyID_CS      :: LedgerApiV2.CurrencySymbol
          , psProtocolValidator        :: LedgerApiV2.Validator
          , psProtocolValidator_Hash   :: LedgerApiV2.ValidatorHash
          , psScriptPolicyID           :: LedgerApiV2.MintingPolicy
          , psScriptPolicyID_CS        :: LedgerApiV2.CurrencySymbol
          , psScriptValidator          :: LedgerApiV2.Validator
          , psScriptValidator_Hash     :: LedgerApiV2.ValidatorHash
          , psSellOfferPolicyID        :: LedgerApiV2.MintingPolicy
          , psSellOfferPolicyID_CS     :: LedgerApiV2.CurrencySymbol
          , psSellOfferValidator       :: LedgerApiV2.Validator
          , psSellOfferValidator_Hash  :: LedgerApiV2.ValidatorHash
          , psBuyOrderPolicyID         :: LedgerApiV2.MintingPolicy
          , psBuyOrderPolicyID_CS      :: LedgerApiV2.CurrencySymbol
          , psBuyOrderValidator        :: LedgerApiV2.Validator
          , psBuyOrderValidator_Hash   :: LedgerApiV2.ValidatorHash
          , psDelegationPolicyID       :: LedgerApiV2.MintingPolicy
          , psDelegationPolicyID_CS    :: LedgerApiV2.CurrencySymbol
          , psDelegationValidator      :: LedgerApiV2.Validator
          , psDelegationValidator_Hash :: LedgerApiV2.ValidatorHash
          , psInvestUnitValidator      :: LedgerApiV2.Validator
          , psInvestUnitValidator_Hash :: LedgerApiV2.ValidatorHash
          }
    deriving (Show)


data ProtocolData
    = ProtocolData
          { 
            pdProtocolScripts :: ProtocolScripts
          , pdProtocolCreator     :: T.WalletPaymentPKH
          , pdProtocolAdmins      :: [T.WalletPaymentPKH]
          , pdDelegatorsAdmins    :: [T.WalletPaymentPKH]
          }
    deriving (Show)

-----------------------------------------------------------------

-- | 'setupUsers' sets up two users with an initial amount of ADA.
setupUsers :: TxDeployParams -> PlutusSimpleModel.Run [LedgerApiV2.PubKeyHash]
setupUsers txDeployParams = do
    let protocolTokenEmergencyAdmin_AC = LedgerValue.AssetClass (tdpTokenEmergencyAdminPolicy_CS txDeployParams, T.protocolTokenEmergencyAdmin_TN)
        protocolTokenEmergencyAdmin_Value = LedgerValue.assetClassValue protocolTokenEmergencyAdmin_AC 1
        protocolTokenAdmin_AC = LedgerValue.AssetClass (tdpTokenAdminPolicy_CS txDeployParams, T.protocolTokenAdmin_TN)
        protocolTokenAdmin_Value = LedgerValue.assetClassValue  protocolTokenAdmin_AC 1
    
    ControlMonad.replicateM 5 $ PlutusSimpleModel.newUser (PlutusSimpleModel.ada (PlutusSimpleModel.Lovelace 100_000_000) <> protocolTokenEmergencyAdmin_Value <> protocolTokenAdmin_Value)

waitBeforeConsumingTx :: LedgerApiV2.POSIXTime
waitBeforeConsumingTx = 1000

-----------------------------------------------------------------

gen56BuiltinByteString :: TastyQC.Gen TxBuiltins.BuiltinByteString
gen56BuiltinByteString = TxBuiltinsClass.toBuiltin . BS.pack <$> TastyQC.vectorOf 28 (TastyQC.choose (0, 255) :: TastyQC.Gen DataWord.Word8)

pubKeyHashGen :: TastyQC.Gen LedgerApiV2.PubKeyHash
pubKeyHashGen = LedgerApiV2.PubKeyHash <$> gen56BuiltinByteString

currencySymbolGen :: TastyQC.Gen LedgerApiV2.CurrencySymbol
currencySymbolGen = do
        LedgerApiV2.CurrencySymbol <$> gen56BuiltinByteString

tokenNameGen :: TastyQC.Gen LedgerApiV2.TokenName
tokenNameGen= do
        bytes <- TastyQC.vectorOf 2 TastyQC.arbitrary
        let byteString = BS.pack bytes
        let builtinByteString = TxBuiltins.toBuiltin byteString
        return $ LedgerApiV2.TokenName builtinByteString

minMaxDefIntegerGen :: TastyQC.Gen (ProtocolT.MinMaxDef Integer)
minMaxDefIntegerGen = do
    minValue <- integer_QEQ_Zero_Gen
    maxValue <- TastyQC.suchThat (TastyQC.arbitrary `TastyQC.suchThat` (>= minValue)) (>= minValue)
    defValue <- TastyQC.choose (minValue, maxValue)
    return $ ProtocolT.MinMaxDef minValue maxValue defValue

minMaxDefPOSIXTimeGen :: TastyQC.Gen (ProtocolT.MinMaxDef LedgerApiV2.POSIXTime)
minMaxDefPOSIXTimeGen = do
    minValue <- integer_QEQ_Zero_Gen
    maxValue <- TastyQC.suchThat (TastyQC.arbitrary `TastyQC.suchThat` (>= minValue)) (>= minValue)
    defValue <- TastyQC.choose (minValue, maxValue)
    return $ ProtocolT.MinMaxDef (LedgerApiV2.POSIXTime  minValue) (LedgerApiV2.POSIXTime maxValue )(LedgerApiV2.POSIXTime defValue)

fundCategoryGen :: TastyQC.Gen ProtocolT.FundCategory
fundCategoryGen = ((ProtocolT.FundCategory <$> integer_QEQ_Zero_Gen) <*> integer_QEQ_Zero_Gen) <*> integer_QEQ_Zero_Gen

fundCategoryGenValid :: Bool ->  TastyQC.Gen ProtocolT.FundCategory
fundCategoryGenValid swValid = ((ProtocolT.FundCategory <$> integer_QEQ_Zero_GenValid swValid) <*> integer_QEQ_Zero_GenValid swValid ) <*> integer_QEQ_Zero_GenValid swValid

integer_QEQ_One_Gen :: TastyQC.Gen Integer
integer_QEQ_One_Gen = TastyQC.arbitrary `TastyQC.suchThat` \x -> x >= 1

integer_QEQ_Zero_Gen :: TastyQC.Gen Integer
integer_QEQ_Zero_Gen = TastyQC.arbitrary `TastyQC.suchThat` \x -> x >= 0

integer_QEQ_One_GenValid :: Bool -> TastyQC.Gen Integer
integer_QEQ_One_GenValid swValid = TastyQC.arbitrary `TastyQC.suchThat` \x -> do 
        if swValid then 
            x >= 1
        else
            x < 1

integer_QEQ_Zero_GenValid :: Bool -> TastyQC.Gen Integer
integer_QEQ_Zero_GenValid swValid = TastyQC.arbitrary `TastyQC.suchThat` \x -> do 
        if swValid then 
            x >= 0
        else
            x < 0

share_InBPx1e2_Gen :: TastyQC.Gen (Integer, Integer, Integer)
share_InBPx1e2_Gen = do
    x <- TastyQC.choose (0, 980_000) 
    y <- TastyQC.choose (0, 990_000 - x) 
    let z = 1_000_000 - x - y -- z is guaranteed to be positive
    return (x, y, z)

-- txConfigGen :: Bool -> TastyQC.Gen TxConfig
-- txConfigGen swValid = do
--     bools <- if swValid
--              then return $ replicate 3 True
--              else do
--                  -- Generate a list where at least one is False
--                  let genBools = do
--                          bs <- mapM (const TastyQC.arbitrary) [1..3]
--                          if and bs  -- If all are True, make one False
--                             then TastyQC.elements (map (\i -> take i bs ++ [False] ++ drop (i + 1) bs) [0..2])
--                             else return bs
--                  genBools
--     return $ TxConfig swValid (head bools) (bools !! 1) (bools !! 2)

txDeployParamsGen :: TxDeployConfig -> TastyQC.Gen TxDeployParams
txDeployParamsGen txDeployConfig = do
    let
        oraclePrivateKey =
            LedgerCrypo.generateFromSeed' $
                OffChainHelpers.stringToStrictByteString
                    "dad cupboard hotel cause mansion feature oppose prevent \
                    \install venue finish galaxy tone green volcano neglect \
                    \coil toast future exchange prize social next tape"
    let
        oraclePublicKey = LedgerCrypo.toPublicKey oraclePrivateKey
    let
        oraclePPK = Ledger.PaymentPubKey oraclePublicKey

    -- let tokenEmergencyAdminPolicyCoin_AC = fakeCoin (PlutusSimpleModel.FakeCoin T.protocolTokenEmergencyAdmin_TN )
    --     (tokenEmergencyAdminPolicyCoin_CS,_) = LedgerApiV2.UnAssecClass tokenEmergencyAdminPolicyCoin_AC

    --     tokenAdminPolicy_AC = fakeCoin (PlutusSimpleModel.FakeCoin T.protocolTokenAdmin_TN )
    --     (tokenAdminPolicy_CS,_) = LedgerApiV2.UnAssecClass tokenAdminPolicy_AC

    --     tokenMAYZ_AC = fakeCoin (PlutusSimpleModel.FakeCoin "MAYZ" )
    --     (tokenMAYZ_CS,_) = LedgerApiV2.UnAssecClass tokenMAYZ_AC

    tpTokenEmergencyAdminPolicy_CS               <- currencySymbolGen
    tpTokenAdminPolicy_CS                        <- currencySymbolGen
    tpTokenMAYZ_CS                               <- currencySymbolGen
    tpTokenMAYZ_TN                               <- tokenNameGen
    let tpOraclePrivateKey                           =oraclePrivateKey
        tpOraclePaymentPubKey                        =oraclePPK
    tpFundCategories                               <-  TastyQC.vectorOf 1 ( fundCategoryGenValid (tdcUseValidDatum txDeployConfig))
    tpFundLifeTime                               <- minMaxDefPOSIXTimeGen
    tpRequiredMAYZForSellOffer                   <- integer_QEQ_Zero_Gen
    tpRequiredMAYZForBuyOrder                    <- integer_QEQ_Zero_Gen 
    tpCommissionFund_PerYear_InBPx1e3 <- minMaxDefIntegerGen
    tpCommissionSellOffer_InBPx1e3    <- minMaxDefIntegerGen
    tpCommissionBuyOrder_InBPx1e3     <- minMaxDefIntegerGen
    (tpShare_InBPx1e2_Protocol, tpShare_InBPx1e2_Delegators, tpShare_InBPx1e2_Managers)                     <- share_InBPx1e2_Gen
    return $ TxDeployParams
                tpTokenEmergencyAdminPolicy_CS
                tpTokenAdminPolicy_CS
                tpTokenMAYZ_CS
                tpTokenMAYZ_TN
                tpOraclePrivateKey
                tpOraclePaymentPubKey
                tpFundCategories
                tpFundLifeTime
                tpRequiredMAYZForSellOffer
                tpRequiredMAYZForBuyOrder
                tpCommissionFund_PerYear_InBPx1e3
                tpCommissionSellOffer_InBPx1e3
                tpCommissionBuyOrder_InBPx1e3
                tpShare_InBPx1e2_Protocol
                tpShare_InBPx1e2_Delegators
                tpShare_InBPx1e2_Managers


txUpdateParamsGen :: TxUpdateConfig -> TastyQC.Gen TxUpdateParams
txUpdateParamsGen txUpdateConfig = do
    let
        oraclePrivateKey =
            LedgerCrypo.generateFromSeed' $
                OffChainHelpers.stringToStrictByteString
                    "dad cupboard hotel cause mansion feature oppose prevent \
                    \install venue finish galaxy tone green volcano neglect \
                    \coil toast future exchange prize social next tape"
    let
        oraclePublicKey = LedgerCrypo.toPublicKey oraclePrivateKey
    let
        oraclePPK = Ledger.PaymentPubKey oraclePublicKey

    tpTokenAdminPolicy_CS                        <- currencySymbolGen
    tpTokenMAYZ_CS                               <- currencySymbolGen
    tpTokenMAYZ_TN                               <- tokenNameGen
    let tpOraclePrivateKey                           =oraclePrivateKey
        tpOraclePaymentPubKey                        =oraclePPK
    tpFundCategories                               <-  TastyQC.vectorOf 1 ( fundCategoryGenValid (tucUseValidDatum txUpdateConfig))
    tpFundLifeTime                               <- minMaxDefPOSIXTimeGen
    tpRequiredMAYZForSellOffer                   <- integer_QEQ_Zero_Gen
    tpRequiredMAYZForBuyOrder                    <- integer_QEQ_Zero_Gen 
    tpCommissionFund_PerYear_InBPx1e3 <- minMaxDefIntegerGen
    tpCommissionSellOffer_InBPx1e3    <- minMaxDefIntegerGen
    tpCommissionBuyOrder_InBPx1e3     <- minMaxDefIntegerGen
    (tpShare_InBPx1e2_Protocol, tpShare_InBPx1e2_Delegators, tpShare_InBPx1e2_Managers)                     <- share_InBPx1e2_Gen
    return $ TxUpdateParams
                tpTokenAdminPolicy_CS
                tpTokenMAYZ_CS
                tpTokenMAYZ_TN
                tpOraclePrivateKey
                tpOraclePaymentPubKey
                tpFundCategories
                tpFundLifeTime
                tpRequiredMAYZForSellOffer
                tpRequiredMAYZForBuyOrder
                tpCommissionFund_PerYear_InBPx1e3
                tpCommissionSellOffer_InBPx1e3
                tpCommissionBuyOrder_InBPx1e3
                tpShare_InBPx1e2_Protocol
                tpShare_InBPx1e2_Delegators
                tpShare_InBPx1e2_Managers