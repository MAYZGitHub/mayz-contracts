--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Automatic.HelpersMAYZ where

--------------------------------------------------------------------------------

-- Non-IOG imports
import           Prelude                              as P hiding ((<>))

-- IOG imports
import qualified Plutus.V2.Ledger.Api                 as LedgerApiV2

-- Project imports
import qualified Protocol.Constants                   as T
import qualified Protocol.Fund.Holding.Types          as FundHoldingT
import qualified Protocol.Fund.Types                  as FundT
import qualified Protocol.Fund.InvestUnit.Types            as InvestUnitT
import qualified Protocol.Protocol.Types              as ProtocolT
import qualified Protocol.SwapOffer.Types             as SwapOfferT
import           TestUtils.Automatic.ContextGenerator
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.HelpersMAYZ
import           TestUtils.TypesMAYZ

----------------------------------------------------------------------------------------

getEntityTestConfig :: TestParams -> TestEntity -> TxSpecsEntityTestConfig
getEntityTestConfig _ Protocol_TestEntity = TxSpecsEntityTestConfig
    { testInvalidValueOtherTokens = []
    }
getEntityTestConfig tp Fund_TestEntity = TxSpecsEntityTestConfig
    {
        testInvalidValueOtherTokens = [InvalidValueOtherToken "MAYZ" (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp)]
    }
getEntityTestConfig _ FundHolding_TestEntity = TxSpecsEntityTestConfig
    {
        testInvalidValueOtherTokens = []
    }
getEntityTestConfig _ InvestUnit_TestEntity = TxSpecsEntityTestConfig
    {
        testInvalidValueOtherTokens = []
    }
getEntityTestConfig tp SwapOffer_TestEntity = TxSpecsEntityTestConfig
    {
        testInvalidValueOtherTokens = [InvalidValueOtherToken "MAYZ" (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp)]
    }
getEntityTestConfig _ _ =  P.error "getEntityTestConfig: not implemented for this TestEntity"

----------------------------------------------------------------------------------------

txOut_With_TestEntity_Gen :: TestParams
                                -> LedgerApiV2.TxOut
                                -> TestEntity
                                -> TxOutOptions
                                -> [LedgerApiV2.TxOut]

txOut_With_TestEntity_Gen tp valid_UTxO Protocol_TestEntity options =
    let
        -----------------
        datum = ProtocolT.getProtocol_DatumType_From_UTxO valid_UTxO
        datumUpdated =
            datum { ProtocolT.pdMinADA = ProtocolT.pdMinADA datum + sum_ANY_INVALID_NUMBER * 2}
        -----------------
        invalidDatumData' = Just $ ProtocolT.mkDatum datumUpdated
        invalidDatumType' = Just (fund_Datum_MockData tp)
        invalidDatumNonExist' = Just fakeDatumEmpty
        -----------------
        protocol_UTxO = txOut_Gen valid_UTxO (tpProtocolPolicyID_CS tp) T.protocolID_TN invalidDatumData' invalidDatumType' invalidDatumNonExist' options
    in
        protocol_UTxO

txOut_With_TestEntity_Gen tp valid_UTxO Fund_TestEntity options =
    let
        -----------------
        datum = FundT.getFund_DatumType_From_UTxO valid_UTxO
        datumUpdated =
            datum { FundT.fdMinADA = FundT.fdMinADA datum + sum_ANY_INVALID_NUMBER * 2}
        -----------------
        invalidDatumData' = Just $ FundT.mkDatum datumUpdated
        invalidDatumType' = Just (protocol_Datum_MockData tp)
        invalidDatumNonExist' = Just fakeDatumEmpty
        -----------------
        fund_UTxO = txOut_Gen valid_UTxO (tpFundPolicy_CS tp) T.fundID_TN invalidDatumData' invalidDatumType' invalidDatumNonExist' options
    in
        fund_UTxO

txOut_With_TestEntity_Gen tp valid_UTxO FundHolding_TestEntity options =
    let
        -----------------
        datum = FundHoldingT.getFundHolding_DatumType_From_UTxO valid_UTxO
        datumUpdated =
            datum { FundHoldingT.hdMinADA = FundHoldingT.hdMinADA datum + sum_ANY_INVALID_NUMBER * 2}
        -----------------
        invalidDatumData' = Just $ FundHoldingT.mkDatum datumUpdated
        invalidDatumType' = Just (fund_Datum_MockData tp)
        invalidDatumNonExist' = Just fakeDatumEmpty
        -----------------
        fhIndex = FundHoldingT.hdFundHolding_Index datumUpdated
        -----------------
        fundHolding_UTxO = txOut_Gen valid_UTxO (tpFundHoldingPolicyID_CS tp) (mkFundHoldingID_TN fhIndex) invalidDatumData' invalidDatumType' invalidDatumNonExist' options
    in
        fundHolding_UTxO

txOut_With_TestEntity_Gen tp valid_UTxO InvestUnit_TestEntity options =
    let
        -----------------
        datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO valid_UTxO
        datumUpdated =
            datum { InvestUnitT.iudMinADA = InvestUnitT.iudMinADA datum + sum_ANY_INVALID_NUMBER * 2}
        -----------------
        invalidDatumData' = Just $ InvestUnitT.mkDatum datumUpdated
        invalidDatumType' = Just (protocol_Datum_MockData tp)
        invalidDatumNonExist' = Just fakeDatumEmpty
        -----------------
        investUnit_UTxO = txOut_Gen valid_UTxO (tpFundPolicy_CS tp) T.investUnitID_TN invalidDatumData' invalidDatumType' invalidDatumNonExist' options
    in
        investUnit_UTxO


txOut_With_TestEntity_Gen tp valid_UTxO SwapOffer_TestEntity options =
    let
        -----------------
        datum = SwapOfferT.getSwapOffer_DatumType_From_UTxO valid_UTxO
        datumUpdated =
            datum { SwapOfferT.sodMinADA = SwapOfferT.sodMinADA datum + sum_ANY_INVALID_NUMBER * 2}
        -----------------
        invalidDatumData' = Just $ SwapOfferT.mkDatum datumUpdated
        invalidDatumType' = Just (protocol_Datum_MockData tp)
        invalidDatumNonExist' = Just fakeDatumEmpty
        -----------------
        swapOffer_UTxO = txOut_Gen valid_UTxO (tpSwapOfferPolicyID_CS tp) T.swapOfferID_TN invalidDatumData' invalidDatumType' invalidDatumNonExist' options
    in
        swapOffer_UTxO

txOut_With_TestEntity_Gen _ _ _ _ = P.error "txOut_With_TestEntity_Gen: not implemented for this TestEntity"

--------------------------------------------------------------------------------

mint_Value_With_TestToken_Gen :: TestParams
                                    -> (TestToken, PolicyTestRedeemer)
                                    -> Integer
                                    -> MintOptions
                                    -> [(LedgerApiV2.Value, LedgerApiV2.Redeemer)]

mint_Value_With_TestToken_Gen tp (ProtocolID_TestToken, Protocol_MintID_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemeer = ProtocolT.mkMintIDRedeemer
        invalidRedeemerData = Nothing
        invalidRedeemerType = Nothing
        invalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        mint_ProtocolID_gen = mint_Value_Gen (tpProtocolPolicyID_CS tp) T.protocolID_TN validAmount validRedeemeer invalidRedeemerData invalidRedeemerType invalidRedeemerNonExist options
        -----------------
    in
        mint_ProtocolID_gen

mint_Value_With_TestToken_Gen tp (FundID_TestToken, Fund_MintID_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemeer = FundT.mkMintIDRedeemer
        invalidRedeemerData = Nothing
        invalidRedeemerType = Just FundT.mkMintFTRedeemer
        invalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        mint_FundID_gen = mint_Value_Gen (tpFundPolicy_CS tp) T.fundID_TN validAmount validRedeemeer invalidRedeemerData invalidRedeemerType invalidRedeemerNonExist options
        -----------------
    in
        mint_FundID_gen

mint_Value_With_TestToken_Gen tp (FundID_TestToken, Fund_BurnID_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemer = FundT.mkBurnIDRedeemer
        invalidRedeemerData' = Nothing
        invalidRedeemerType' = Just FundT.mkMintFTRedeemer
        invalidRedeemerNonExist' = Just fakeRedeemerEmpty
        -----------------
        burn_FundID_gen = mint_Value_Gen (tpFundPolicy_CS tp) T.fundID_TN (-validAmount) validRedeemer invalidRedeemerData' invalidRedeemerType' invalidRedeemerNonExist' options
        -----------------
    in
        burn_FundID_gen

mint_Value_With_TestToken_Gen tp (InvestUnitID_TestToken, Fund_MintID_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemeer = FundT.mkMintIDRedeemer
        invalidRedeemerData = Nothing
        invalidRedeemerType = Just FundT.mkBurnIDRedeemer
        invalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        mint_FundID_gen = mint_Value_Gen (tpFundPolicy_CS tp) T.investUnitID_TN validAmount validRedeemeer invalidRedeemerData invalidRedeemerType invalidRedeemerNonExist options
        -----------------
    in
        mint_FundID_gen

mint_Value_With_TestToken_Gen tp (InvestUnitID_TestToken, Fund_BurnID_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemer = FundT.mkBurnIDRedeemer
        invalidRedeemerData' = Nothing
        invalidRedeemerType' = Just FundT.mkMintIDRedeemer
        invalidRedeemerNonExist' = Just fakeRedeemerEmpty
        -----------------
        burn_FundID_gen = mint_Value_Gen (tpFundPolicy_CS tp) T.investUnitID_TN (-validAmount) validRedeemer invalidRedeemerData' invalidRedeemerType' invalidRedeemerNonExist' options
        -----------------
    in
        burn_FundID_gen

mint_Value_With_TestToken_Gen tp (FundFT_TestToken, Fund_MintFT_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemeer = FundT.mkMintFTRedeemer
        invalidRedeemerData = Nothing
        invalidRedeemerType = Just FundT.mkBurnFTRedeemer
        invalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        mint_FundID_gen = mint_Value_Gen (tpFundPolicy_CS tp) (tpFundFT_TN tp) validAmount validRedeemeer invalidRedeemerData invalidRedeemerType invalidRedeemerNonExist options
        -----------------
    in
        mint_FundID_gen

mint_Value_With_TestToken_Gen tp (FundFT_TestToken, Fund_BurnFT_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemer = FundT.mkBurnFTRedeemer
        invalidRedeemerData' = Nothing
        invalidRedeemerType' = Just FundT.mkMintFTRedeemer
        invalidRedeemerNonExist' = Just fakeRedeemerEmpty
        -----------------
        burn_FundID_gen = mint_Value_Gen (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-validAmount) validRedeemer invalidRedeemerData' invalidRedeemerType' invalidRedeemerNonExist' options
        -----------------
    in
        burn_FundID_gen

mint_Value_With_TestToken_Gen tp (FundHoldingID_TestToken, FundHolding_MintID_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemeer = FundHoldingT.mkMintIDRedeemer
        invalidRedeemerData = Nothing
        invalidRedeemerType = Just FundHoldingT.mkBurnIDRedeemer
        invalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        mint_FundHoldingID_gen = mint_Value_Gen (tpFundHoldingPolicyID_CS tp) (mkFundHoldingID_TN 0) validAmount validRedeemeer invalidRedeemerData invalidRedeemerType invalidRedeemerNonExist options
        -----------------
    in
        mint_FundHoldingID_gen

mint_Value_With_TestToken_Gen tp (FundHoldingID_TestToken, FundHolding_BurnID_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemer = FundHoldingT.mkBurnIDRedeemer
        invalidRedeemerData' = Nothing
        invalidRedeemerType' = Just FundHoldingT.mkMintIDRedeemer
        invalidRedeemerNonExist' = Just fakeRedeemerEmpty
        -----------------
        burn_FundHoldingID_gen = mint_Value_Gen (tpFundHoldingPolicyID_CS tp) (mkFundHoldingID_TN 0) (-validAmount) validRedeemer invalidRedeemerData' invalidRedeemerType' invalidRedeemerNonExist' options
        -----------------
    in
        burn_FundHoldingID_gen

mint_Value_With_TestToken_Gen tp (SwapOfferID_TestToken, SwapOffer_MintID_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemeer = SwapOfferT.mkMintIDRedeemer
        invalidRedeemerData = Nothing
        invalidRedeemerType = Just SwapOfferT.mkBurnIDRedeemer
        invalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        mint_SwapOfferID_gen = mint_Value_Gen (tpSwapOfferPolicyID_CS tp) T.swapOfferID_TN validAmount validRedeemeer invalidRedeemerData invalidRedeemerType invalidRedeemerNonExist options
        -----------------
    in
        mint_SwapOfferID_gen

mint_Value_With_TestToken_Gen tp (SwapOfferID_TestToken, SwapOffer_BurnID_TestRedeemer) validAmount options =
    let
        -----------------
        validRedeemer = SwapOfferT.mkBurnIDRedeemer
        invalidRedeemerData' = Nothing
        invalidRedeemerType' = Just SwapOfferT.mkMintIDRedeemer
        invalidRedeemerNonExist' = Just fakeRedeemerEmpty
        -----------------
        burn_SwapOfferID_gen = mint_Value_Gen (tpSwapOfferPolicyID_CS tp) T.swapOfferID_TN (-validAmount) validRedeemer invalidRedeemerData' invalidRedeemerType' invalidRedeemerNonExist' options
        -----------------
    in
        burn_SwapOfferID_gen
        
mint_Value_With_TestToken_Gen _ _ _ _ = P.error "mint_Value_With_TestToken_Gen: not implemented for this TestToken"

--------------------------------------------------------------------------------

