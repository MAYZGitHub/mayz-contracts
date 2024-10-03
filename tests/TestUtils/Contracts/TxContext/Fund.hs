--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : TestUtils.Contracts.TxContext.Fund
Description :
-}
module TestUtils.Contracts.TxContext.Fund where

-- Non-IOG imports

-- IOG imports
import qualified Plutus.V2.Ledger.Api                      as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Generic.OnChainHelpers                    as OnChainHelpers
import qualified Generic.Types                             as T
import qualified Protocol.Constants                        as T
import qualified Protocol.Fund.Helpers                     as FundHelpers
import qualified Protocol.Fund.Types                       as FundT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.FundHolding
import           TestUtils.Contracts.TxContext.InvestUnit 
import           TestUtils.Helpers
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------
-- Fund Contract
--------------------------------------------------------------------------------

fund_Create_TxContext :: TestParams -> LedgerApiV2.ScriptContext
fund_Create_TxContext tp =
    mkContext
        |> setInputWithTxOufRef (fund_spend_UTxO_And_TxOutRef_MockData tp)
        |> setInputsRef [protocol_UTxO_MockData tp]
        |> setOutputs [fund_UTxO_MockData tp, investUnit_UTxO_MockData tp]
        |> setMintAndAddRedeemers
            [( LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1
                <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 1
            , FundT.mkMintIDRedeemer)]
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

fund_Delete_TxContext :: TestParams -> LedgerApiV2.ScriptContext
fund_Delete_TxContext =  investUnit_Delete_TxContext

--------------------------------------------------------------------------------

fund_Deposit_TxContext :: TestParams -> LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.ScriptContext
fund_Deposit_TxContext = fundHolding_Deposit_TxContext

--------------------------------------------------------------------------------

fund_Withdraw_TxContext :: TestParams -> LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.ScriptContext
fund_Withdraw_TxContext = fundHolding_Withdraw_TxContext

--------------------------------------------------------------------------------

fund_DatumUpdate_TxContext :: TestParams -> [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.ScriptContext
fund_DatumUpdate_TxContext tp admins tokenAdminPolicy_CS =
    let
        input_Fund_UTxO = fund_UTxO_MockData tp
        input_Fund_Datum = FundT.getFund_DatumType_From_UTxO input_Fund_UTxO
        -----------------
        output_Fund_Datum = FundHelpers.mkUpdated_Fund_Datum_With_NormalChanges
                input_Fund_Datum
                admins tokenAdminPolicy_CS
        output_Fund_UTxO = input_Fund_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    FundT.mkDatum output_Fund_Datum
            }
    in
        mkContext
            |> setInputsAndAddRedeemers [(input_Fund_UTxO, FundT.mkDatumUpdateRedeemer)]
            |> setOutputs [output_Fund_UTxO]
            |> setSignatories (tpFundAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

fund_UpdateMinADA_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
fund_UpdateMinADA_TxContext tp newMinADA =
    let
        input_Fund_UTxO = fund_UTxO_MockData tp
        input_Fund_Datum = FundT.getFund_DatumType_From_UTxO input_Fund_UTxO
        input_Value = LedgerApiV2.txOutValue input_Fund_UTxO
        -----------------
        output_Fund_Datum = FundHelpers.mkUpdated_Fund_Datum_With_MinADAChanged
                input_Fund_Datum
                newMinADA
        output_Fund_UTxO = input_Fund_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    FundT.mkDatum output_Fund_Datum
            , LedgerApiV2.txOutValue =
                changeValue_Amount
                    input_Value
                    OnChainHelpers.adaAssetClass
                    newMinADA
            }
        -----------------
    in
        mkContext
            |> setInputsAndAddRedeemers [(input_Fund_UTxO, FundT.mkUpdateMinADARedeemer)]
            |> setOutputs [output_Fund_UTxO]
            |> setSignatories (tpFundAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

fund_FundHoldingAdd_TxContext :: TestParams -> LedgerApiV2.ScriptContext
fund_FundHoldingAdd_TxContext = fundHolding_Create_TxContext

--------------------------------------------------------------------------------

fund_FundHoldingDelete_TxContext :: TestParams -> LedgerApiV2.ScriptContext
fund_FundHoldingDelete_TxContext = fundHolding_Delete_TxContext

--------------------------------------------------------------------------------

fund_Finish_TxContext :: TestParams -> LedgerApiV2.POSIXTime -> LedgerApiV2.ScriptContext
fund_Finish_TxContext tp closedAt =
    let
        input_Fund_UTxO = fund_UTxO_MockData tp
        input_Fund_Datum = FundT.getFund_DatumType_From_UTxO input_Fund_UTxO
        -----------------
        output_Fund_Datum = FundHelpers.mkUpdated_Fund_Datum_With_ClosedAt
                input_Fund_Datum
                closedAt
        output_Fund_UTxO = input_Fund_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    FundT.mkDatum output_Fund_Datum
            }
    in
    mkContext
        |> setInputsAndAddRedeemers [(input_Fund_UTxO, FundT.mkFinishRedeemer closedAt)]
        |> setOutputs [output_Fund_UTxO]
        |> setSignatories (tpFundAdmins tp)
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
