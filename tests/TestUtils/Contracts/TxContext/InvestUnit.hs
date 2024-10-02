--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3
{- |
Module      : TestUtils.Contracts.TxContext.InvestUnit
Description :
-}
module TestUtils.Contracts.TxContext.InvestUnit where

-- Non-IOG imports
import           Prelude                         as P

-- IOG imports
import qualified Plutus.V2.Ledger.Api            as LedgerApiV2

-- Project imports
import qualified Generic.OnChainHelpers          as OnChainHelpers
import qualified Protocol.Fund.Holding.Types     as FundHoldingT
import qualified Protocol.InvestUnit.OnChain     as InvestUnit
import qualified Protocol.InvestUnit.Types       as InvestUnitT
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------
-- InvestUnit Contract
--------------------------------------------------------------------------------

investUnit_ReIndexing_TxContext :: TestParams -> LedgerApiV2.ScriptContext
investUnit_ReIndexing_TxContext tp =
    mkContext
        |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_With_Added_FundHolding_MockData tp,
                              uTxOForValidatorAsReference tp (tpFundHoldingValidator tp), uTxOForValidatorAsReference tp (tpInvestUnitValidator tp)]
        |> setInputsAndAddRedeemers [
            (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial),
            (investUnit_UTxO_MockData tp, InvestUnitT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial (oracleReIdxData tp) (oracleReIdxSignature tp))
            ]
        |> setOutputs [fundHolding_UTxO_After_Reidx_MockData tp investUnit_Initial investUnit_AfterReIdx, investUnit_UTxO_After_ReIdx_MockData tp]
        |> setSignatories (tpFundAdmins tp)
        |> setValidyRange (createValidRange (tpReIdxDate tp))

--------------------------------------------------------------------------------

investUnit_UpdateMinADA_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
investUnit_UpdateMinADA_TxContext tp newMinADA =
    let
        -----------------
        input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
        -----------------
        input_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO 
        input_Value = LedgerApiV2.txOutValue input_InvestUnit_UTxO
        -----------------
        output_Datum = InvestUnit.mkUpdated_InvestUnit_Datum_With_MinADAChanged
                input_Datum
                newMinADA
        output_UTxO = input_InvestUnit_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    InvestUnitT.mkDatum output_Datum
            , LedgerApiV2.txOutValue =
                changeValue_Amount
                    input_Value
                    OnChainHelpers.adaAssetClass
                    newMinADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [fund_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_InvestUnit_UTxO, InvestUnitT.mkUpdateMinADARedeemer)]
            |> setOutputs [output_UTxO]
            |> setSignatories (tpFundAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
