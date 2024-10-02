--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : TestUtils.Contracts.TxContext.Protocol
Description : M
-}
module TestUtils.Contracts.TxContext.Protocol where

-- Non-IOG imports

import           Prelude                         as P

-- IOG imports
import qualified Plutus.V2.Ledger.Api            as LedgerApiV2

-- Project imports

import qualified Generic.OnChainHelpers          as OnChainHelpers
import qualified Protocol.Constants              as T
import qualified Protocol.Protocol.Helpers       as ProtocolHelpers
import qualified Protocol.Protocol.Types         as ProtocolT
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ
import qualified Generic.Types as T

--------------------------------------------------------------------------------
-- Protocol Contract
--------------------------------------------------------------------------------

protocol_Create_TxContext :: TestParams -> LedgerApiV2.ScriptContext
protocol_Create_TxContext tp =
    mkContext
        |> setInputWithTxOufRef (protocol_spend_UTxO_And_TxOutRef_MockData tp)
        |> setOutputs [protocol_UTxO_MockData tp]
        |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpProtocolPolicyID_CS tp) T.protocolID_TN 1, ProtocolT.mkMintIDRedeemer)]
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

protocol_DatumUpdate_TxContext :: TestParams -> [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.ScriptContext
protocol_DatumUpdate_TxContext tp admins tokenAdminPolicy_CS =
    let
        input_Protocol_UTxO = protocol_UTxO_MockData tp
        input_Protocol_Datum = ProtocolT.getProtocol_DatumType_From_UTxO input_Protocol_UTxO
        -----------------
        output_Protocol_Datum = input_Protocol_Datum {
            ProtocolT.pdAdmins = admins,
            ProtocolT.pdTokenAdminPolicy_CS = tokenAdminPolicy_CS
        }
        output_Protocol_UTxO = input_Protocol_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    ProtocolT.mkDatum output_Protocol_Datum
            }
    in
        mkContext
            |> setInputsAndAddRedeemers [(input_Protocol_UTxO, ProtocolT.mkDatumUpdateRedeemer)]
            |> setOutputs [output_Protocol_UTxO]
            |> setSignatories (tpProtocolAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

protocol_UpdateMinADA_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
protocol_UpdateMinADA_TxContext tp newMinADA =
    let
        input_Protocol_UTxO = protocol_UTxO_MockData tp
        input_Protocol_Datum = ProtocolT.getProtocol_DatumType_From_UTxO input_Protocol_UTxO
        input_Protocol_Value = LedgerApiV2.txOutValue input_Protocol_UTxO
        -----------------
        output_Protocol_Datum = ProtocolHelpers.mkUpdated_Protocol_Datum_With_MinADAChanged
                input_Protocol_Datum
                newMinADA
        output_Protocol_UTxO = input_Protocol_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    ProtocolT.mkDatum output_Protocol_Datum
            , LedgerApiV2.txOutValue =
                changeValue_Amount
                    input_Protocol_Value
                    OnChainHelpers.adaAssetClass
                    newMinADA
            }
        -----------------
    in
        mkContext
            |> setInputsAndAddRedeemers [(input_Protocol_UTxO, ProtocolT.mkUpdateMinADARedeemer)]
            |> setOutputs [output_Protocol_UTxO]
            |> setSignatories (tpProtocolAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
