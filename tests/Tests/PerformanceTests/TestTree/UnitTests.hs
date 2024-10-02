{-# LANGUAGE DataKinds #-}
--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3
module TestTree.UnitTests where

------------------------------------------------------------------------------------------
-- External Imports
------------------------------------------------------------------------------------------

import qualified Ledger
import qualified Ledger.Crypto           as Crypto
import qualified Plutus.Model            as PlutusSimpleModel
import           PlutusTx.Prelude        hiding (($), (.))
import           Prelude
import qualified Test.Tasty              as Tasty

------------------------------------------------------------------------------------------
-- Internal Imports
------------------------------------------------------------------------------------------
import qualified Generic.OffChainHelpers as OffChainHelpers
import           Helpers.PSM
import           Helpers.Validator
import           TestUtils.TypesMAYZ

------------------------------------------------------------------------------------------

unit_Tests :: TestParams -> Tasty.TestTree
unit_Tests tp =
    Tasty.testGroup
        "Unit Tests"
        [ signatureTests tp
        ]

------------------------------------------------------------------------------------------

signatureTests :: TestParams -> Tasty.TestTree
signatureTests _ =
    let
        message = OffChainHelpers.stringToBuiltinByteString "" :: BuiltinByteString
        -----------------
        oraclePrivateKey = Crypto.generateFromSeed' $ OffChainHelpers.stringToStrictByteString "dad cupboard hotel cause mansion feature oppose prevent install venue finish galaxy tone green volcano neglect coil toast future exchange prize social next tape"
        oraclePublicKey = Crypto.toPublicKey oraclePrivateKey
        oraclePPK = Ledger.PaymentPubKey oraclePublicKey
        oracleSignature = Crypto.sign' message oraclePrivateKey
        -----------------
        oracleFalsePrivateKey = Crypto.generateFromSeed' "he eeeee he ds fd gg ge eew rer trt erw rwerwe trter gfgdf gfdgdf rtet trtre treter ghfhgf treter gfdgdf tretre gfdgdf tretre"
        oracleFalsePublicKey = Crypto.toPublicKey oracleFalsePrivateKey
        oracleFalsePPK = Ledger.PaymentPubKey oracleFalsePublicKey
        -----------------
        datum = MkCustomDatum message oracleSignature
        redeemerOK = MkCustomRedeemer oraclePPK
        redeemerBad = MkCustomRedeemer oracleFalsePPK
    in
        -----------------

        do
            Tasty.testGroup
                "Signature validation"
                [ good "User 1 locks and user 2 takes with Good Oracle PK must succeed" $ testScript datum redeemerOK
                , bad "User 1 locks and user 2 takes with Bad Oracle PK must fail" $ testScript datum redeemerBad
                ]
    where
        bad msg = good msg . PlutusSimpleModel.mustFail
        good = PlutusSimpleModel.testNoErrors (PlutusSimpleModel.adaValue 10_000_000) PlutusSimpleModel.defaultBabbage

------------------------------------------------------------------------------------------
