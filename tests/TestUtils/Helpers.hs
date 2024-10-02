{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
{- |
Module      : TestUtils.Helpers
Description : Common Mock Data and Auxiliary Functions for testing.
-}
module TestUtils.Helpers where

--------------------------------------------------------------------------------

-- Non-IOG imports
import qualified Control.Monad           as ControlMonad
import qualified Data.List               as DataList
import qualified Data.Maybe              as DataMaybe
import qualified Debug.Trace             as DebugTrace
import qualified Plutus.Model            as PlutusSimpleModel
import           Prelude                 as P
import qualified System.Environment      as SystemEnvironment
import qualified Test.QuickCheck         as QC
import qualified Test.Tasty              as Tasty
import qualified Text.Read               as TextRead
import qualified GHC.IO as GHCIO

-- IOG imports
import qualified Ledger.Ada              as LedgerAda
import qualified Ledger.Value            as LedgerValue
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import           PlutusTx.Prelude        as Ptx hiding (($), (*), (+), (++), (-), (.), (<>), (==))

-- Project imports

import qualified Generic.OffChainHelpers as OffChainHelpers
import           TestUtils.TypesMAYZ


--------------------------------------------------------------------------------

traceIf :: Bool -> String -> IO ()
traceIf True msg = putStrLn msg
traceIf False _  = return ()

unsafePerformIOTraceIf :: Bool -> String -> ()
unsafePerformIOTraceIf swTrace msg = GHCIO.unsafePerformIO $ traceIf swTrace msg

--------------------------------------------------------------------------------

debugTraceIf :: (Monad m) => Bool -> String -> m ()
debugTraceIf swTrace msg = ControlMonad.when swTrace $ DebugTrace.trace msg (return ())

debugTraceIf_ :: Bool -> String -> a -> a
debugTraceIf_ True msg val = DebugTrace.trace msg val
debugTraceIf_ False _ val = val

debugTraceIfM :: Bool -> String -> QC.Gen ()
debugTraceIfM swTrace msg =
    -- para que funcione tiene que llemarse:  !_ <- debugTraceIfM swTrace "MESSAGE"
    ControlMonad.when swTrace $ QC.frequency [(1, DebugTrace.trace msg (return ()))]

--------------------------------------------------------------------------------

getNumTestCases :: Int -> IO Int
getNumTestCases defaultTestCases = do
    -- Read the environment variable or command line
    testCasesStr <- SystemEnvironment.lookupEnv "QUICKCHECK_TESTS"
    args <- SystemEnvironment.getArgs
    return $
        case parseQuickCheckTests args of
            Just n  -> n
            Nothing -> DataMaybe.fromMaybe defaultTestCases $ testCasesStr >>= TextRead.readMaybe

parseQuickCheckTests :: [String] -> Maybe Int
parseQuickCheckTests [] = Nothing
parseQuickCheckTests (arg:rest)
    | "--quickcheck-tests=" `DataList.isPrefixOf` arg = TextRead.readMaybe $ drop 19 arg
    | arg == "--quickcheck-tests" = TextRead.readMaybe =<< DataMaybe.listToMaybe rest
    | otherwise = parseQuickCheckTests rest

--------------------------------------------------------------------------------

badCase :: forall a. P.String -> LedgerApiV2.Value ->  PlutusSimpleModel.Run a -> Tasty.TestTree
badCase msg initialValueAdminUser = goodCase msg initialValueAdminUser . PlutusSimpleModel.mustFail

goodCase :: forall a. P.String -> LedgerApiV2.Value ->  PlutusSimpleModel.Run a -> Tasty.TestTree
goodCase msg initialValueAdminUser = PlutusSimpleModel.testNoErrorsTrace initialValueAdminUser  PlutusSimpleModel.defaultBabbage msg

--------------------------------------------------------------------------------

policyPSM :: LedgerApiV2.MintingPolicy ->  PlutusSimpleModel.TypedPolicy redeemer
policyPSM policy = PlutusSimpleModel.TypedPolicy $ PlutusSimpleModel.toV2  policy

validatorPSM ::LedgerApiV2.Validator ->  PlutusSimpleModel.TypedValidator datum redeemer
validatorPSM validator  = PlutusSimpleModel.TypedValidator $ PlutusSimpleModel.toV2  validator

--------------------------------------------------------------------------------

-- Adjusts the value by removing the current value and adding the new value
changeValue_AC_And_Amount :: LedgerValue.Value -> LedgerValue.AssetClass -> LedgerValue.AssetClass -> Integer -> LedgerValue.Value
changeValue_AC_And_Amount valueToChange acToChange newAC newAmount =
    let currentValue = LedgerValue.assetClassValueOf valueToChange acToChange
        valueWithout = valueToChange <> Ptx.negate (LedgerValue.assetClassValue acToChange currentValue)
    in valueWithout <> LedgerValue.assetClassValue newAC newAmount

--------------------------------------------------------------------------------

-- Adjust the value only changing the amount
changeValue_Amount :: LedgerValue.Value -> LedgerValue.AssetClass -> Integer -> LedgerValue.Value
changeValue_Amount valueToChange acToChange newAmount =
    let currentValue = LedgerValue.assetClassValueOf valueToChange acToChange
        valueWithout = valueToChange <> Ptx.negate (LedgerValue.assetClassValue acToChange currentValue)
    in valueWithout <> LedgerValue.assetClassValue acToChange newAmount

-----------------------------------------------------------------

fakeDatumEmpty :: LedgerApiV2.Datum
fakeDatumEmpty = LedgerApiV2.Datum $ LedgerApiV2.toBuiltinData (11::Integer)

fakeRedeemerEmpty :: LedgerApiV2.Redeemer
fakeRedeemerEmpty = LedgerApiV2.Redeemer $ LedgerApiV2.toBuiltinData (11::Integer)

-----------------------------------------------------------------

minAdaScriptDatum :: Integer
minAdaScriptDatum = 20_000_000

----------------------------------------------------------------------------------------

uTxOForValidatorAsReference :: TestParams -> LedgerApiV2.Validator -> LedgerApiV2.TxOut
uTxOForValidatorAsReference tp validator =
    let
        scriptHash = LedgerApiV2.getScriptHash $ OffChainHelpers.hashScriptValidator validator
    in LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpScriptValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaScriptDatum
                <> LedgerApiV2.singleton (tpScriptPolicyID_CS tp) (LedgerValue.TokenName scriptHash) 1
            )
            LedgerApiV2.NoOutputDatum
            (Just $ LedgerApiV2.ScriptHash scriptHash)


uTxOForMintingAsReference :: TestParams -> LedgerApiV2.MintingPolicy -> LedgerApiV2.TxOut
uTxOForMintingAsReference tp policy =
    let
        scriptHash = LedgerApiV2.getScriptHash $ OffChainHelpers.hashScriptMinting policy
    in LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpScriptValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaScriptDatum
                <> LedgerApiV2.singleton (tpScriptPolicyID_CS tp) (LedgerValue.TokenName scriptHash) 1
            )
            LedgerApiV2.NoOutputDatum
            (Just $ LedgerApiV2.ScriptHash scriptHash)

----------------------------------------------------------------------------------------
