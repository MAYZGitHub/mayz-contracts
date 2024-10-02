{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module TestUtils.Automatic.Types where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Data.Csv             as Csv
import qualified Data.Data            as Data
import qualified Data.Map             as DataMap
import qualified Data.Semigroup       as Semigroup (Semigroup (..))
import           Prelude              as P
import qualified Test.QuickCheck      as QC

-- IOG imports

import qualified Plutus.V2.Ledger.Api as LedgerApiV2

-- Project imports

import qualified Generic.Types        as T
import           TestUtils.Types
import           TestUtils.TypesMAYZ

-----------------------------------------------------------------

type TxOutGenerator = TxOutOptions -> [(String, Bool)] -> [LedgerApiV2.TxOut]
type TxOutAndRedeemerGenerator = InputOptions -> [(String, Bool)] -> [(LedgerApiV2.TxOut, LedgerApiV2.Redeemer)]
type MintValueAndRedeemerGenerator = MintOptions -> [(String, Bool)] -> [(LedgerApiV2.Value, LedgerApiV2.Redeemer)]
type SignaturesGenerator = SignaturesOptions -> [(String, Bool)] -> [T.WalletPaymentPKH]
type ValidityRangeGenerator = ValidityRangeOptions -> [(String, Bool)] -> LedgerApiV2.POSIXTimeRange

data TxSpecs
    = TxSpecs
          { tsInputsRef        :: [(TestEntity, TxOutGenerator)]
          , tsInputsRefScripts :: [LedgerApiV2.TxOut]
          , tsInputs           :: [(TestEntity, TxOutAndRedeemerGenerator, ValidatorTestRedeemer)]
          , tsInputsFromWallet :: [(LedgerApiV2.TxOut, LedgerApiV2.TxOutRef)]
          , tsOutputs          :: [(TestEntity, TxOutGenerator)]
          , tsMints            :: [(TestToken, MintValueAndRedeemerGenerator, PolicyTestRedeemer)]
          , tsUseSignatures    :: Maybe SignaturesGenerator
          , tsUseValidityRange :: Maybe ValidityRangeGenerator
          , tsExtras           :: [(String, Bool)]
              -- name of the test and if is property test
          }

instance Show TxSpecs where
    show txSpec =   "TxSpecs {"
                -- ++  "tsInputsRef = " ++ show (tsInputsRef txSpec) ++ ", "
                -- ++  "tsInputs = " ++ show (tsInputs txSpec) ++ ", "
                ++  "tsInputsFromWallet = " ++ show (tsInputsFromWallet txSpec) ++ ", "
                -- ++  "tsOutputs = " ++ show (tsOutputs txSpec) ++ ", "
                -- ++  "tsMints = " ++ show (tsMints txSpec) ++ ", "
                -- ++  "tsUseSignatures = " ++ show (tsUseSignatures txSpec) ++ ", "
                -- ++  "tsUseValidityRange = " ++ show (tsUseValidityRange txSpec) ++ ", "
                ++  "tsExtras = " ++ show (tsExtras txSpec) ++ "}"

--------------------------------------------

type TxSpecsParametrizable = [TxParam] -> TxSpecs

--------------------------------------------

-- Test configuration in base to TestEntity
newtype TxSpecsEntityTestConfig
    = TxSpecsEntityTestConfig { testInvalidValueOtherTokens :: [InvalidValueOtherToken] }
    deriving (P.Show)

data InvalidValueOtherToken
    = InvalidValueOtherToken
          { testTokenName :: String
            -- The name of the token (for dynamic test case display)
          , testTokenCS   :: T.CS
            -- Currency Symbol of the token
          , testTokenTN   :: T.TN
              -- Token Name of the token
          }
    deriving (P.Show)

---------------------------------------------------------------

-- | TxParamGenerator is an existential type that represents a parameter in a transaction.
-- It encapsulates the generation logic, default value, and name of the parameter.
data TxParamGenerator where
    -- | Constructor for TxParamGenerator
    --
    -- @param (DataMap.Map String TxParam -> QC.Gen a)
    --        A function that generates a value of type 'a' based on previously generated parameters.
    --        This allows for creating dependencies between parameters.
    --
    -- @param a
    --        The default value for this parameter. Used when a specific value is not generated.
    --
    -- @param String
    --        The name of the parameter. Used for identifying the parameter in the generation process.
    --
    -- The use of existential types (via GADT syntax) allows for parameters of different types
    -- to be stored in the same list, as long as they are instances of Typeable and Show.
    TxParamGenerator :: (Data.Typeable a, Show a) => (DataMap.Map String TxParam -> QC.Gen a) -> String -> TxParamGenerator

-- Wrapper for TxParamGenerators
newtype TxParamGenerators
    = TxParamGenerators [TxParamGenerator]

type TxParamGeneratorsList = [(String, TxParamGenerators)]

-- Define a Semigroup instance for TxParamGenerators
instance Semigroup.Semigroup TxParamGenerators where
    (TxParamGenerators a) <> (TxParamGenerators b) = TxParamGenerators (a ++ b)

instance Monoid TxParamGenerators where
    mempty = TxParamGenerators []
    mappend = (Semigroup.<>)

-- | Helper function to create a TxParamGenerator (optional, but can make code more readable)
txParamDetail :: (Data.Typeable a, Show a) =>
    (DataMap.Map String TxParam -> QC.Gen a) ->
    String ->
    TxParamGenerator
txParamDetail = TxParamGenerator

---------------------------------------------------------------

-- Type for building all different test cases
data TestCaseParams
    = TestCaseParams
          { tcInputsRef     :: [InputRefOptions]
          , tcInputs        :: [InputOptions]
          , tcOutputs       :: [OutputOptions]
          , tcMints         :: [MintOptions]
          , tcValidityRange :: ValidityRangeOptions
          , tcSignatures    :: SignaturesOptions
          , tcExtras        :: [(String, Bool)]
          }
    deriving (P.Show)

data InputRefOptions
    = InputRefValid
    | InputRefInvalid TxOutInvalidOptions
    deriving (P.Show)
data InputOptions
    = InputValid
    | InputInvalid InputInvalidOptions
    deriving (P.Show)
data OutputOptions
    = OutputValid
    | OutputInvalid TxOutInvalidOptions
    deriving (P.Show)
data MintOptions
    = MintValid
    | MintInvalid MintInvalidOptions
    deriving (P.Show)
data ValidityRangeOptions = RangeValid | RangeNone | RangeInvalid deriving (P.Show)
data SignaturesOptions = SignatureValid | SignatureNone | SignatureOther deriving (P.Show)

data TxOutOptions
    = TxOutValid
    | TxOutInvalid TxOutInvalidOptions
    deriving (P.Show)
data TxOutInvalidOptions
    = TxOutInvalidNone
    | TxOutInvalidMore
    | TxOutInvalidEntity TxOutInvalidEntityOptions
    deriving (P.Show)
data TxOutInvalidEntityOptions
    = TxOutInvalidEntityValue InvalidValueOptions
    | TxOutInvalidEntityDatum InvalidEntityDatumOptions
    | TxOutInvalidEntityAddress
    deriving (P.Show)
data InvalidValueOptions
    = TxOutInvalidEntityValueID InvalidTokenOptions
    | TxOutInvalidEntityValueADA InvalidTokenAmountOptions
    | TxOutInvalidEntityValueOtherTokens [TokenTestOptions]
    deriving (P.Show)
data InvalidTokenOptions
    = InvalidTokenCS
    | InvalidTokenTN
    | InvalidTokenAmount InvalidTokenAmountOptions
    deriving (P.Show)
data InvalidTokenAmountOptions = InvalidValueZero | InvalidValueLess | InvalidValueMore deriving (P.Show)

data TokenTestOptions
    = TokenTestOptions
          { tcTokenCS             :: T.CS
            -- Currency Symbol of the token
          , tcTokenTN             :: T.TN
            -- Token Name of the token
          , tcTokenInvalidOptions :: InvalidTokenOptions
              -- Invalid options for the token
          }
    deriving (P.Show)

data InputInvalidOptions
    = InputInvalidRedeemer InvalidRedeemerOptions
    | InputInvalidTxOut TxOutInvalidOptions
    deriving (P.Show)

data MintInvalidOptions
    = MintInvalidRedeemer InvalidRedeemerOptions
    | MintInvalidValue InvalidTokenOptions
    deriving (P.Show)

data InvalidEntityDatumOptions = InvalidEntityDatumData | InvalidEntityDatumNonExist | InvalidEntityDatumType deriving (P.Show)

data InvalidRedeemerOptions = InvalidRedeemerData | InvalidRedeemerNonExist | InvalidRedeemerType deriving (P.Show)

-----------------------------------------------------------------

-- Core configuration type for each test case
data TestConfig
    = TestConfig
          { tcExpectedOutcome :: TestOutcome
          , tcErrorSource     :: ErrorSource
          }
    deriving (P.Show)

data TestOutcome
    = TestNone
    | TestSuccess
    | TestFailure [String] -- List of possible error messages
    deriving (P.Show)

data ErrorSource
    = NoError
    | SelfRedeemer
    | OtherRedeemer (Maybe RedeemerIdentifier)
    | CombinedRedeemerResults
    deriving (P.Show)

data RedeemerIdentifier
    = RedeemerIdentifierValidator ValidatorTestRedeemer
    | RedeemerIdentifierPolicy PolicyTestRedeemer
    deriving (P.Show)

data ConfigTestTree
    = TestConfigLeaf TestConfig
    | TestConfigBranch (DataMap.Map String ConfigTestTree)
    deriving (P.Show)

-- Main configuration type mirroring TestCaseParams
data RedeemerTestConfigTree
    = RedeemerTestConfigTree
          { tctInputsRef   :: [ConfigTestTree]
          , tctInputs      :: [ConfigTestTree]
          , tctOutputs     :: [ConfigTestTree]
          , tctMints       :: [ConfigTestTree]
          , tctValidyRange :: ConfigTestTree
          , tctSignatures  :: ConfigTestTree
          , tctExtras      :: ConfigTestTree
          }
    deriving (P.Show)

-----------------------------------------------------------------

-- Types for reading rules from excel

-- Define CSVRow
data CSVRow
    = CSVRow
          { csvUse             :: String
          , csvTx              :: String
          , csvScript          :: String
          , csvRedeemer        :: String
          , csvGroup           :: String
          , csvPath            :: String
          , csvExpectedOutcome :: String
          , csvErrorMessage    :: String
          , csvErrorSource     :: String
          }
    deriving (Show)

-- CSV Parsing
instance Csv.FromNamedRecord CSVRow where
    parseNamedRecord r = CSVRow
        <$> r Csv..: "Use"
        <*> r Csv..: "Tx"
        <*> r Csv..: "Script"
        <*> r Csv..: "Redeemer"
        <*> r Csv..: "Group"
        <*> r Csv..: "Path"
        <*> r Csv..: "ExpectedOutcome"
        <*> r Csv..: "ErrorMessage"
        <*> r Csv..: "ErrorSource"

data CSVRule
    = CSVRule
          { ruleExpectedOutcome :: TestOutcome
          , ruleErrorSource     :: ErrorSource
          , ruleScore           :: MatchScore
          }
    deriving (Show)


data MatchType = Direct | Partial | Any deriving (Eq, Ord, Show)

data MatchScore
    = MatchScore
          { msLevelsMatched :: Int
          , msDirectFirst4  :: Int
          , msPartialFirst4 :: Int
          , msAnyFirst4     :: Int
          , msDirectRest    :: Int
          , msPartialRest   :: Int
          , msAnyRest       :: Int
          }
    deriving (Eq, Show)

data RuleTree
    = RuleNode (DataMap.Map String RuleTree) (Maybe CSVRule)
    deriving (Show)

-----------------------------------------------------------------

