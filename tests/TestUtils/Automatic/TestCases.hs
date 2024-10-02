--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Automatic.TestCases where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Data.Maybe                as DataMaybe
import           Prelude                   as P hiding ((<>))

-- IOG imports

-- Project imports
import           TestUtils.Automatic.Types

----------------------------------------------------------------------------------------

generateTestCaseParams :: TxSpecs -> TestCaseParams
generateTestCaseParams txSpecs =
    TestCaseParams {
        tcInputsRef = replicate (P.length $ tsInputsRef txSpecs) InputRefValid
        , tcInputs = replicate (P.length $ tsInputs txSpecs) InputValid
        , tcOutputs = replicate (P.length $ tsOutputs txSpecs) OutputValid
        , tcMints = replicate (P.length $ tsMints txSpecs) MintValid
        , tcValidityRange = if DataMaybe.isJust $ tsUseValidityRange txSpecs then RangeValid else RangeNone
        , tcSignatures = if DataMaybe.isJust $ tsUseSignatures txSpecs then SignatureValid else SignatureNone
        , tcExtras = map (\(key, _) -> (key, False)) (tsExtras txSpecs)
    }

--------------------------------------------------------------------------------

setTcInputsRef :: [InputRefOptions] -> TestCaseParams -> TestCaseParams
setTcInputsRef newInputsRef tc = tc { tcInputsRef = newInputsRef }

setTcInputRefAtIndex :: Integer -> InputRefOptions -> TestCaseParams -> TestCaseParams
setTcInputRefAtIndex index newValue tc =
    let
        -- Safely update the element at the given index
        updatedInputsRef = P.take (P.fromIntegral index) (tcInputsRef tc)
                        ++ [newValue]
                        ++ P.drop (P.fromIntegral index P.+ 1) (tcInputsRef tc)
    in
        tc { tcInputsRef = updatedInputsRef }

setTcInputAtIndex :: Integer -> InputOptions -> TestCaseParams -> TestCaseParams
setTcInputAtIndex index newValue tc =
    let
        -- Safely update the element at the given index
        updatedInputs = P.take (P.fromIntegral index) (tcInputs tc)
                        ++ [newValue]
                        ++ P.drop (P.fromIntegral index P.+ 1) (tcInputs tc)
    in
        tc { tcInputs = updatedInputs }

setTcOutputAtIndex :: Integer -> OutputOptions -> TestCaseParams -> TestCaseParams
setTcOutputAtIndex index newValue tc =
    let
        -- Safely update the element at the given index
        updatedOutputs = P.take (P.fromIntegral index) (tcOutputs tc)
                        ++ [newValue]
                        ++ P.drop (P.fromIntegral index P.+ 1) (tcOutputs tc)
    in
        tc { tcOutputs = updatedOutputs }

setTcMintsAtIndex :: Integer -> MintOptions -> TestCaseParams -> TestCaseParams
setTcMintsAtIndex index newValue tc =
    let
        -- Safely update the element at the given index
        updatedMints = P.take (P.fromIntegral index) (tcMints tc)
                        ++ [newValue]
                        ++ P.drop (P.fromIntegral index P.+ 1) (tcMints tc)
    in
        tc { tcMints = updatedMints }

setTcSignatures :: SignaturesOptions -> TestCaseParams -> TestCaseParams
setTcSignatures newValue tc = tc { tcSignatures = newValue }

setTcValidityRange :: ValidityRangeOptions -> TestCaseParams -> TestCaseParams
setTcValidityRange newValue tc = tc { tcValidityRange = newValue }

setTcExtras :: [(String, Bool)] -> TestCaseParams -> TestCaseParams
setTcExtras extras tc = tc { tcExtras = extras }

--------------------------------------------------------------------------------
