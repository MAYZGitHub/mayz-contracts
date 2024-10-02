--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Automatic.ContextGenerator where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Data.List                     as DataList
import           Prelude                       as P hiding ((<>))

-- IOG imports

import qualified Ledger.Value                  as LedgerValue
import qualified Plutus.V2.Ledger.Api          as LedgerApiV2

-- Project imports

import qualified Generic.OffChainHelpers       as OffChainHelpers
import qualified Generic.OnChainHelpers        as OnChainHelpers
import qualified Generic.Types                 as T
import           TestUtils.Automatic.Helpers
import           TestUtils.Automatic.Types
import           TestUtils.Helpers
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

----------------------------------------------------------------------------------------


-- Genera un contexto a partir de las especificaciones de una transaccion y el caso de testeo que se va a ejecutar
context_Gen :: TestParams -> TxSpecs -> TestCaseParams -> LedgerApiV2.ScriptContext
context_Gen _ txSpecs tc =
    -- DebugTrace.trace ("Entering context_Gen with txSpecs: " ++ show txSpecs ++ " and tc: " ++ show tc) $
    let
        -----------------
        inputRefs = concat $ safeZipWith "inputRefs" (\(_, genFunc) options ->
            let
                txOutOptions = case options of
                    InputRefValid            -> TxOutValid
                    InputRefInvalid options'-> TxOutInvalid options'
                result = genFunc txOutOptions (tcExtras tc)
            in
                -- DebugTrace.trace ("Generating inputRefs with options: " ++ show options) $
                --     DebugTrace.trace ("Result: " ++ show result) $
                    result
            ) (tsInputsRef txSpecs) (tcInputsRef tc)
        -----------------
        inputsRefScripts = tsInputsRefScripts txSpecs
        -----------------
        inputs = concat $ safeZipWith "inputs" (\(_, genFunc, _) options ->
            -- DebugTrace.trace ("Generating inputs with options: " ++ show options) $
            genFunc options (tcExtras tc)
            ) (tsInputs txSpecs) (tcInputs tc)
        -----------------
        inputsFromWallets = tsInputsFromWallet txSpecs
        -----------------
        outputs = concat $ safeZipWith "outputs" (\(_, genFunc) options ->
            let
                txOutOptions = case options of
                    OutputValid            -> TxOutValid
                    OutputInvalid options'-> TxOutInvalid options'
            in
                -- DebugTrace.trace ("Generating outputs with options: " ++ show options) $
                genFunc txOutOptions (tcExtras tc)
            ) (tsOutputs txSpecs) (tcOutputs tc)
        -----------------
        mintValuesAndRedeemers = concat $ safeZipWith "mints" (\(_, genFunc, _) options ->
            -- DebugTrace.trace ("Generating mints with options: " ++ show options) $
            genFunc options (tcExtras tc)
            ) (tsMints txSpecs) (tcMints tc)
        -----------------
        signature = case tsUseSignatures txSpecs of
            Just gen ->
                -- DebugTrace.trace "Generating signatures" $
                gen (tcSignatures tc) (tcExtras tc)
            Nothing  ->
                -- DebugTrace.trace "No signatures generated"
                []
        -----------------
        validityRange = case tsUseValidityRange txSpecs of
            Just gen ->
                -- DebugTrace.trace "Generating validity range" $
                gen (tcValidityRange tc) (tcExtras tc)
            Nothing  ->
                -- DebugTrace.trace "Using default validity range"
                LedgerApiV2.always
        -----------------
    in
    -- DebugTrace.trace "Building final context" $
    -- DebugTrace.trace ("inputRefs: " ++ show inputRefs)  $
    mkContext
        |> setInputsRef (inputRefs ++ inputsRefScripts)
        |> setInputsAndAddRedeemers inputs
        |> addInputsWithTxOutRef inputsFromWallets
        |> setOutputs outputs
        |> setMintAndAddRedeemers mintValuesAndRedeemers
        |> setSignatories signature
        |> setValidyRange validityRange

----------------------------------------------------------------------------------------

txOut_Gen :: LedgerApiV2.TxOut
                         -> LedgerApiV2.CurrencySymbol
                         -> LedgerApiV2.TokenName
                        -> Maybe LedgerApiV2.Datum
                        -> Maybe LedgerApiV2.Datum
                        -> Maybe LedgerApiV2.Datum
                         -> TxOutOptions
                         -> [LedgerApiV2.TxOut]
txOut_Gen valid_UTxO tokenID_CS tokenID_TN invalidDatumData invalidDatumType invalidDatumNonExist options =
    let
        ------------------
        validValue = LedgerApiV2.txOutValue valid_UTxO
        adaAmount = OnChainHelpers.getADAfromValue validValue
        tokenID_AC = LedgerValue.AssetClass (tokenID_CS, tokenID_TN)
        tokenIDAmount = OnChainHelpers.getAmt_With_AC_InValue validValue tokenID_AC
        fakeID_CS = "aa"
        fakeID_TN = "aa"
        fakeAC_CS = LedgerValue.AssetClass (fakeID_CS, tokenID_TN)
        fakeAC_TN = LedgerValue.AssetClass (tokenID_CS, fakeID_TN)
        fakeAddress = OffChainHelpers.addressValidator "aa"
        ------------------
    in
        case options of
            TxOutValid ->
                [valid_UTxO]
            TxOutInvalid invalidOptions ->
                case invalidOptions of
                    TxOutInvalidNone ->
                        []
                    TxOutInvalidMore ->
                        [valid_UTxO, valid_UTxO]
                    TxOutInvalidEntity entityOptions ->
                        case entityOptions of
                            TxOutInvalidEntityValue invalidValueOptions ->
                                case invalidValueOptions of
                                    -- Change only the CurrencySymbol, keep TokenName and amount the same
                                    TxOutInvalidEntityValueID InvalidTokenCS ->
                                        let wrongUTxO =
                                                valid_UTxO
                                                    {
                                                        LedgerApiV2.txOutValue = changeValue_AC_And_Amount validValue tokenID_AC fakeAC_CS tokenIDAmount
                                                    }
                                        in [wrongUTxO]
                                    -- Change only the TokenName, keep CurrencySymbol and amount the same
                                    TxOutInvalidEntityValueID InvalidTokenTN ->
                                        let wrongUTxO =
                                                valid_UTxO
                                                    {
                                                        LedgerApiV2.txOutValue = changeValue_AC_And_Amount validValue tokenID_AC fakeAC_TN tokenIDAmount
                                                    }
                                        in [wrongUTxO]
                                    -- Change only the amount, keep CurrencySymbol and TokenName the same
                                    TxOutInvalidEntityValueID (InvalidTokenAmount amountOption) ->
                                        let adjustedTokenIDAmount = case amountOption of
                                                                InvalidValueZero -> 0
                                                                InvalidValueLess -> tokenIDAmount - 1
                                                                InvalidValueMore -> tokenIDAmount + 1
                                            wrongUTxO =
                                                valid_UTxO
                                                    {
                                                        LedgerApiV2.txOutValue = changeValue_Amount validValue tokenID_AC adjustedTokenIDAmount
                                                    }
                                        in [wrongUTxO]
                                    -- Change the ADA amount
                                    TxOutInvalidEntityValueADA invalidValueADAOptions ->
                                        let adjustedADAAmount = case invalidValueADAOptions of
                                                            InvalidValueZero -> 0
                                                            InvalidValueLess -> adaAmount - 1
                                                            InvalidValueMore -> adaAmount + 1
                                            wrongUTxO =
                                                valid_UTxO
                                                    {
                                                        LedgerApiV2.txOutValue = changeValue_Amount validValue OnChainHelpers.adaAssetClass adjustedADAAmount
                                                    }
                                        in [wrongUTxO]
                                    -- Iterate through multiple tokens, applying adjustments based on options
                                    TxOutInvalidEntityValueOtherTokens tokenOptionsList ->
                                        let adjustForToken accValue (TokenTestOptions cs tn invalidOpt) =
                                                let ac = LedgerValue.AssetClass (cs, tn)
                                                    tokenAmount = OnChainHelpers.getAmt_With_AC_InValue accValue ac  -- Use the accumulated value
                                                    -- Adjust for InvalidTokenCS and InvalidTokenTN
                                                    adjustedValue = case invalidOpt of
                                                                        InvalidTokenCS ->
                                                                            let fakeAC_CS' = LedgerValue.AssetClass (fakeID_CS, tn)
                                                                            in changeValue_AC_And_Amount accValue ac fakeAC_CS' tokenAmount
                                                                        InvalidTokenTN ->
                                                                            let fakeAC_TN' = LedgerValue.AssetClass (cs, fakeID_TN)
                                                                            in changeValue_AC_And_Amount accValue ac fakeAC_TN' tokenAmount
                                                                        InvalidTokenAmount amountOpt ->
                                                                            let adjustedAmount = case amountOpt of
                                                                                                    InvalidValueZero -> 0
                                                                                                    InvalidValueLess -> tokenAmount - 1
                                                                                                    InvalidValueMore -> tokenAmount + 1
                                                                            in changeValue_Amount accValue ac adjustedAmount
                                                in adjustedValue
                                            wrongUTxO = valid_UTxO { LedgerApiV2.txOutValue = DataList.foldl' adjustForToken validValue tokenOptionsList }
                                        in [wrongUTxO]
                            -- Alter the Datum
                            TxOutInvalidEntityDatum inputInvalidDatumOptions ->
                                let fakeDatum =
                                        case inputInvalidDatumOptions of
                                            InvalidEntityDatumData ->
                                                case invalidDatumData of
                                                        Just val -> val
                                                        Nothing  -> error "Error: InvalidDatumData is Nothing"
                                            InvalidEntityDatumNonExist ->
                                                case invalidDatumNonExist of
                                                        Just val -> val
                                                        Nothing  -> error "Error: InvalidDatumNonExist is Nothing"
                                            InvalidEntityDatumType ->
                                                case invalidDatumType of
                                                        Just val -> val
                                                        Nothing  -> error "Error: InvalidDatumType is Nothing"
                                    wrongUTxO =
                                        valid_UTxO
                                            {
                                                LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum fakeDatum
                                            }
                                in [wrongUTxO]
                            -- Alter the Address
                            TxOutInvalidEntityAddress ->
                                let wrongUTxO =
                                        valid_UTxO
                                            {
                                                LedgerApiV2.txOutAddress = fakeAddress
                                            }
                                in [wrongUTxO]

----------------------------------------------------------------------------------------

consume_TxOut_Gen ::
               TxOutGenerator
               -> LedgerApiV2.Redeemer
               -> Maybe LedgerApiV2.Redeemer
               -> Maybe LedgerApiV2.Redeemer
               -> Maybe LedgerApiV2.Redeemer
               -> InputOptions
               -> [(String, Bool)]
               -> [(LedgerApiV2.TxOut, LedgerApiV2.Redeemer)]
consume_TxOut_Gen txOut_Gen' validRedeemer invalidRedeemerData invalidRedeemerType invalidRedeemerNonExist inputOptions extras =
    let
        ---------------------
        txOuts = txOut_Gen' (convertInputOptionsToTxOutOptions inputOptions) extras
        ---------------------
        generateConsume txOuts' inputOptions' =
            case inputOptions' of
                InputValid ->
                    [(txOut, validRedeemer) | txOut <- txOuts']
                InputInvalid inputInvalidOptions ->
                    case inputInvalidOptions of
                        InputInvalidRedeemer inputInvalidRedeemerOptions ->
                            case inputInvalidRedeemerOptions of
                                InvalidRedeemerData ->
                                    [(txOut, invalidRedeemerDataValue) | txOut <- txOuts']
                                    where
                                        invalidRedeemerDataValue = case invalidRedeemerData of
                                            Just val -> val
                                            Nothing  -> error "Error: InvalidRedeemerData is Nothing"
                                InvalidRedeemerNonExist ->
                                    [(txOut, invalidRedeemerNonExistValue) | txOut <- txOuts']
                                    where
                                        invalidRedeemerNonExistValue = case invalidRedeemerNonExist of
                                            Just val -> val
                                            Nothing  -> error "Error: InvalidRedeemerNonExist is Nothing"
                                InvalidRedeemerType ->
                                    [(txOut, invalidRedeemerTypeValue) | txOut <- txOuts']
                                    where
                                        invalidRedeemerTypeValue = case invalidRedeemerType of
                                            Just val -> val
                                            Nothing  -> error "Error: InvalidRedeemerType is Nothing"

                        InputInvalidTxOut _ -> [(txOut, validRedeemer) | txOut <- txOuts']

    in
        generateConsume txOuts inputOptions

----------------------------------------------------------------------------------------

mint_Value_Gen :: LedgerApiV2.CurrencySymbol
               -> LedgerApiV2.TokenName
               -> Integer
               -> LedgerApiV2.Redeemer
               -> Maybe LedgerApiV2.Redeemer
               -> Maybe LedgerApiV2.Redeemer
               -> Maybe LedgerApiV2.Redeemer
               -> MintOptions
               -> [(LedgerApiV2.Value, LedgerApiV2.Redeemer)]
mint_Value_Gen tokenID_CS tokenID_TN amount validRedeemer invalidRedeemerData invalidRedeemerType invalidRedeemerNonExist options =
    let
        -- Define the valid minting value
        validValue = LedgerApiV2.singleton tokenID_CS tokenID_TN amount

        -- Adjust the value and redeemer based on the MintOptions
        generateMint :: MintOptions -> (LedgerApiV2.Value, LedgerApiV2.Redeemer)
        generateMint MintValid = (validValue, validRedeemer)

        generateMint (MintInvalid (MintInvalidRedeemer redeemerOption)) =
            case redeemerOption of
                InvalidRedeemerData     -> (validValue, safeGetRedeemer invalidRedeemerData "InvalidRedeemerData")
                InvalidRedeemerNonExist -> (validValue, safeGetRedeemer invalidRedeemerNonExist "InvalidRedeemerNonExist")
                InvalidRedeemerType     -> (validValue, safeGetRedeemer invalidRedeemerType "InvalidRedeemerType")
            where
                safeGetRedeemer :: Maybe a -> String -> a
                safeGetRedeemer maybeVal errorMsg =
                    case maybeVal of
                        Just val -> val
                        Nothing  -> error $ "Error: " ++ errorMsg ++ " is Nothing"

        generateMint (MintInvalid (MintInvalidValue invalidValueOption)) =
            case invalidValueOption of
                InvalidTokenCS ->
                    let fakeValue = LedgerApiV2.singleton (LedgerApiV2.CurrencySymbol "fakeCS") tokenID_TN amount
                    in (fakeValue, validRedeemer)
                InvalidTokenTN ->
                    let fakeValue = LedgerApiV2.singleton tokenID_CS (LedgerApiV2.TokenName "fakeTN") amount
                    in (fakeValue, validRedeemer)
                InvalidTokenAmount amountOption ->
                    let adjustedAmount = case amountOption of
                                            InvalidValueZero -> 0
                                            InvalidValueLess -> if amount < 0 then amount + 1 else amount - 1
                                            InvalidValueMore -> if amount < 0 then amount - 1 else amount + 1
                        adjustedValue = LedgerApiV2.singleton tokenID_CS tokenID_TN adjustedAmount
                    in (adjustedValue, validRedeemer)

    in
        [generateMint options]

----------------------------------------------------------------------------------------

signatures_gen :: TestParams ->  [T.WalletPaymentPKH] -> SignaturesOptions -> [T.WalletPaymentPKH]
signatures_gen _ pkhs op = case op of
       SignatureValid -> pkhs
       SignatureNone  ->  []
       SignatureOther -> ["aa"]

--------------------------------------------------------------------------------

validityRange_gen :: TestParams -> LedgerApiV2.POSIXTime -> ValidityRangeOptions -> LedgerApiV2.POSIXTimeRange
validityRange_gen _ date op = case op of
       RangeValid   -> createValidRange date
       RangeInvalid -> createInValidRange date
       RangeNone    -> LedgerApiV2.always

--------------------------------------------------------------------------------
