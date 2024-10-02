{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
module TestUtils.TestContext.Evaluate where

------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Data.ByteString as BS
import qualified Data.List as DataList
import qualified Data.Maybe as DataMaybe
import qualified Data.Text as DataText
import qualified Text.Read as TextRead
import Prelude as P

-- IOG imports

import qualified Ledger.Value as LedgerValue
import qualified Plutus.V1.Ledger.ProtocolVersions as LedgerProtocolVersionsV1
import qualified Plutus.V1.Ledger.Scripts as LedgerScriptsV1
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusCore (defaultCostModelParams)
import qualified PlutusTx.AssocMap as TxAssocMap
import qualified PlutusTx.Builtins as PlutusTx

-- Project imports

import qualified Generic.OffChainHelpers as OffChainHelpers
import qualified Generic.OnChainHelpers as OnChainHelpers
import TestUtils.Constants
import TestUtils.Helpers
import TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

-- Define the testContext function to validate all redeemers using the needed scripts, validators or policies
testContext :: (LedgerApiV2.ScriptHash -> Maybe LedgerApiV2.Validator) -> (LedgerApiV2.ScriptHash -> Maybe LedgerApiV2.MintingPolicy) -> (Maybe LedgerApiV2.ScriptHash -> LedgerApiV2.Redeemer -> Maybe ValidatorTestRedeemer) -> (Maybe LedgerApiV2.ScriptHash -> LedgerApiV2.Redeemer -> Maybe PolicyTestRedeemer) -> LedgerApiV2.ScriptContext -> Bool -> IO [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Maybe Integer))]
testContext getValidator getPolicy getValidatorRedeemerTxSpec getPolicyRedeemerTxSpec ctx swTraceEvaluate = do
    -----------------
    traceIf swTraceEvaluate ">>>>>>>>"
    traceIf swTraceEvaluate ">> Testing Redeemers in Context ..."
    -----------------

    let -----------------
        txInfo = LedgerApiV2.scriptContextTxInfo ctx
        -- Extract all inputs and their corresponding TxOutRefs
        txInfoReferenceInputs = LedgerApiV2.txInfoReferenceInputs txInfo
    -----------------
    traceIf swTraceEvaluate $ ">> ctx: " ++ show ctx
    -----------------
    let txInfoInputs = LedgerApiV2.txInfoInputs txInfo
        txInfoOutputs = LedgerApiV2.txInfoOutputs txInfo
        -----------------
        inputsFromScripts =
            [ LedgerApiV2.txInInfoOutRef txInfoInput | txInfoInput <- txInfoInputs, let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                     in OnChainHelpers.isScriptAddress address
            ]
        -----------------
        -- Extract all CurrencySymbols in the minting value
        minting = LedgerApiV2.txInfoMint txInfo
        -----------------
        porpuseAndRedeemers = TxAssocMap.toList $ LedgerApiV2.txInfoRedeemers $ LedgerApiV2.scriptContextTxInfo ctx
        -----------------
        getMintingSymbols :: LedgerValue.Value -> [LedgerApiV2.CurrencySymbol]
        getMintingSymbols val =
            let flattened = OnChainHelpers.flattenValue val
                csList = map (\(cs, _, _) -> cs) flattened
             in DataList.nub csList
        -----------------
        mintingSymbols = getMintingSymbols minting
        -----------------
        checkMinADA :: [LedgerApiV2.TxOut] -> LedgerApiV2.LogOutput
        checkMinADA outputs =
            let checkOutput :: LedgerApiV2.TxOut -> LedgerApiV2.LogOutput
                checkOutput txOut =
                    let adaAmount = OnChainHelpers.getADAfromValue (LedgerApiV2.txOutValue txOut)
                        !value = LedgerApiV2.txOutValue txOut
                        !minADA = OnChainHelpers.calculateMinADAOfValue value True
                     in ---------------------
                        [DataText.pack $ "Output does not meet minimum ADA requirement: " ++ show adaAmount ++ " < " ++ show minADA | adaAmount < minADA]
             in concatMap checkOutput outputs
        -----------------
        minADALogOutput = checkMinADA txInfoOutputs
        -----------------
        -- Check if each input has a corresponding spending redeemer
        missingInputRedeemers :: [LedgerApiV2.TxOutRef]
        missingInputRedeemers =
            [ ref
            | ref <- inputsFromScripts
            , not
                ( any
                    ( \(purpose, _) -> case purpose of
                        LedgerApiV2.Spending ref' -> ref' == ref
                        _ -> False
                    )
                    porpuseAndRedeemers
                )
            ]
        -----------------
        -- Check if each minting CurrencySymbol has a corresponding minting redeemer
        missingMintingRedeemers :: [LedgerApiV2.CurrencySymbol]
        missingMintingRedeemers =
            [ cs
            | cs <- mintingSymbols
            , not
                ( any
                    ( \(purpose, _) -> case purpose of
                        LedgerApiV2.Minting cs' -> cs' == cs
                        _ -> False
                    )
                    porpuseAndRedeemers
                )
            ]
        -----------------
        -- Check if each redeemer references a valid input
        invalidSpendingRedeemers :: [(LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)] -> [LedgerApiV2.TxInInfo] -> [LedgerApiV2.TxOutRef]
        invalidSpendingRedeemers redeemers txInfoInputs' =
            [ ref
            | (LedgerApiV2.Spending ref, _) <- redeemers
            , ref `notElem` map LedgerApiV2.txInInfoOutRef txInfoInputs'
            ]

        -- Check if each minting redeemer references a valid minting currency symbol
        invalidMintingRedeemers :: [(LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer)] -> [LedgerApiV2.CurrencySymbol] -> [LedgerApiV2.CurrencySymbol]
        invalidMintingRedeemers redeemers mintingSymbols' =
            [ cs
            | (LedgerApiV2.Minting cs, _) <- redeemers
            , cs `notElem` mintingSymbols'
            ]

        -- Log messages for missing redeemers
        inputWithoutRedeemerLogs = [DataText.pack $ "Input consumed with no redeemer: " P.++ P.show ref | ref <- missingInputRedeemers] :: LedgerApiV2.LogOutput
        mintingWithoutRedeemerLogs = [DataText.pack $ "Minting with no redeemer: " P.++ P.show cs | cs <- missingMintingRedeemers] :: LedgerApiV2.LogOutput
        spendingRedeemerWithoutInputLogs = [DataText.pack $ "Spending redeemer without input: " P.++ P.show ref | ref <- invalidSpendingRedeemers porpuseAndRedeemers txInfoInputs] :: LedgerApiV2.LogOutput
        mintingRedeemerWithoutMintingLogs = [DataText.pack $ "Minting redeemer without minting: " P.++ P.show cs | cs <- invalidMintingRedeemers porpuseAndRedeemers mintingSymbols] :: LedgerApiV2.LogOutput
        -----------------
        -- Helper function to check budget and size limits and add errors to logs
        checkLimits ::
            P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget ->
            Maybe Integer ->
            LedgerApiV2.LogOutput ->
            Integer ->
            Integer ->
            Integer ->
            Bool ->
            Integer ->
            LedgerApiV2.LogOutput
        checkLimits evalErr sizeMaybe logOutput maxMem' maxCPU' maxSize' swErrorOnMemCPUSizeWhenNothing errorOnPercentageOver =
            let -------------
                -- Adjust the maximum values based on the percentage margin
                adjustedMaxMem = maxMem' + (maxMem' * errorOnPercentageOver `div` 100)
                adjustedMaxCPU = maxCPU' + (maxCPU' * errorOnPercentageOver `div` 100)
                adjustedMaxSize = maxSize' + (maxSize' * errorOnPercentageOver `div` 100)
                -------------
                (memIntMaybe, cpuIntMaybe) = case evalErr of
                    P.Left _ -> (Nothing, Nothing)
                    P.Right (LedgerApiV2.ExBudget (LedgerApiV2.ExCPU cpu) (LedgerApiV2.ExMemory mem)) -> do
                        let memIntMaybe' = TextRead.readMaybe (P.show mem) :: Maybe Integer
                        let cpuIntMaybe' = TextRead.readMaybe (P.show cpu) :: Maybe Integer
                        (memIntMaybe', cpuIntMaybe')
                -------------
                (memExceeded, cpuExceeded) =
                    case (memIntMaybe, cpuIntMaybe) of
                        (Just memInt, Just cpuInt) -> (memInt > adjustedMaxMem, cpuInt > adjustedMaxCPU)
                        _ -> (False, False)
                -------------
                sizeExceededCheck =
                    case sizeMaybe of
                        Just size -> size > adjustedMaxSize
                        Nothing -> False
                -------------
                -- Helper to calculate percentage
                percentageOverMax :: Integer -> Integer -> P.Double
                percentageOverMax current maxVal = (P.fromIntegral current P./ P.fromIntegral maxVal) P.* 100
                -------------
                memLog =
                    if DataMaybe.isNothing memIntMaybe
                        then "Memory usage data is unavailable."
                        else
                            "Memory usage exceeds limit. Current = " ++ maybe "N/A" P.show memIntMaybe ++ ", max = " ++ P.show maxMem'
                                ++ " ("
                                ++ maybe "N/A" (P.show . (`percentageOverMax` maxMem')) memIntMaybe
                                ++ "% over max)"
                -------------
                cpuLog =
                    if DataMaybe.isNothing cpuIntMaybe
                        then "CPU usage data is unavailable."
                        else
                            "CPU usage exceeds limit. Current = " ++ maybe "N/A" P.show cpuIntMaybe ++ ", max = " ++ P.show maxCPU'
                                ++ " ("
                                ++ maybe "N/A" (P.show . (`percentageOverMax` maxCPU')) cpuIntMaybe
                                ++ "% over max)"
                -------------
                sizeLog =
                    if DataMaybe.isNothing sizeMaybe
                        then "Size usage data is unavailable."
                        else
                            "Size usage exceeds limit. Current = " ++ maybe "N/A" P.show sizeMaybe ++ ", max = " ++ P.show maxSize'
                                ++ " ("
                                ++ maybe "N/A" (P.show . (`percentageOverMax` maxSize')) sizeMaybe
                                ++ "% over max)"
                -------------
                newLogOutput =
                    logOutput
                        ++ [DataText.pack memLog | memExceeded || (swErrorOnMemCPUSizeWhenNothing && DataMaybe.isNothing memIntMaybe)]
                        ++ [DataText.pack cpuLog | cpuExceeded || (swErrorOnMemCPUSizeWhenNothing && DataMaybe.isNothing cpuIntMaybe)]
                        ++ [DataText.pack sizeLog | sizeExceededCheck || (swErrorOnMemCPUSizeWhenNothing && DataMaybe.isNothing sizeMaybe)]
             in -------------
                newLogOutput
        -----------------
        -- si el redeemer usa un script, pero ese script hash esta en referencia en referencia inputs, no sumo size de ese script al sizeTotal
        -- si no, si sumo el size
        -- de todas formas, al no surmarlo, igual controlo su propio tamaño por si solo. Me interesa que nunca supere ningun script ese tamaño.
        -- el size final, de todas formas, nos es simplemente la suma de redeemers
        -- es la suma de los scripts de cada redeemer, que se hayan sumado por que no estaban por referencia,
        -- mas la suma del tamano del contexto, mas la suma de los scripts que estan en outputs para ser agregados por referencia
        -----------------
        findScriptHashInTxInfoReferenceInputs :: Maybe LedgerApiV2.ScriptHash -> [LedgerApiV2.TxInInfo] -> Bool
        findScriptHashInTxInfoReferenceInputs Nothing _ = False
        findScriptHashInTxInfoReferenceInputs (Just scriptHash) txInfoReferenceInputs' =
            let refScriptsHashesMaybe = LedgerApiV2.txOutReferenceScript . LedgerApiV2.txInInfoResolved <$> txInfoReferenceInputs'
                refScriptsHashes = DataMaybe.catMaybes refScriptsHashesMaybe -- Filter to get only the Just values
             in scriptHash `elem` refScriptsHashes
        -----------------
        testRedeemerFor :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer) -> IO (Maybe RedeemerLog, (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Maybe Integer))
        testRedeemerFor (LedgerApiV2.Spending txOutRef, redeemer) = do
            -----------------
            traceIf swTraceEvaluate $ ">> INIT - testRedeemerFor " ++ show (LedgerApiV2.Spending txOutRef, redeemer) ++ ":"
            -----------------
            (scriptHash, logOutput, exBudget, sizeMaybe) <- testRedeemer getValidator getPolicy ctx (LedgerApiV2.Spending txOutRef, redeemer) swTraceEvaluate
            -----------------
            let redeemerTxSpecs = getValidatorRedeemerTxSpec scriptHash redeemer
            -----------------
            realUsedSize <-
                if findScriptHashInTxInfoReferenceInputs scriptHash txInfoReferenceInputs
                    then do
                        -----------------
                        traceIf swTraceEvaluate $ ">> Results - testRedeemerFor: " ++ show (LedgerApiV2.Spending txOutRef, redeemer) ++ " - Using as reference input - size (not to be added to tx): " ++ show (DataMaybe.fromJust sizeMaybe) ++ " - exBudget: " ++ show exBudget
                        -----------------
                        return (Just 0)
                    else do
                        -----------------
                        traceIf swTraceEvaluate $ ">> Results - testRedeemerFor: " ++ show (LedgerApiV2.Spending txOutRef, redeemer) ++ " - Using as normal input - size: " ++ show (DataMaybe.fromJust sizeMaybe) ++ " - exBudget: " ++ show exBudget
                        -----------------
                        return sizeMaybe
            -----------------
            -- al margen de que el tamaño del script por referencia no lo sumo al resultado final, donde se calcula el tamaño de la transaccion
            -- igual el checkLimits lo uso con el tamaño del script, sizeMaybe, para asegurarme que ninguno supera el máximo de txSize
            -----------------
            let updatedLogOutput = checkLimits exBudget realUsedSize logOutput maxMemory maxCPU maxTxSize False showErrorOnPercentageOver
            -----------------
            traceIf swTraceEvaluate $ ">> LogOutput - testRedeemerFor: " ++ show (LedgerApiV2.Spending txOutRef, redeemer) ++ ": " ++ show updatedLogOutput
            -----------------
            return (Just (RedeemerLogValidator redeemerTxSpecs), (updatedLogOutput, exBudget, realUsedSize))
        -----------------
        testRedeemerFor (LedgerApiV2.Minting cs, redeemer) = do
            -----------------
            traceIf swTraceEvaluate $ ">> INIT - testRedeemerFor: " ++ show (LedgerApiV2.Minting cs, redeemer) ++ ":"
            -----------------
            (scriptHash, logOutput, exBudget, sizeMaybe) <- testRedeemer getValidator getPolicy ctx (LedgerApiV2.Minting cs, redeemer) swTraceEvaluate
            -----------------
            let redeemerTxSpecs = getPolicyRedeemerTxSpec scriptHash redeemer
            -----------------
            realUsedSize <-
                if findScriptHashInTxInfoReferenceInputs scriptHash txInfoReferenceInputs
                    then do
                        -----------------
                        traceIf swTraceEvaluate $ ">>  Results - testRedeemerFor: " ++ show (LedgerApiV2.Minting cs, redeemer) ++ " - Using as reference input - size (not to be added to tx): " ++ show (DataMaybe.fromJust sizeMaybe) ++ " - exBudget: " ++ show exBudget
                        return (Just 0)
                    else do
                        -----------------
                        traceIf swTraceEvaluate $ ">>  Results - testRedeemerFor: " ++ show (LedgerApiV2.Minting cs, redeemer) ++ " - Using as normal input - size: " ++ show (DataMaybe.fromJust sizeMaybe) ++ " - exBudget: " ++ show exBudget
                        -----------------
                        return sizeMaybe
            -----------------
            -- al margen de que el tamaño del script por referencia no lo sumo al resultado final, donde se calcula el tamaño de la transaccion
            -- igual el checkLimits lo uso con el tamaño del script, sizeMaybe, para asegurarme que ninguno supera el máximo de txSize
            -----------------
            let updatedLogOutput = checkLimits exBudget sizeMaybe logOutput maxMemory maxCPU maxTxSize False showErrorOnPercentageOver
            -----------------
            traceIf swTraceEvaluate $ ">> LogOutput - testRedeemerFor: " ++ show (LedgerApiV2.Minting cs, redeemer) ++ ": " ++ show updatedLogOutput
            -----------------
            return (Just (RedeemerLogPolicy redeemerTxSpecs), (updatedLogOutput, exBudget, realUsedSize))
        -----------------
        testRedeemerFor (_, _) = return (Nothing, (["UNKNOWN SCRIPT PURPOSE"], Left P.undefined, Nothing))
    -----------------
    traceIf swTraceEvaluate $ ">> porpuseAndRedeemers: " ++ show porpuseAndRedeemers
    -----------------
    results <- mapM testRedeemerFor porpuseAndRedeemers
    -----------------
    let combineLogs = foldMap (\(_, (logOutput, _, _)) -> logOutput) results
        -----------------
        combineExBudgets :: [Either a LedgerApiV2.ExBudget] -> Either a LedgerApiV2.ExBudget
        combineExBudgets exs =
            let lefts = [e | P.Left e <- exs]
                rights = [b | P.Right b <- exs]
                sumExBudget = foldr (\(LedgerApiV2.ExBudget mem cpu) (LedgerApiV2.ExBudget accMem accCpu) -> LedgerApiV2.ExBudget (accMem P.+ mem) (accCpu P.+ cpu)) (LedgerApiV2.ExBudget 0 0)
             in if not (null lefts)
                    then P.Left (head lefts)
                    else P.Right (sumExBudget rights)
        -----------------
        -- Function to calculate the size of the ScriptContext in bytes
        getScriptContextSize :: LedgerApiV2.ScriptContext -> IO Integer
        getScriptContextSize ctx' = do
            let builtinData = LedgerApiV2.toBuiltinData ctx' -- Convert ScriptContext to BuiltinData
                serializedData = PlutusTx.serialiseData builtinData -- Serialize BuiltinData to ByteString
                size = fromIntegral $ BS.length $ OffChainHelpers.builtinByteStringToHexString serializedData
            -----------------
            traceIf swTraceEvaluate $ ">> getScriptContextSize - size: " ++ show size
            -----------------
            return size
        -----------------
        getScriptAddedInTxOutputAsReferencesSize :: IO Integer
        getScriptAddedInTxOutputAsReferencesSize = do
            let scriptHashesInOutputs = DataMaybe.catMaybes $ LedgerApiV2.txOutReferenceScript <$> txInfoOutputs
                -- getAddress scriptHash'= LedgerApiV2.Address (LedgerApiV2.ScriptCredential $ LedgerApiV2.ValidatorHash $ LedgerApiV2.getScriptHash scriptHash') Nothing
                -- addresses = getAddress <$> scriptHashesInOutputs
                -----------------
                validators = DataMaybe.catMaybes $ getValidator <$> scriptHashesInOutputs
                policies = DataMaybe.catMaybes $ getPolicy <$> scriptHashesInOutputs
                -----------------
                validatorSize val =
                    let scriptUnValidatorV2 = OffChainHelpers.getScriptUnValidator val
                        size = LedgerScriptsV1.scriptSize scriptUnValidatorV2
                     in size
                mintingSize mp =
                    let scriptMintingPolicyV2 = OffChainHelpers.getScriptMintingPolicy mp
                        size = LedgerScriptsV1.scriptSize scriptMintingPolicyV2
                     in size
                -----------------
                valSizes = sum $ validatorSize <$> validators
                mpSizes = sum $ mintingSize <$> policies
            -----------------
            traceIf swTraceEvaluate $ ">> getScriptAdded - validators: " ++ show validators ++ " - sizes: " ++ show mpSizes ++ " - policies: " ++ show validators ++ " - sizes: " ++ show mpSizes
            -----------------
            return $ valSizes + mpSizes
    -----------------
    ctxSize <- getScriptContextSize ctx
    scriptSizes <- getScriptAddedInTxOutputAsReferencesSize
    -----------------
    let -----------------
        combineSizes :: [Maybe Integer] -> Maybe Integer
        combineSizes ints =
            if Nothing `elem` ints
                then Nothing
                else Just (sum (DataMaybe.catMaybes ints) + ctxSize + scriptSizes)
        -----------------
        combinedExBudget = combineExBudgets [ex | (_, (_, ex, _)) <- results]
        -----------------
        combinedSizes = combineSizes [size | (_, (_, _, size)) <- results]
        -----------------
        -- Check overall limits for the combined result and update the log
        combinedLogOutput = checkLimits combinedExBudget combinedSizes combineLogs maxMemory maxCPU maxTxSize True showErrorOnPercentageOver -- Adjust maxMem, maxCPU, and maxSize as needed
        -----------------
        -- Additional checks for missing redeemers
        noRedeemersFound = ["No redeemers Found" | null porpuseAndRedeemers] :: LedgerApiV2.LogOutput
        totalLogOutput = combinedLogOutput ++ noRedeemersFound ++ inputWithoutRedeemerLogs ++ mintingWithoutRedeemerLogs ++ spendingRedeemerWithoutInputLogs ++ mintingRedeemerWithoutMintingLogs ++ minADALogOutput
        -----------------
        finalResults = results ++ [(Nothing, (totalLogOutput, combinedExBudget, combinedSizes))]
    -----------------
    traceIf swTraceEvaluate $ ">> finalResults: " ++ show finalResults
    -----------------
    return finalResults

--------------------------------------------------------------------------------2

-- test a redeemer with validator or minting policy
testRedeemer :: (LedgerApiV2.ScriptHash -> Maybe LedgerApiV2.Validator) -> (LedgerApiV2.ScriptHash -> Maybe LedgerApiV2.MintingPolicy) -> LedgerApiV2.ScriptContext -> (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer) -> Bool -> IO (Maybe LedgerApiV2.ScriptHash, LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Maybe Integer)
testRedeemer getValidator _ ctx (LedgerApiV2.Spending txOutRef, redeemer) _ = do
    let findTxInInfo :: LedgerApiV2.TxOutRef -> [LedgerApiV2.TxInInfo] -> Maybe LedgerApiV2.TxInInfo
        findTxInInfo txOutRef' list =
            let filtered = filter (\txInInfo' -> LedgerApiV2.txInInfoOutRef txInInfo' == txOutRef') list
             in case filtered of
                    (x : _) -> Just x
                    [] -> Nothing
        !txInfoInputs = LedgerApiV2.txInfoInputs $ LedgerApiV2.scriptContextTxInfo ctx
        !txInInfo = findTxInInfo txOutRef txInfoInputs
    case txInInfo of
        Just !info -> do
            let !txOut = LedgerApiV2.txInInfoResolved info
                !address = LedgerApiV2.txOutAddress txOut
                !scriptHashMaybe = case LedgerApiV2.addressCredential address of
                    LedgerApiV2.ScriptCredential (LedgerApiV2.ValidatorHash hash) -> Just (LedgerApiV2.ScriptHash hash)
                    _ -> Nothing
            case scriptHashMaybe of
                Nothing -> return (Nothing, ["NO SCRIPT HASH FOUND"], Left P.undefined, Nothing)
                Just scriptHash ->
                    let !validator = getValidator scriptHash
                        !datum = OffChainHelpers.getUnsafe_LedgerApiV2Datum_From_TxOutOutputDatum txOut
                        !purpose = LedgerApiV2.Spending txOutRef
                        !ctxWithPurpose = ctx {LedgerApiV2.scriptContextPurpose = purpose}
                     in case validator of
                            Just !v -> do
                                -----------------
                                -- NOTE: para exportar y leer y probar desde cabal repl
                                -- OffChainHelpers.writePlutusDataToFile ("ctx-Spending-" ++ P.show (LedgerApiV2.txOutRefIdx txOutRef) ++ ".txt") ctxWithPurpose
                                -- OffChainHelpers.writePlutusDataToFile ("ctx-Spending-datum" ++ P.show (LedgerApiV2.txOutRefIdx txOutRef)++ ".txt") datum
                                -- OffChainHelpers.writePlutusDataToFile ("ctx-Spending-redeemer" ++ P.show (LedgerApiV2.txOutRefIdx txOutRef)++ ".txt") redeemer
                                -- ctx_  <- OffChainHelpers.readFileToPlutusDataAsTyped @LedgerApiV2.ScriptContext ("ctx-Spending-1.txt")
                                -- datum_  <- OffChainHelpers.readFileToPlutusDataAsTyped @LedgerApiV2.Datum ("ctx-Spending-datum1.txt")
                                -- redeemer_   <- OffChainHelpers.readFileToPlutusDataAsTyped @LedgerApiV2.Redeemer ("ctx-Spending-redeemer1.txt")
                                -- datumData = (LedgerApiV2.toData datum_ )
                                -- redeemerData = ( LedgerApiV2.toData redeemer_)
                                -- ctxData = (LedgerApiV2.toData  ctx_)
                                -- datumRaw = (LedgerApiV2.toBuiltinData datum_ )
                                -- redeemerRaw = ( LedgerApiV2.toBuiltinData redeemer_)
                                -- ctxRaw  = (LedgerApiV2.toBuiltinData  ctx_)
                                -----------------
                                let (log', eval, size) = evaluateScriptValidatorEX v datum redeemer ctxWithPurpose
                                return (Just scriptHash, log', eval, size)
                            Nothing -> return (Nothing, ["NO VALIDATOR FOUND"], Left P.undefined, Nothing)
        Nothing -> return (Nothing, ["NO TxOut FOUND"], Left P.undefined, Nothing)
testRedeemer _ getPolicy ctx (LedgerApiV2.Minting (LedgerApiV2.CurrencySymbol csBBS), redeemer) swTraceEvaluate = do
    -----------------
    -- traceIf swTraceEvaluate ">> testRedeemer - Minting!"
    -----------------
    let !scriptHash = LedgerApiV2.ScriptHash csBBS
        !policy = getPolicy scriptHash
        !purpose = LedgerApiV2.Minting (LedgerApiV2.CurrencySymbol csBBS)
    -----------------
    -- traceIf swTraceEvaluate $ ">> scriptHash " ++ show scriptHash
    -- traceIf swTraceEvaluate $ ">> policy " ++ show policy
    -- traceIf swTraceEvaluate $ ">> purpose " ++ show purpose
    -----------------
    case policy of
        Just !mp -> do
            -- traceIf swTraceEvaluate $ ">> mp " ++ show mp

            -- let
            --     scriptMintingPolicyV2 = OffChainHelpers.getScriptMintingPolicy mp
            --     scriptShortBsV2 = OffChainHelpers.getScriptShortBs scriptMintingPolicyV2
            --     size = LedgerScriptsV1.scriptSize scriptMintingPolicyV2
            -- -- traceIf swTraceEvaluate $ ">> scriptMintingPolicyV2 " ++ show ( scriptMintingPolicyV2   , size)
            -- case PlutusCore.defaultCostModelParams of
            --         Just costModelParams -> do
            --             let arguments =
            --                     [ LedgerApiV2.toData redeemer
            --                     , LedgerApiV2.toData ctx
            --                     ]
            --             -- traceIf swTraceEvaluate $ ">> arguments " ++ show ( arguments)
            --             let mcontext :: Either LedgerApiV2.CostModelApplyError LedgerApiV2.EvaluationContext
            --                 mcontext = LedgerApiV2.mkEvaluationContext costModelParams
            --             case mcontext of
            --                 Right evalContext -> do
            --                         traceIf swTraceEvaluate $ ">> RightevalContext"
            --                         let
            --                             a = LedgerApiV2.evaluateScriptCounting
            --                                 LedgerProtocolVersionsV1.vasilPV
            --                                 LedgerApiV2.Verbose
            --                                 evalContext
            --                                 scriptShortBsV2
            --                                 arguments
            --                         traceIf swTraceEvaluate $ ">> evaluateScriptCounting "  ++ show a

            --                     -- LedgerApiV2.evaluateScriptCounting
            --                     --     LedgerProtocolVersionsV1.vasilPV
            --                     --     LedgerApiV2.Verbose
            --                     --     evalContext
            --                     --     scriptShortBsV2
            --                     --     arguments
            --                 Left _ -> (["ERROR GETTING EVALUATION CONTEXT"], Left P.undefined)
            --     in case ev of
            --             Right _ -> (logout, ev, Just size)
            --             Left _ ->
            --                 case logout of
            --                     [] -> (["ERROR EVALUATING SCRIPT"], Left P.undefined, Just size)
            --                     _  -> (logout, Left P.undefined, Just size)
            -- Nothing -> (["COST MODEL NOT FOUND"], Left P.undefined, Just size)
            let (log', eval, size) = evaluateScriptPolicyEX mp redeemer (ctx {LedgerApiV2.scriptContextPurpose = purpose})
            traceIf swTraceEvaluate $ ">> evaluateScriptPolicyEX " ++ show (Just scriptHash, log', eval, size)
            return (Just scriptHash, log', eval, size)
        Nothing -> return (Nothing, ["NO MINTING POLICY FOUND"], Left P.undefined, Nothing)
testRedeemer _ _ _ _ _ = do return (Nothing, ["UNKNOWN SCRIPT PURPOSE"], Left P.undefined, Nothing)

------------------------------------------------------------------------------

evaluateScriptValidatorEX :: LedgerApiV2.Validator -> LedgerApiV2.Datum -> LedgerApiV2.Redeemer -> LedgerApiV2.ScriptContext -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Maybe Integer)
evaluateScriptValidatorEX validator datum redeemer context =
    let scriptUnValidatorV2 = OffChainHelpers.getScriptUnValidator validator
        scriptShortBsV2 = OffChainHelpers.getScriptShortBs scriptUnValidatorV2
        size = LedgerScriptsV1.scriptSize scriptUnValidatorV2
     in case PlutusCore.defaultCostModelParams of
            Just costModelParams ->
                let arguments =
                        [ LedgerApiV2.toData datum
                        , LedgerApiV2.toData redeemer
                        , LedgerApiV2.toData context
                        ]
                    mcontext :: Either LedgerApiV2.CostModelApplyError LedgerApiV2.EvaluationContext
                    mcontext = LedgerApiV2.mkEvaluationContext costModelParams
                    (logout, ev) =
                        case mcontext of
                            Right evalContext ->
                                LedgerApiV2.evaluateScriptCounting
                                    LedgerProtocolVersionsV1.vasilPV
                                    LedgerApiV2.Verbose
                                    evalContext
                                    scriptShortBsV2
                                    arguments
                            Left _ -> (["ERROR GETTING EVALUATION CONTEXT"], Left P.undefined)
                 in case ev of
                        Right _ -> (logout, ev, Just size)
                        Left _ ->
                            case logout of
                                [] -> (["ERROR EVALUATING SCRIPT"], ev, Just size)
                                _ -> (logout, ev, Just size)
            Nothing -> (["COST MODEL NOT FOUND"], Left P.undefined, Just size)

------------------------------------------------------------------------------

evaluateScriptPolicyEX :: LedgerApiV2.MintingPolicy -> LedgerApiV2.Redeemer -> LedgerApiV2.ScriptContext -> (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Maybe Integer)
evaluateScriptPolicyEX policy redeemer context =
    let scriptMintingPolicyV2 = OffChainHelpers.getScriptMintingPolicy policy
        scriptShortBsV2 = OffChainHelpers.getScriptShortBs scriptMintingPolicyV2
        size = LedgerScriptsV1.scriptSize scriptMintingPolicyV2
     in case PlutusCore.defaultCostModelParams of
            Just costModelParams ->
                let arguments =
                        [ LedgerApiV2.toData redeemer
                        , LedgerApiV2.toData context
                        ]
                    mcontext :: Either LedgerApiV2.CostModelApplyError LedgerApiV2.EvaluationContext
                    mcontext = LedgerApiV2.mkEvaluationContext costModelParams
                    (logout, ev) =
                        case mcontext of
                            Right evalContext ->
                                LedgerApiV2.evaluateScriptCounting
                                    LedgerProtocolVersionsV1.vasilPV
                                    LedgerApiV2.Verbose
                                    evalContext
                                    scriptShortBsV2
                                    arguments
                            Left _ -> (["ERROR GETTING EVALUATION CONTEXT"], Left P.undefined)
                 in case ev of
                        Right _ -> (logout, ev, Just size)
                        Left _ ->
                            case logout of
                                [] -> (["ERROR EVALUATING SCRIPT"], ev, Just size)
                                _ -> (logout, ev, Just size)
            Nothing -> (["COST MODEL NOT FOUND"], Left P.undefined, Just size)

---------------------------------------------------------------------------------
