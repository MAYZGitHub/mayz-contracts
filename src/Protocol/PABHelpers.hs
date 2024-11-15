{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.PABHelpers where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Cardano.Node.Emulator.TimeSlot as CardanoNodeEmulatorTimeSlot
import qualified Control.Concurrent.STM         as ConcurrentSTM (atomically)
import qualified Control.Monad                  as Monad
import qualified Control.Monad.Freer.Internal   as MonadFreerInternal (Eff)
import qualified Control.Monad.IO.Class         as MonadIOClass (MonadIO (..))
import qualified Data.Default                   as DataDefault (def)
import qualified Data.Maybe                     as DataMaybe
import qualified Ledger
import qualified Ledger.Value                   as LedgerValue
import qualified Plutus.PAB.Simulator           as PABSimulator
import qualified Plutus.V2.Ledger.Api           as LedgerApiV2
import           PlutusTx.Prelude               hiding (unless)
import qualified Prelude                        as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2


import qualified Generic.CLIHelpers             as CLIHelpers
import qualified Generic.OnChainHelpers         as OnChainHelpers
import qualified Generic.PABHelpers             as PABHelpers
import qualified Generic.Types                  as T
import qualified Protocol.Constants             as T
import qualified Protocol.Fund.Holding.Types    as FundHoldingT
import qualified Protocol.Fund.Types            as FundT
import qualified Protocol.Fund.InvestUnit.Types      as InvestUnitT
import qualified Protocol.PABContracts          as PABContracts
import qualified Protocol.PABTypes              as T
import qualified Protocol.Protocol.Types        as ProtocolT
import qualified Protocol.Script.Types          as ScriptT
import qualified Protocol.Types                 as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

pabSelectWallet ::  (Maybe Integer, Integer) -> MonadFreerInternal.Eff PABContracts.PABEffects (Maybe Integer, Integer)
pabSelectWallet (_, walletCount')  = do
    (walletNro, walletCount) <- PABHelpers.selectWallet "Select Wallet" 1 walletCount' True
    return (Just walletNro, walletCount)

--------------------------------------------------------------------------------2

pabUTxOAtWallet :: (Maybe Integer, Integer) ->  MonadFreerInternal.Eff PABContracts.PABEffects ()
pabUTxOAtWallet (walletNro', _) = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "UTxOs at Wallet"
    case walletNro' of
        Just walletNro -> do
            let address = PABHelpers.walletPaymentPubKeyHashAddress walletNro
            PABHelpers.showUTxOsAtAddress @LedgerApiV2.Datum address
            PABHelpers.waitKeyPress
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Wallet"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress

--------------------------------------------------------------------------------2

pabUTxOAtScript :: Maybe T.ProtocolPABParams -> MonadFreerInternal.Eff PABContracts.PABEffects ()
pabUTxOAtScript protocolPABParams' = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "UTxOs at Scripts"
    case protocolPABParams' of
        (Just protocolPABParams) -> do
            MonadIOClass.liftIO $ CLIHelpers.printSubSubTitle "Protocol Validator"
            PABHelpers.showUTxOsAtAddress @ProtocolT.ValidatorDatum (T.pppProtocolValidator_Address protocolPABParams)
            MonadIOClass.liftIO $ CLIHelpers.printSubSubTitle "Script Validator"
            PABHelpers.showUTxOsAtAddress @ScriptT.ValidatorDatum (T.pppScriptValidator_Address protocolPABParams)
            MonadIOClass.liftIO $ CLIHelpers.printSubSubTitle "Fund Validator"
            PABHelpers.showUTxOsAtAddress @FundT.ValidatorDatum (T.ffppFundValidator_Address $ head $ T.pppFundFactoryPABParams protocolPABParams)
            MonadIOClass.liftIO $ CLIHelpers.printSubSubTitle "Invest Unit Validator"
            PABHelpers.showUTxOsAtAddress @InvestUnitT.ValidatorDatum (T.pppInvestUnitValidator_Address protocolPABParams)
            let fundHoldingAddresses = [ ("Fund " ++ P.show (T.fppFundPolicy_CS fundPABParams) ++ " Holdings", T.fppFundHoldingValidator_Address fundPABParams ) | fundPABParams <- T.ffppFundPABParams $ head $ T.pppFundFactoryPABParams protocolPABParams ]
                showFundHolding (name, address) = do
                    MonadIOClass.liftIO $ CLIHelpers.printSubSubTitle name
                    PABHelpers.showUTxOsAtAddress @FundHoldingT.ValidatorDatum address
            mapM_ showFundHolding fundHoldingAddresses
            PABHelpers.waitKeyPress
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Protocol"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress

--------------------------------------------------------------------------------2

pabBalances :: (Maybe Integer, Integer) ->  Maybe T.ProtocolPABParams -> MonadFreerInternal.Eff PABContracts.PABEffects ()
pabBalances (walletNro', walletCount) protocolPABParams'  = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Balances"
    let validatorsNameAndHashes = case protocolPABParams' of
            Just protocolPABParams ->
                    let fundHashes = [ ("Fund " ++ P.show (T.fppFundPolicy_CS fundPABParams) ++ " Holdings", T.fppFundHoldingValidator_Hash fundPABParams ) | fundPABParams <- T.ffppFundPABParams $ head $ T.pppFundFactoryPABParams protocolPABParams ]
                    in  [
                            ("Protocol", T.pppProtocolValidator_Hash protocolPABParams),
                            ("Funds", T.ffppFundValidator_Hash $ head $ T.pppFundFactoryPABParams protocolPABParams),
                            ("Invest Units", T.pppInvestUnitValidator_Hash protocolPABParams),
                            ("Scripts", T.pppScriptValidator_Hash protocolPABParams)
                        ] ++ fundHashes
            _ -> []

    PABHelpers.balances (walletNro', walletCount) validatorsNameAndHashes
    PABHelpers.waitKeyPress

--------------------------------------------------------------------------------2

pabTimeAndSlot :: MonadFreerInternal.Eff PABContracts.PABEffects ()
pabTimeAndSlot = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Time and Slot"
    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
    let posixTime = CardanoNodeEmulatorTimeSlot.slotToEndPOSIXTime DataDefault.def slot
    MonadIOClass.liftIO $ P.putStrLn $ "Slot: " ++ P.show slot
    MonadIOClass.liftIO $ P.putStrLn $ "Time: " ++ CLIHelpers.formatTime posixTime ++ "(" ++ P.show posixTime ++ ")"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    PABHelpers.waitKeyPress

--------------------------------------------------------------------------------2

pabEndSimulation :: (Maybe Integer, Integer) ->  Maybe T.ProtocolPABParams -> MonadFreerInternal.Eff PABContracts.PABEffects () -> MonadFreerInternal.Eff PABContracts.PABEffects ()
pabEndSimulation (walletNro', walletCount) protocolPABParams' pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Shutting Down PAB Simulator"
    pabBalances (walletNro', walletCount) protocolPABParams'
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Exiting now..."
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    pabShutdown

--------------------------------------------------------------------------------2

isCoreTeam :: Maybe Integer -> Bool
isCoreTeam Nothing          = False
isCoreTeam (Just walletNro) = Ledger.unPaymentPubKeyHash (PABHelpers.walletPaymentPubKeyHash walletNro) `P.elem` T.coreTeamWallets_aux

--------------------------------------------------------------------------------2

isProtocolAdmin :: Maybe Integer -> Maybe T.ProtocolPABParams -> MonadFreerInternal.Eff PABContracts.PABEffects Bool
isProtocolAdmin Nothing _ = return False
isProtocolAdmin _ Nothing = return False
isProtocolAdmin (Just walletNro) (Just protocolPABParams) = do
        blockchain <- PABSimulator.blockchain
        ---------------------
        let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
            !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        ---------------------
        let !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain (T.pppProtocolValidator_Address protocolPABParams)
            !uTxO_With_ProtocolDatum' = [ (txOutRef, txOut,  ProtocolT.getProtocol_DatumType $ OnChainHelpers.fromJust $ PABHelpers.getDatumInPABSimulator @ProtocolT.ValidatorDatum blockchain (txOutRef, txOut) )
                                                    | (txOutRef, txOut) <- uTxOuts,
                                                        LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) protocolID_AC > 0
                                                        && DataMaybe.isJust (PABHelpers.getDatumInPABSimulator @ProtocolT.ValidatorDatum blockchain (txOutRef, txOut) )
                                                ]
        ---------------------
        protocolAdmins <- case uTxO_With_ProtocolDatum' of
            [] -> do
                return []
            (uTxO_With_ProtocolDatum:_) -> do
                ---------------------
                let !protocolDatum = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
                return $ ProtocolT.pdAdmins protocolDatum
        ---------------------
        return $ Ledger.unPaymentPubKeyHash (PABHelpers.walletPaymentPubKeyHash walletNro) `P.elem` protocolAdmins

--------------------------------------------------------------------------------2

isMAYZHolderAdmin :: Maybe Integer -> Maybe T.ProtocolPABParams -> MonadFreerInternal.Eff PABContracts.PABEffects Bool
isMAYZHolderAdmin Nothing _ = return False
isMAYZHolderAdmin _ Nothing = return False
isMAYZHolderAdmin (Just walletNro) (Just protocolPABParams) = do
        blockchain <- PABSimulator.blockchain
        ---------------------
        let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
            !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        ---------------------
        let !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain (T.pppProtocolValidator_Address protocolPABParams)
            !uTxO_With_ProtocolDatum' = [ (txOutRef, txOut,  ProtocolT.getProtocol_DatumType $ OnChainHelpers.fromJust $ PABHelpers.getDatumInPABSimulator @ProtocolT.ValidatorDatum blockchain (txOutRef, txOut) )
                                                    | (txOutRef, txOut) <- uTxOuts,
                                                        LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) protocolID_AC > 0
                                                        && DataMaybe.isJust (PABHelpers.getDatumInPABSimulator @ProtocolT.ValidatorDatum blockchain (txOutRef, txOut) )
                                                ]
        ---------------------
        protocolAdmins <- case uTxO_With_ProtocolDatum' of
            [] -> do
                return []
            (uTxO_With_ProtocolDatum:_) -> do
                ---------------------
                let !protocolDatum = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
                return $ ProtocolT.pdDelegatorsAdmins protocolDatum
        ---------------------
        return $ Ledger.unPaymentPubKeyHash (PABHelpers.walletPaymentPubKeyHash walletNro) `P.elem` protocolAdmins

--------------------------------------------------------------------------------2

isMAYZHolder :: Maybe Integer -> MonadFreerInternal.Eff PABContracts.PABEffects Bool
isMAYZHolder Nothing  = return False
isMAYZHolder (Just _) = return True
    -- TODO: tenemos que agregar el token mayx en los pab params para pdoer acceder luego
    -- do
    -- !balances' <- PABSimulator.currentBalances
    -- let !balanceList = DataMap.toList balances'
    --     fromWalletEntity walletNro' (entity, _) = PABHelpers.fromWallet walletNro' entity
    --     !entiyValue' = find (fromWalletEntity walletNro) balanceList
    --     ----------------
    -- case entiyValue' of
    --     Nothing -> return False
    --     Just (_, value) -> do
    --         let !tokenMAYZ_AC = LedgerValue.AssetClass (T.tokenMAYZ_CS, T.tokenMAYZ_TN)
    --         return $ LedgerValue.assetClassValueOf value tokenMAYZ_AC > 0

--------------------------------------------------------------------------------2

isFundAdmin :: Maybe Integer -> Maybe T.ProtocolPABParams -> MonadFreerInternal.Eff PABContracts.PABEffects Bool
isFundAdmin Nothing _ = return False
isFundAdmin _ Nothing = return False
isFundAdmin (Just walletNro) (Just protocolPABParams) = do
        blockchain <- PABSimulator.blockchain
        ---------------------
        let getAdmins :: T.FundPABParams -> MonadFreerInternal.Eff PABContracts.PABEffects [T.WalletPaymentPKH]
            getAdmins fundPABParams = do
                let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
                    !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                ---------------------
                let !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain (T.fppFundValidator_Address fundPABParams)
                    !uTxO_With_FundDatum' = [ (txOutRef, txOut,  FundT.getFund_DatumType $ OnChainHelpers.fromJust $ PABHelpers.getDatumInPABSimulator @FundT.ValidatorDatum blockchain (txOutRef, txOut) )
                                                            | (txOutRef, txOut) <- uTxOuts,
                                                                LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) fundID_AC > 0
                                                                && DataMaybe.isJust (PABHelpers.getDatumInPABSimulator @FundT.ValidatorDatum blockchain (txOutRef, txOut) )
                                                        ]
                ---------------------
                case uTxO_With_FundDatum' of
                    [] -> do
                        return []
                    (uTxO_With_FundDatum:_) -> do
                        ---------------------
                        let !fundDatum = (\(_, _, dat) -> dat) uTxO_With_FundDatum
                        return $ FundT.fdAdmins fundDatum
                ---------------------
        fundAdmins <- Monad.foldM (\acc fundPABParams -> do
                    admins <- getAdmins fundPABParams
                    return $ admins ++ acc
                 ) [] (T.ffppFundPABParams $ head $ T.pppFundFactoryPABParams protocolPABParams)
        ---------------------
        return $ Ledger.unPaymentPubKeyHash (PABHelpers.walletPaymentPubKeyHash walletNro) `P.elem`  fundAdmins

--------------------------------------------------------------------------------2

isThisFundAdmin :: Maybe Integer -> Maybe T.ProtocolPABParams -> Maybe T.FundPABParams ->  MonadFreerInternal.Eff PABContracts.PABEffects Bool
isThisFundAdmin Nothing _ _= return False
isThisFundAdmin _ Nothing _= return False
isThisFundAdmin _ _ Nothing = return False
isThisFundAdmin (Just walletNro) (Just _) (Just fundPABParams) = do
        blockchain <- PABSimulator.blockchain
        ---------------------
        let getAdmins :: MonadFreerInternal.Eff PABContracts.PABEffects [T.WalletPaymentPKH]
            getAdmins = do
                let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
                    !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                ---------------------
                let !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain (T.fppFundValidator_Address fundPABParams)
                    !uTxO_With_FundDatum' = [ (txOutRef, txOut,  FundT.getFund_DatumType $ OnChainHelpers.fromJust $ PABHelpers.getDatumInPABSimulator @FundT.ValidatorDatum blockchain (txOutRef, txOut) )
                                                            | (txOutRef, txOut) <- uTxOuts,
                                                                LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) fundID_AC > 0
                                                                && DataMaybe.isJust (PABHelpers.getDatumInPABSimulator @FundT.ValidatorDatum blockchain (txOutRef, txOut) )
                                                        ]
                ---------------------
                case uTxO_With_FundDatum' of
                    [] -> do
                        return []
                    (uTxO_With_FundDatum:_) -> do
                        ---------------------
                        let !fundDatum = (\(_, _, dat) -> dat) uTxO_With_FundDatum
                        return $ FundT.fdAdmins fundDatum
                ---------------------
        fundAdmins <- getAdmins
        ---------------------
        return $ Ledger.unPaymentPubKeyHash (PABHelpers.walletPaymentPubKeyHash walletNro) `P.elem`  fundAdmins

--------------------------------------------------------------------------------2

pabShowInvestUnit :: PABContracts.PABParamsInFundMenu
pabShowInvestUnit _ (_, _) protocolPABParams fundPABParams' _ _ = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Invest Unit details"
    case fundPABParams' of
        (Just fundPABParams) -> do
            blockchain <- PABSimulator.blockchain
            let !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain (T.pppInvestUnitValidator_Address protocolPABParams)
            ---------------------
                !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
                !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
            ---------------------
                !uTxO_With_InvestUnitDatum' = [ (txOutRef, txOut,  InvestUnitT.getInvestUnit_DatumType $ OnChainHelpers.fromJust $ PABHelpers.getDatumInPABSimulator @InvestUnitT.ValidatorDatum blockchain (txOutRef, txOut) )
                                                        | (txOutRef, txOut) <- uTxOuts,
                                                            LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) investUnitID_AC > 0
                                                            && DataMaybe.isJust (PABHelpers.getDatumInPABSimulator @InvestUnitT.ValidatorDatum blockchain (txOutRef, txOut) )
                                                    ]
            ---------------------
            case uTxO_With_InvestUnitDatum' of
                [] -> do
                    MonadIOClass.liftIO $ P.putStrLn "No Invest Unit found. Did you prepare the Fund?"
                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                    PABHelpers.waitKeyPress
                (uTxO_With_InvestUnitDatum:_) -> do
                    ---------------------
                    let !investUnitDatum = (\(_, _, dat) -> dat) uTxO_With_InvestUnitDatum
                        !investUnit = InvestUnitT.iudInvestUnit investUnitDatum
                        !investUnitTokens = T.iuValues investUnit
                    ---------------------
                        formatInvestUnitTokens  = [P.show cs ++ "." ++ P.show (LedgerApiV2.unTokenName tn) ++ ": " ++ P.show am | (cs, tn, am) <- investUnitTokens]
                    ---------------------
                    mapM_ (MonadIOClass.liftIO . P.putStrLn) formatInvestUnitTokens
                    ---------------------
                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                    PABHelpers.waitKeyPress
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress

--------------------------------------------------------------------------------2

pabShowFTPriceADA :: PABContracts.PABParamsInFundMenu
pabShowFTPriceADA _ (_, _) _rotocolPABParams _fundPABParams' _ _ = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "FT Price"
    -- case fundPABParams' of
    --     (Just fundPABParams) -> do
    --         blockchain <- PABSimulator.blockchain
    --         let !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain (T.pppOracleValidator_Address protocolPABParams)
    --         ---------------------
    --             !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    --             !oracleID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.oracleID_TN)
    --         ---------------------
    --             !uTxO_With_OracleDatum' = [ (txOutRef, txOut,  OracleT.getOracleDatumType $ OnChainHelpers.fromJust $ PABHelpers.getDatumInPABSimulator @OracleT.ValidatorDatum blockchain (txOutRef, txOut) )
    --                                                     | (txOutRef, txOut) <- uTxOuts,
    --                                                         LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) oracleID_AC > 0
    --                                                         && DataMaybe.isJust (PABHelpers.getDatumInPABSimulator @OracleT.ValidatorDatum blockchain (txOutRef, txOut) )
    --                                                 ]
    --         ---------------------
    --         case uTxO_With_OracleDatum' of
    --             [] -> do
    --                 MonadIOClass.liftIO $ P.putStrLn "No Oracle found. Did you prepare the Fund?"
    --                 MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    --                 PABHelpers.waitKeyPress
    --             (uTxO_With_OracleDatum:_) -> do
    --                 ---------------------
    --                 let !oracleDatum = (\(_, _, dat) -> dat) uTxO_With_OracleDatum
    --                     !priceADA = OracleT.odPriceADA oracleDatum
    --                 ---------------------
    --                 MonadIOClass.liftIO . P.putStrLn $ "ADA (lovelace): " ++ P.show priceADA
    --                 ---------------------
    --                 MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    --                 PABHelpers.waitKeyPress
    --     _ -> do
    --         MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
    --         MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    --         PABHelpers.waitKeyPress

--------------------------------------------------------------------------------2

pabShowReIdxPriceADA :: PABContracts.PABParamsInFundMenu
pabShowReIdxPriceADA _ (_, _) _protocolPABParams _fundPABParams' _ _ = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Prices for Re-Indexing"
    -- case fundPABParams' of
    --     (Just fundPABParams) -> do
    --         blockchain <- PABSimulator.blockchain
    --         let !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain (T.pppOracleValidator_Address protocolPABParams)
    --         ---------------------
    --             !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    --             !oracleReIdxID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.oracleReIdxID_TN)
    --         ---------------------
    --             !uTxO_With_OracleReIdxDatum' = [ (txOutRef, txOut,  OracleT.getOracleReIdxDatumType $ OnChainHelpers.fromJust $ PABHelpers.getDatumInPABSimulator @OracleT.ValidatorDatum blockchain (txOutRef, txOut) )
    --                                                     | (txOutRef, txOut) <- uTxOuts,
    --                                                         LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) oracleReIdxID_AC > 0
    --                                                         && DataMaybe.isJust (PABHelpers.getDatumInPABSimulator @OracleT.ValidatorDatum blockchain (txOutRef, txOut) )
    --                                                 ]
    --         ---------------------
    --         case uTxO_With_OracleReIdxDatum' of
    --             [] -> do
    --                 MonadIOClass.liftIO $ P.putStrLn "No OracleReIdx found. Did you prepare the Fund?"
    --                 MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    --                 PABHelpers.waitKeyPress
    --             (uTxO_With_OracleReIdxDatum:_) -> do
    --                 ---------------------
    --                 let !oracleReIdxDatum = (\(_, _, dat) -> dat) uTxO_With_OracleReIdxDatum
    --                     !investUnit = OracleT.oridTokensPriceADA oracleReIdxDatum
    --                     !investUnitTokens = T.iuValues investUnit
    --                 ---------------------
    --                     formatInvestUnitTokens  = [P.show cs ++ "." ++ P.show (LedgerApiV2.unTokenName tn) ++ ": " ++ P.show am | (cs, tn, am) <- investUnitTokens]
    --                 ---------------------
    --                 mapM_ (MonadIOClass.liftIO . P.putStrLn) formatInvestUnitTokens
    --                 ---------------------
    --                 MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    --                 PABHelpers.waitKeyPress
    --     _ -> do
    --         MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
    --         MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    --         PABHelpers.waitKeyPress

--------------------------------------------------------------------------------2

