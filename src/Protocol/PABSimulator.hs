
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.PABSimulator where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Control.Monad                         as Monad
import qualified Control.Monad.Freer.Internal          as MonadFreerInternal (Eff)
import qualified Control.Monad.IO.Class                as MonadIOClass (MonadIO (..))
import qualified Data.Aeson                            as DataAeson (decode)
import qualified Data.Maybe                            as DataMaybe
import qualified Plutus.PAB.Simulator                  as PABSimulator
import qualified Plutus.PAB.Webserver.Server           as PABServer
import           PlutusTx.Prelude                      hiding (unless)
import qualified Prelude                               as P
import qualified System.Directory                      as SystemDirectory
import qualified System.FilePath.Posix                 as SystemFilePathPosix

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.CLIHelpers                    as CLIHelpers
import qualified Generic.OffChainHelpers               as OffChainHelpers
import qualified Generic.PABHelpers                    as PABHelpers
import qualified Protocol.Constants                    as T
import qualified Protocol.Deploy                       as Deploy
import qualified Protocol.Fund.PABSimulator            as FundPABSimulator
import qualified Protocol.Fund.PABSimulatorCommissions as FundPABSimulatorCommissions
import qualified Protocol.Fund.PABSimulatorUser        as FundPABSimulatorUser
import qualified Protocol.Others.PABSimulator          as OthersPABSimulator
import qualified Protocol.PABContracts                 as PABContracts
import qualified Protocol.PABHelpers                   as PABHelpers
import qualified Protocol.PABTypes                     as T
import qualified Protocol.Protocol.PABSimulator        as ProtocolPABSimulator

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

runPABSimulator :: P.IO ()
runPABSimulator = do
    P.putStrLn ""
    P.putStrLn "--------------------------------"
    P.putStrLn ""
    Monad.void $ PABSimulator.runSimulationWith PABHelpers.handlers simulateInteractive

--------------------------------------------------------------------------------2

simulateInteractive :: MonadFreerInternal.Eff PABContracts.PABEffects ()
simulateInteractive = do
    pabShutdown <- PABServer.startServerDebug
    PABHelpers.prepareStdIn
    PABHelpers.waitStdinReady
    pabMainLoop (Just 1, 2) Nothing pabShutdown

------------------------------------------------------------------------------

menuEndPoints :: P.String -> PABContracts.PABParamsInMainMenu' P.String
menuEndPoints name (walletNro', _) protocolPABParams' _ = do
    MonadIOClass.liftIO $ CLIHelpers.printTitle $ name ++ " - MAIN MENU"
    ---------------------
    !isCoreTeam <- return $ PABHelpers.isCoreTeam walletNro'
    !isProtocolAdmin <- PABHelpers.isProtocolAdmin walletNro' protocolPABParams'
    !isMAYZHolderAdmin <- PABHelpers.isMAYZHolderAdmin walletNro' protocolPABParams'
    !isFundAdmin <- PABHelpers.isFundAdmin walletNro' protocolPABParams'
    !isMAYZHolder <- PABHelpers.isMAYZHolder walletNro'
    ---------------------
    case walletNro' of
        Nothing ->
             MonadIOClass.liftIO $ P.putStrLn "1 - Select Wallet"
        Just walletNro -> do
             MonadIOClass.liftIO $ P.putStrLn $ "1 - Select Wallet (" ++ P.show walletNro ++ ")"
             MonadIOClass.liftIO $ P.putStrLn $ "isCoreTeam: " ++ P.show isCoreTeam ++ " - isProtocolAdmin: " ++ P.show isProtocolAdmin ++ " - isMAYZHolderAdmin: " ++ P.show isMAYZHolderAdmin ++ " - isFundAdmin: " ++ P.show isFundAdmin ++ " - isMAYZHolder: " ++ P.show isMAYZHolder
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    ---------------------
    Monad.when isCoreTeam $ do
        MonadIOClass.liftIO $ P.putStrLn "21 - New Protocol"
        case protocolPABParams' of
            Nothing ->
                MonadIOClass.liftIO $ P.putStrLn "22 - Select Protocol"
            Just protocolPABParams ->
                MonadIOClass.liftIO $ P.putStrLn $ "22 - Select Protocol (" ++ P.show (T.pppProtocolPolicyID_CS protocolPABParams) ++ ")"
        MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    ---------------------
    Monad.when (DataMaybe.isJust protocolPABParams' && (isCoreTeam || isProtocolAdmin)) $ do
        MonadIOClass.liftIO $ P.putStrLn "31 - Protocol Actions"
    ---------------------
    Monad.when (  DataMaybe.isJust protocolPABParams' && (isCoreTeam || isProtocolAdmin || isMAYZHolder || isFundAdmin )) $ do
        MonadIOClass.liftIO $ P.putStrLn "32 - Fund Manage Actions"
    ---------------------
    Monad.when (  DataMaybe.isJust protocolPABParams' ) $ do
        MonadIOClass.liftIO $ P.putStrLn "33 - Fund User Actions"
    ---------------------
    Monad.when (  DataMaybe.isJust protocolPABParams' && (isCoreTeam || isProtocolAdmin || isMAYZHolderAdmin || isFundAdmin )) $ do
        MonadIOClass.liftIO $ P.putStrLn "34 - Fund Commissions Actions"
    ---------------------
    -- MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    -- MonadIOClass.liftIO $ P.putStrLn "41 - Save Simulation"
    -- MonadIOClass.liftIO $ P.putStrLn "42 - Load Simulation"
    MonadIOClass.liftIO $ P.putStrLn "35 - Other Actions"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "81 - Balances"
    MonadIOClass.liftIO $ P.putStrLn "82 - UTxOs at Wallet"
    MonadIOClass.liftIO $ P.putStrLn "83 - UTxOs at Script"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "91 - Time and Slot"
    MonadIOClass.liftIO $ P.putStrLn "99 - Exit"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Enter option:"
    option <- MonadIOClass.liftIO P.getLine
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    return option

--------------------------------------------------------------------------------2

pabMainLoop :: PABContracts.PABParamsInMainMenu
pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown = do
    -- MonadIOClass.liftIO $ ControlConcurrent.threadDelay 10000
    -- _ <- MonadIOClass.liftIO $ SystemIO.hWaitForInput SystemIO.stdin 100
    option <- menuEndPoints "MAYZ PAB Simulator" (walletNro', walletCount) protocolPABParams' pabShutdown
    case option of
        "1" -> do
            (walletNro'_, walletCount_) <- PABHelpers.pabSelectWallet (walletNro', walletCount)
            pabMainLoop (walletNro'_, walletCount_) protocolPABParams' pabShutdown
        "21" ->
            pabCreateProtocolParams (walletNro', walletCount) protocolPABParams' pabShutdown
        "22" ->
            pabLoadProtocolParams (walletNro', walletCount) protocolPABParams' pabShutdown
        "31" ->
            case (walletNro', protocolPABParams') of
                (Just walletNro, Just protocolPABParams) ->
                    ProtocolPABSimulator.pabMainLoop (walletNro, walletCount) protocolPABParams pabMainLoop pabShutdown
                _ -> do
                    MonadIOClass.liftIO $ P.putStrLn "You must select a Wallet and a Protocol"
                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                    PABHelpers.waitKeyPress
                    pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown
        "32" ->
            case (walletNro', protocolPABParams') of
            (Just walletNro, Just protocolPABParams) ->
                FundPABSimulator.pabMainLoop True (walletNro, walletCount) protocolPABParams Nothing pabMainLoop pabShutdown
            _ -> do
                MonadIOClass.liftIO $ P.putStrLn "You must select a Wallet and a Protocol"
                MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                PABHelpers.waitKeyPress
                pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown
        "33" ->
            case (walletNro', protocolPABParams') of
            (Just walletNro, Just protocolPABParams) ->
                FundPABSimulatorUser.pabMainLoop False (walletNro, walletCount) protocolPABParams Nothing pabMainLoop pabShutdown
            _ -> do
                MonadIOClass.liftIO $ P.putStrLn "You must select a Wallet and a Protocol"
                MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                PABHelpers.waitKeyPress
                pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown
        "34" ->
            case (walletNro', protocolPABParams') of
                (Just walletNro, Just protocolPABParams) ->
                    FundPABSimulatorCommissions.pabMainLoop False (walletNro, walletCount) protocolPABParams Nothing pabMainLoop pabShutdown
                _ -> do
                    MonadIOClass.liftIO $ P.putStrLn "You must select a Wallet and a Protocol"
                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                    PABHelpers.waitKeyPress
                    pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown

        "35" ->
            OthersPABSimulator.pabMainLoop (walletNro', walletCount) protocolPABParams' pabMainLoop pabShutdown

        "81" -> do
            PABHelpers.pabBalances (walletNro', walletCount) protocolPABParams'
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown
        "82" -> do
            PABHelpers.pabUTxOAtWallet (walletNro', walletCount)
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown
        "83" -> do
            PABHelpers.pabUTxOAtScript protocolPABParams'
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown
        "91" -> do
            PABHelpers.pabTimeAndSlot
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown
        "99" -> do
            PABHelpers.pabEndSimulation (walletNro', walletCount) protocolPABParams' pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid option"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown

--------------------------------------------------------------------------------2

pabCreateProtocolParams :: PABContracts.PABParamsInMainMenu
pabCreateProtocolParams (walletNro', walletCount) protocolPABParams' pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Create Protocol"
    case walletNro' of
        Just walletNro -> do
            blockchain <- PABSimulator.blockchain
            let uTxOutRefAt = fst <$> PABHelpers.getUTxOsListInPABSimulator blockchain (PABHelpers.walletPaymentPubKeyHashAddress walletNro)
                protocolPolicyID_TxOutRef = head uTxOutRefAt
            protocolPABParams <- MonadIOClass.liftIO $ Deploy.deploy_ProtocolPAB_With_RequestingParams protocolPolicyID_TxOutRef
            -- MonadIOClass.liftIO $ Deploy.deploy_FundFactory_With_RequestingParams (T.pppProtocolPolicyID_CS protocolPABParams)
            PABHelpers.waitKeyPress
            pabMainLoop (Just walletNro, walletCount) (Just protocolPABParams) pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Wallet"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown

--------------------------------------------------------------------------------2

pabLoadProtocolParams :: PABContracts.PABParamsInMainMenu
pabLoadProtocolParams (walletNro', walletCount) protocolPABParams' pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Load Protocol"
    -- MonadIOClass.liftIO $ P.putStrLn "Path (default=export/protocol):"
    -- !path <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault "export/protocol"
    let !path = "export/protocol-v" ++ P.show T.protocolFactoryVersion
    !existPath <- MonadIOClass.liftIO $ SystemDirectory.doesPathExist path
    if existPath
        then do
            !protocolName <- MonadIOClass.liftIO $ CLIHelpers.selectFolder path ""
            if P.null protocolName --TODO: controlar length protocolName == 0
                then do
                    pabMainLoop (walletNro', walletCount) protocolPABParams' pabShutdown
                else do
                    !exist <- MonadIOClass.liftIO $ SystemDirectory.doesFileExist (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolPAB.json")
                    if exist
                        then do
                            !jsonFile <- MonadIOClass.liftIO $ OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolPAB.json")
                            case DataAeson.decode jsonFile :: Maybe T.ProtocolPABParams of
                                Nothing -> do
                                    MonadIOClass.liftIO $ P.putStrLn "Invalid input. Can't decode ProtocolPAB.json file"
                                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                                    pabLoadProtocolParams (walletNro', walletCount) protocolPABParams' pabShutdown
                                Just protocolPABParams -> do
                                    MonadIOClass.liftIO $ P.putStrLn "Protocol loaded"
                                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                                    pabMainLoop (walletNro', walletCount) (Just protocolPABParams) pabShutdown
                        else do
                            MonadIOClass.liftIO $ P.putStrLn "Invalid input. Can't find ProtocolPAB.json file"
                            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                            pabLoadProtocolParams (walletNro', walletCount) protocolPABParams' pabShutdown
        else do
            MonadIOClass.liftIO $ P.putStrLn "Invalid input. Can't find Path"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabLoadProtocolParams (walletNro', walletCount) protocolPABParams' pabShutdown

--------------------------------------------------------------------------------2
