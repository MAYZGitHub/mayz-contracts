
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.Script.PABSimulator where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Control.Monad.IO.Class as MonadIOClass (MonadIO (..))
import qualified Plutus.PAB.Simulator   as PABSimulator
import           PlutusTx.Prelude       hiding (unless)
import qualified Prelude                as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.CLIHelpers     as CLIHelpers
import qualified Generic.PABHelpers     as PABHelpers
import qualified Protocol.PABContracts  as PABContracts
import qualified Protocol.PABTypes      as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- menuEndPoints :: P.String -> (Integer, Integer) -> T.ProtocolPABParams -> P.IO P.String
-- menuEndPoints name _ _ = do
--     CLIHelpers.printTitle $ "SCRIPTS MENU - " ++ name
--     P.putStrLn "31 - Add Scripts"
--     P.putStrLn "32 - Delete Scripts"
--     P.putStrLn "--------------------------------"
--     P.putStrLn "0  - Return to Main Menu"
--     P.putStrLn "1  - Return to Previus Menu"
--     P.putStrLn "99 - Exit"
--     P.putStrLn "--------------------------------"
--     P.putStrLn "Enter option:"
--     option <- P.getLine
--     P.putStrLn "--------------------------------"
--     return option

-- --------------------------------------------------------------------------------2

-- pabMainLoop :: PABContracts.PABParamsInProtocolScriptMenu
-- pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToMainMenu' pabShutdown = do
--     let name = "Scripts For: " ++ P.show (T.pppProtocolPolicyID_CS protocolPABParams)
--     option <- MonadIOClass.liftIO $ menuEndPoints name (walletNro, walletCount) protocolPABParams
--     case TextRead.readMaybe option :: Maybe Integer of
--         Just 31 ->
--             pabScriptAdd (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToMainMenu' pabShutdown
--         Just 32 ->
--             pabScriptDelete (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToMainMenu' pabShutdown
--         Just 1 ->
--             pabReturnToMainMenu' (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
--         Just 0 ->
--             pabReturnToMainMenu (Just walletNro, walletCount) (Just protocolPABParams) pabShutdown
--         Just 99 -> do
--             MonadIOClass.liftIO $ P.putStrLn "Balances at the End of Simulation:"
--             MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
--             -- let validatorsNameAndHashes = [("Protocol", T.pppProtocolValidator_Hash protocolPABParams), ("Protocol Scripts", T.pppScriptValidator_Hash protocolPABParams)]
--             let validatorsNameAndHashes = [
--                                     ("Protocol", T.pppProtocolValidator_Hash protocolPABParams),
--                                     ("Funds", T.fppFundValidator_Hash fundPABParams),
--                                     ("Scripts", T.pppScriptValidator_Hash protocolPABParams)
--                                 ]
--             PABHelpers.balances (Just walletNro, walletCount) validatorsNameAndHashes pabShutdown
--             MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
--             MonadIOClass.liftIO $ P.putStrLn "Exiting now..."
--             MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
--             pabShutdown
--         _ -> do
--             MonadIOClass.liftIO $ P.putStrLn "Invalid option"
--             MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
--             pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToMainMenu' pabShutdown

-- ------------------------------------------------------------------------------

-- pabScriptAdd :: PABContracts.PABParamsInProtocolScriptMenu
-- pabScriptAdd (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToMainMenu' pabShutdown = do
--     let contract =
--             PABContracts.PABProtocolScriptAdd
--                 T.PABProtocolScriptAddParams
--                     { T.ppsapProtocolPABParams = protocolPABParams
--                     }
--     contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
--     PABHelpers.waitContractAndKeyPress contractInstance
--     pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToMainMenu' pabShutdown

-- --------------------------------------------------------------------------------2

-- pabScriptDelete :: PABContracts.PABParamsInProtocolScriptMenu
-- pabScriptDelete (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToMainMenu' pabShutdown = do
--     let contract =
--             PABContracts.PABProtocolScriptDelete
--                 T.PABProtocolScriptDeleteParams
--                     { T.ppsdpProtocolPABParams = protocolPABParams
--                     }

--     contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
--     PABHelpers.waitContractAndKeyPress contractInstance
--     pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToMainMenu' pabShutdown

--------------------------------------------------------------------------------2

pabScriptAddInProtocol :: PABContracts.PABParamsInProtocolScriptMenu
pabScriptAddInProtocol (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToProtocol pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Protocol Add Scripts"
    let contract =
            PABContracts.PABProtocolScriptAdd
                T.PABProtocolScriptAddParams
                    { T.ppsapProtocolPABParams = protocolPABParams
                    }
    contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
    PABHelpers.waitContractAndKeyPress contractInstance
    pabReturnToProtocol (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

----------------------------------------------------------------------------

pabScriptDeleteInProtocol :: PABContracts.PABParamsInProtocolScriptMenu
pabScriptDeleteInProtocol (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToProtocol pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Protocol Delete Scripts"
    let contract =
            PABContracts.PABProtocolScriptDelete
                T.PABProtocolScriptDeleteParams
                    { T.ppsdpProtocolPABParams = protocolPABParams
                    }

    contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
    PABHelpers.waitContractAndKeyPress contractInstance
    pabReturnToProtocol (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabScriptAddInFund :: PABContracts.PABParamsInFundScriptMenu
pabScriptAddInFund (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabReturnToFundMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Fund Add Scripts"
    case fundPABParams' of
        (Just fundPABParams) -> do
            let contract =
                    PABContracts.PABFundScriptAdd
                        T.PABFundScriptAddParams
                            { T.pfsapProtocolPABParams = protocolPABParams,
                              T.pfsapFundPABParams = fundPABParams
                            }
            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabReturnToFundMenu True (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
            pabReturnToFundMenu True (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

----------------------------------------------------------------------------

pabScriptDeleteInFund :: PABContracts.PABParamsInFundScriptMenu
pabScriptDeleteInFund (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabReturnToFundMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Fund Delete Scripts"
    case fundPABParams' of
        (Just fundPABParams) -> do
            let contract =
                    PABContracts.PABFundScriptDelete
                        T.PABFundScriptDeleteParams
                            { T.pfsdpProtocolPABParams = protocolPABParams,
                              T.pfsdpFundPABParams = fundPABParams
                            }

            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabReturnToFundMenu True (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
            pabReturnToFundMenu True (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

----------------------------------------------------------------------------
