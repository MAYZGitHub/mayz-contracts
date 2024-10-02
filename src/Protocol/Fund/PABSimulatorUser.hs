--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.Fund.PABSimulatorUser where

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

import qualified Control.Monad          as Monad
import qualified Data.Maybe             as DataMaybe
import qualified Generic.CLIHelpers     as CLIHelpers
import qualified Generic.CLIHelpers     as HelpersCLI
import qualified Generic.PABHelpers     as PABHelpers
import qualified Protocol.PABContracts  as PABContracts
import qualified Protocol.PABHelpers    as PABHelpers
import qualified Protocol.PABTypes      as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

menuEndPoints :: P.String -> PABContracts.PABParamsInFundMenu' P.String
menuEndPoints name _ (walletNro, _) protocolPABParams fundPABParams' _ _  = do
    ---------------------
    !isCoreTeam <- return $ PABHelpers.isCoreTeam (Just walletNro)
    !isProtocolAdmin <- PABHelpers.isProtocolAdmin (Just walletNro) (Just protocolPABParams)
    !isMAYZHolderAdmin <- PABHelpers.isMAYZHolderAdmin (Just walletNro) (Just protocolPABParams)
    !isFundAdmin <- PABHelpers.isFundAdmin (Just walletNro) (Just protocolPABParams)
    !isThisFundAdmin <- PABHelpers.isThisFundAdmin (Just walletNro) (Just protocolPABParams) fundPABParams'
    !isMAYZHolder <- PABHelpers.isMAYZHolder (Just walletNro)
    ---------------------
    MonadIOClass.liftIO $ CLIHelpers.printTitle $ "FUNDS USER MENU - " ++ name
    MonadIOClass.liftIO $ P.putStrLn $ "isCoreTeam: " ++ P.show isCoreTeam ++ " - isProtocolAdmin: " ++ P.show isProtocolAdmin ++ " - isMAYZHolderAdmin: " ++ P.show isMAYZHolderAdmin ++ " - isFundAdmin: " ++ P.show isFundAdmin ++ " - isThisFundAdmin: " ++ P.show isThisFundAdmin ++ " - isMAYZHolder: " ++ P.show isMAYZHolder
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn $ "There are " ++ P.show (length $ T.ffppFundPABParams $ head $ T.pppFundFactoryPABParams protocolPABParams) ++ " Fund(s)"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    case fundPABParams' of
        Nothing -> do
            MonadIOClass.liftIO $ P.putStrLn "Please select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
        Just fundPABParams -> do
            MonadIOClass.liftIO $ P.putStrLn $ "Selected Fund: " ++ P.show (T.fppFundPolicy_CS fundPABParams) ++ ")"
            MonadIOClass.liftIO $ P.putStrLn "11: Invest Unit | 12: FT Price | 13: Token Prices"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "21 - Select Fund"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    Monad.when (DataMaybe.isJust fundPABParams') $ do
        MonadIOClass.liftIO $ P.putStrLn "31 - Deposit"
        MonadIOClass.liftIO $ P.putStrLn "32 - Withdraw"
        MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
        MonadIOClass.liftIO $ P.putStrLn "61 - Mint Fund Tokens"
        MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "0  - Return to Main Menu"
    MonadIOClass.liftIO $ P.putStrLn "99 - Exit"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Enter option:"
    option <- MonadIOClass.liftIO P.getLine
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    return option

--------------------------------------------------------------------------------2

pabMainLoop :: PABContracts.PABParamsInFundMenu
pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    let name = "Protocol: " ++ P.show (T.pppProtocolPolicyID_CS protocolPABParams)
    option <- menuEndPoints name isAdminMenu(walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
    case option of
        "11" -> do
            PABHelpers.pabShowInvestUnit isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "12" -> do
            PABHelpers.pabShowFTPriceADA isAdminMenu(walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "13" -> do
            PABHelpers.pabShowReIdxPriceADA isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown


        "21" ->
            pabSelectFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "31" ->
            pabFundDeposit isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "32" ->
            pabFundWithdraw isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

        "61" ->
            pabFundMintTokens isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown




        "81" -> do
            PABHelpers.pabBalances (Just walletNro, walletCount) (Just protocolPABParams)
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "82" -> do
            PABHelpers.pabUTxOAtWallet (Just walletNro, walletCount)
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "83" -> do
            PABHelpers.pabUTxOAtScript (Just protocolPABParams)
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "91" -> do
            PABHelpers.pabTimeAndSlot
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

        "0" -> do
            pabReturnToMainMenu (Just walletNro, walletCount) (Just protocolPABParams) pabShutdown
        "99" -> do
            MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Shutting Down PAB Simulator"
            PABHelpers.pabBalances (Just walletNro, walletCount) (Just protocolPABParams)
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            MonadIOClass.liftIO $ P.putStrLn "Exiting now..."
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid option"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabSelectFundParams :: PABContracts.PABParamsInFundMenu
pabSelectFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Select Fund"
    !fundPABParamsSelected' <- MonadIOClass.liftIO $ CLIHelpers.selectFromList (T.ffppFundPABParams $ head $ T.pppFundFactoryPABParams protocolPABParams)
    case fundPABParamsSelected' of
        Nothing -> do
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        Just fundPABParamsSelected -> do
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams (Just $ snd fundPABParamsSelected) pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabFundDeposit :: PABContracts.PABParamsInFundMenu
pabFundDeposit isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Make a Deposit"
    case fundPABParams' of
        (Just fundPABParams) -> do
            amount <-  MonadIOClass.liftIO $ HelpersCLI.getIntWithDefault "Deposit IU Amount" 1_000_000
            let
                contract = PABContracts.PABFundDeposit T.PABFundDepositParams{
                    T.pfdpProtocolPABParams = protocolPABParams
                    , T.pfdpFundPABParams = fundPABParams
                    , T.pfdpAmount   = amount
                }
            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabFundWithdraw :: PABContracts.PABParamsInFundMenu
pabFundWithdraw isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Make a Withdraw"
    case fundPABParams' of
        (Just fundPABParams) -> do
            amount <-  MonadIOClass.liftIO $ HelpersCLI.getIntWithDefault "Withdraw IU Amount" 1_000_000
            let
                contract = PABContracts.PABFundWithdraw T.PABFundWithdrawParams{
                    T.pfwpProtocolPABParams = protocolPABParams
                    , T.pfwpFundPABParams = fundPABParams
                    , T.pfwpAmount   = amount
                }
            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabFundUpdateOracle :: PABContracts.PABParamsInFundMenu
pabFundUpdateOracle isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    -- MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Fund Update Oracle"
    -- case fundPABParams' of
    --     (Just fundPABParams) -> do
    --         uiPriceADA <-  MonadIOClass.liftIO $ HelpersCLI.getIntWithDefault "IU(uFT) ADA Price" 10_000_000
    --         let
    --             contract = PABContracts.PABFundUpdateOracle T.PABFundUpdateOracleParams{
    --                 T.pfuopProtocolPABParams = protocolPABParams
    --                 , T.pfuopFundPABParams = fundPABParams
    --                 , T.pfuopPriceADA   = uiPriceADA
    --             }
    --         contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
    --         PABHelpers.waitContractAndKeyPress contractInstance
    --         pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
    --     _ -> do
    --         MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
    --         MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    --         PABHelpers.waitKeyPress
    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2


pabFundMintTokens :: PABContracts.PABParamsInFundMenu
pabFundMintTokens isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Mint Fund Tokens"
    case fundPABParams' of
        (Just fundPABParams) -> do
            mintAmount <- MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Amount to mint for each Token" 1_000
            let
                contract = PABContracts.PABMintFundTokens
                                T.PABMintFundTokensParams
                                    {
                                        T.pmftpProtocolPABParams   = protocolPABParams
                                        , T.pmftpFundPABParams       = fundPABParams
                                        , T.pmftpAmount         = mintAmount
                                    }
            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2
