
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.Others.PABSimulator where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Control.Monad.IO.Class  as MonadIOClass (MonadIO (..))
import qualified Plutus.PAB.Simulator    as PABSimulator
import           PlutusTx.Prelude        hiding (unless)
import qualified Prelude                 as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.CLIHelpers      as CLIHelpers
import qualified Generic.OffChainHelpers as OffChainHelpers
import qualified Generic.PABHelpers      as PABHelpers
import qualified Protocol.PABContracts   as PABContracts
import qualified Protocol.PABHelpers     as PABHelpers
import qualified Protocol.PABTypes       as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

menuEndPoints :: P.String -> (Maybe Integer, Integer) -> Maybe T.ProtocolPABParams -> P.IO P.String
menuEndPoints name _ _ = do

    CLIHelpers.printTitle $ "TOOLS MENU - " ++ name
    P.putStrLn "61 - Mint FT"
    P.putStrLn "62 - Mint NFT"
    P.putStrLn "7  - Split UTxO at Wallet"
    P.putStrLn "--------------------------------"
    P.putStrLn "0  - Return to Main Menu"
    P.putStrLn "99 - Exit"
    P.putStrLn "--------------------------------"
    P.putStrLn "Enter option:"
    option <- P.getLine
    P.putStrLn "--------------------------------"
    return option

--------------------------------------------------------------------------------2

pabMainLoop :: PABContracts.PABParamsInOthersMenu
pabMainLoop (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown = do
    option <- MonadIOClass.liftIO $ menuEndPoints "PAB Simulator" (walletNro', walletCount) protocolPABParams'
    case option of
        "61" -> pabMintFT (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown
        "62" -> pabMintNFT (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown
        "7" -> pabSplitUtxO (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown

        "81" -> do
            PABHelpers.pabBalances (walletNro', walletCount) protocolPABParams'
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown
        "82" -> do
            PABHelpers.pabUTxOAtWallet (walletNro', walletCount)
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown
        "83" -> do
            PABHelpers.pabUTxOAtScript protocolPABParams'
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown
        "91" -> do
            PABHelpers.pabTimeAndSlot
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown

        "0" -> do
            pabReturnToMainMenu (walletNro', walletCount) protocolPABParams' pabShutdown
        "99" -> do
            MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Shutting Down PAB Simulator"
            PABHelpers.pabBalances (walletNro', walletCount) protocolPABParams'
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            MonadIOClass.liftIO $ P.putStrLn "Exiting now..."
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid option"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabMintFT :: PABContracts.PABParamsInOthersMenu
pabMintFT (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown =
    case walletNro' of
        Just walletNro -> do
            MonadIOClass.liftIO $ P.putStrLn "Mint FT:"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            policyNum <- MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Currency Symbol" 1
            MonadIOClass.liftIO $ P.putStrLn "TokenName (default=FT): "
            mintTokenNameBaseStr <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault "FT"
            let mintTokenNameBaseBBS = OffChainHelpers.stringToBuiltinByteString mintTokenNameBaseStr
            MonadIOClass.liftIO $ P.putStrLn "Do you want to mint different TokenNames using the given TokenName as a base (y/n - default=n)?"
            mintDiffTokenName <- MonadIOClass.liftIO $ CLIHelpers.getBoolWithDefault False
            contract <-
                if mintDiffTokenName
                    then do
                        mfpDiifTokenNameCount <- MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Different TokenNames" 5
                        mintAmount <- MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Amount to mint for each TokenName" 1
                        return $
                            PABContracts.PABMintFT
                                T.PABMintFTParams
                                    { T.pmfpPolicyNum = policyNum,
                                      T.pmfpTokenNameBase = mintTokenNameBaseBBS,
                                      T.pmfpDiifTokenNameCount = mfpDiifTokenNameCount,
                                      T.pmfpAmount = mintAmount
                                    }
                    else do
                        mintAmount <- MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Amount to mint" 1
                        return $
                            PABContracts.PABMintFT
                                T.PABMintFTParams
                                    { T.pmfpPolicyNum = policyNum,
                                      T.pmfpTokenNameBase = mintTokenNameBaseBBS,
                                      T.pmfpDiifTokenNameCount = 0,
                                      T.pmfpAmount = mintAmount
                                    }
            ----------------
            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabMainLoop (Just walletNro, walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Wallet"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
            pabMainLoop (Nothing, walletCount) Nothing pabReturnToMainMenu pabShutdown

------------------------------------------------------------------------------

pabMintNFT :: PABContracts.PABParamsInOthersMenu
pabMintNFT (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown =
    case walletNro' of
        Just walletNro -> do
            MonadIOClass.liftIO $ P.putStrLn "Mint NFT:"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            MonadIOClass.liftIO $ P.putStrLn "TokenName (default=NFT): "
            mintTokenNameBaseStr <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault "NFT"
            let mintTokenNameBaseBBS = OffChainHelpers.stringToBuiltinByteString mintTokenNameBaseStr
            MonadIOClass.liftIO $ P.putStrLn "Do you want to mint different TokenNames using the given TokenName as a base (y/n - default=n)?"
            mintDiffTokenName <- MonadIOClass.liftIO $ CLIHelpers.getBoolWithDefault False
            contract <-
                if mintDiffTokenName
                    then do
                        mfpDiifTokenNameCount <- MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Different TokenNames" 5
                        return $
                            PABContracts.PABMintNFT
                                T.PABMintNFTParams
                                    { T.pmnpTokenNameBase = mintTokenNameBaseBBS,
                                      T.pmnpDiifTokenNameCount = mfpDiifTokenNameCount,
                                      T.pmnpAmount = 1
                                    }
                    else do
                        return $
                            PABContracts.PABMintNFT
                                T.PABMintNFTParams
                                    { T.pmnpTokenNameBase = mintTokenNameBaseBBS,
                                      T.pmnpDiifTokenNameCount = 0,
                                      T.pmnpAmount = 1
                                    }
            ----------------
            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabMainLoop (Just walletNro, walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Wallet"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
            pabMainLoop (Nothing, walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabSplitUtxO :: PABContracts.PABParamsInOthersMenu
pabSplitUtxO (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown =
    case walletNro' of
        Just walletNro -> do
            MonadIOClass.liftIO $ P.putStrLn "Split UtxOs at Wallet:"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            -- let !ada_AC = LedgerValue.AssetClass (LedgerValue.adaSymbol, LedgerValue.adaToken)
            --     !minADA = OnChainHelpers.calculateMinADA 1 0 1 True
            -- !splitAmount <- MonadIOClass.liftIO $ CLIHelpers.getAmount "ADA (lovelace)" ada_AC minADA
            !splitAmount <- MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "ADA (lovelace)" 5_000_000
            let contract =
                    PABContracts.PABSplitUtxO
                        T.PABSplitUtxOParams
                            { T.psupSplitAmount = splitAmount
                            }
            ----------------
            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Wallet"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
            pabMainLoop (walletNro', walletCount) protocolPABParams' pabReturnToMainMenu pabShutdown

------------------------------------------------------------------------------
