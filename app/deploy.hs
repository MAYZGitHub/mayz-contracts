module Main where

import qualified Prelude            as P
import qualified Protocol.Deploy    as Deploy
import           System.Environment (getArgs)

--Modulo:

main :: P.IO ()
main = do
  [ uTxOutRefStr, tokenMAYZ_CS_Str, tokenMAYZ_TN_Str, tokenEmergencyAdminPolicy_CS_Str ] <- getArgs
  _ <- Deploy.deploy_Protocol_And_FundFactory_With_StringParams uTxOutRefStr tokenMAYZ_CS_Str tokenMAYZ_TN_Str tokenEmergencyAdminPolicy_CS_Str
  P.return ()

-- EMULADOR
-- cabal run deployProtocol "0000000000000000000000000000000000000000000000000000000000000000#0" "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06" "MAYZ" "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06"

-- TESTNET
-- cabal run deployProtocol "55bd76a19aa8e2e5ab40417a97e01dec4e72700439ce734b2900932aabd205af#2" "e0b33937400326885f7186e2725a84786266ec1eb06d397680233f80" "MAYZ" "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06"

-- tokenMAYZ_CS =  "e0b33937400326885f7186e2725a84786266ec1eb06d397680233f80" -- for TESTNET
-- tokenMAYZ_CS =  "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06" -- for EMULATOR
-- tokenMAYZ_CS =   "7288521b8663f07fd6dabe5bdb4a951f57bc24d58fd4b5e92f3ab301" -- mainnet

-- tokenMAYZ_TN = LedgerApiV2.TokenName "MAYZ"

-- tokenEmergencyAdminPolicy_CS =  "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06"
