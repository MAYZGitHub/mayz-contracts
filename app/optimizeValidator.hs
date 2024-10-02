{-# LANGUAGE BlockArguments #-}
module Main where

import qualified Generic.DeployHelpers as DeployHelpers
import qualified Prelude               as P
import           System.Environment    (getArgs)

--Modulo:

main :: P.IO ()
main = do
    args <- getArgs
    case args of
      (contractFilePath : optimizedContractSaveFileName : scriptParams) -> do
          _ <- DeployHelpers.optimizeValidator contractFilePath optimizedContractSaveFileName scriptParams
          P.return ()
      _ ->
          P.putStrLn "Invalid arguments"
