

{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
module TestUtils.Constants where
------------------------------------------------------------------------------

-- Non-IOG imports

import           Prelude                           as P
import qualified Plutus.V2.Ledger.Api as LedgerApiV2

-- IOG imports

-- Project imports

--------------------------------------------------------------------------------


-- Define the testConte

maxMemory :: Integer
maxMemory = 14_000_000

maxCPU :: Integer
maxCPU = 10_000_000_000

maxTxSize :: Integer
maxTxSize = 16_384

showErrorOnPercentageOver :: Integer
showErrorOnPercentageOver = 80

------------------------------------------------------------------------------

sum_ANY_INVALID_NUMBER :: Integer
sum_ANY_INVALID_NUMBER = 3_600_000

sum_ONE_INVALID_NUMBER :: Integer
sum_ONE_INVALID_NUMBER = 1

sum_ONE_INVALID_DATE :: LedgerApiV2.POSIXTime
sum_ONE_INVALID_DATE = 1

------------------------------------------------------------------------------

swTrace :: Bool
swTraceRuleTree :: Bool
swTraceTxParamsGenererator :: Bool
#ifdef VERBOSE
swTrace = True
swTraceRuleTree = True
swTraceTxParamsGenererator = True
#else
swTrace = False
swTraceRuleTree = False
swTraceTxParamsGenererator = False
#endif

------------------------------------------------------------------------------
