--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Helpers.Validator where

------------------------------------------------------------------------------------------
-- External Imports
------------------------------------------------------------------------------------------

import qualified GHC.Generics            as Generics
import qualified Ledger
import qualified Ledger.Address          as LedgerAddress
import qualified Ledger.Crypto           as Crypto
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import qualified PlutusTx
import           Prelude
import           PlutusTx.Prelude        (check, traceIfFalse, verifyEd25519Signature)

------------------------------------------------------------------------------------------

-- SIMPLE VALIDATOR
data CustomDatum
    = MkCustomDatum
          { signedMessage :: LedgerApiV2.BuiltinByteString
          , signature     :: Crypto.Signature
          }
    deriving (Generics.Generic, Show)

PlutusTx.unstableMakeIsData ''CustomDatum

newtype CustomRedeemer
    = MkCustomRedeemer { publickey :: LedgerAddress.PaymentPubKey }
    deriving (Generics.Generic, Show)

PlutusTx.unstableMakeIsData ''CustomRedeemer

{-# INLINEABLE mkValidator #-}
mkValidator :: CustomDatum -> CustomRedeemer -> LedgerApiV2.ScriptContext -> Bool
mkValidator (MkCustomDatum signedMsg signature') (MkCustomRedeemer pkh) _ =
    traceIfFalse "wrong public key" chekSignatureOfMsgWithPkh
    where
        ------------------
        checkSignature :: Ledger.PaymentPubKey
            -- ^ The public key of the signatory
            -> LedgerApiV2.BuiltinByteString
            -- ^ The message
            -> Ledger.Signature
            -- ^ The signed message
            -> Bool
        checkSignature !paymentPubKey !signedMsgBBS !signature'' =
                let
                    !pubKey= Ledger.unPaymentPubKey paymentPubKey
                    !lb = Ledger.getPubKey pubKey
                    !bbs = LedgerApiV2.getLedgerBytes lb
                    !sig = Ledger.getSignature signature''
                in  verifyEd25519Signature bbs signedMsgBBS sig
        ------------------
        chekSignatureOfMsgWithPkh :: Bool
        chekSignatureOfMsgWithPkh = checkSignature pkh signedMsg signature'

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: LedgerApiV2.Validator
validator = LedgerApiV2.mkValidatorScript $$(PlutusTx.compile [||mkWrappedValidator||])

{-# INLINEABLE wrapValidator #-}
wrapValidator ::
    (LedgerApiV2.UnsafeFromData datum, LedgerApiV2.UnsafeFromData redeemer) =>
    (datum -> redeemer -> LedgerApiV2.ScriptContext -> Bool) ->
    PlutusTx.BuiltinData ->
    PlutusTx.BuiltinData ->
    PlutusTx.BuiltinData ->
    ()
wrapValidator validator' datum redeemer ctx =
    check $
        validator'
            (LedgerApiV2.unsafeFromBuiltinData datum)
            (LedgerApiV2.unsafeFromBuiltinData redeemer)
            (LedgerApiV2.unsafeFromBuiltinData ctx)


------------------------------------------------------------------------------------------