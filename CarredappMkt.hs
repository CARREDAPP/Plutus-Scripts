{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module CarredappMkt where

import              Ledger              hiding (singleton)
import              Ledger.Typed.Scripts
import              Ledger.Value        as Value
import              Ledger.Ada
import qualified    PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)
import              Prelude             (Show (..))
import qualified    Prelude               as Haskell


data RecycleParam = RecycleParam
  {
      carreDappAddress :: !PubKeyHash
    , fees     :: !Integer
    }

data CarredappDatum = CarredappDatum
  {
    householdAddress     :: !PubKeyHash
  , companyAddress       :: !PubKeyHash
  , lovelaceAmount      :: !Integer
  , weight              :: !Integer
  , materialType         :: !Integer
    }
PlutusTx.makeLift ''RecycleParam
PlutusTx.unstableMakeIsData ''CarredappDatum

--Weight is larger than weight CarredappDatum > 50, then valid
--Recycling household offers quantity and type of recycling material
--Recycling company weights the quantity, certifies the type (quality?)
--So, we need to check the quantity before paying out

data RecycleAction = CancelR | CancelH | CancelP | Payout
  deriving Show

PlutusTx.makeIsDataIndexed ''RecycleAction [('CancelR, 0), ('CancelH, 1), ('CancelC, 2), ('Payout, 3)]

PlutusTx.makeLift ''RecycleAction

{-# INLINABLE RecycleDatum #-}
RecycleDatum :: Maybe Datum -> Maybe CarredappDatum
RecycleDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d
    
{-# INLINEABLE mkValidator #-}
mkValidator :: RecycleParam -> RecycleDatum -> RecycleAction -> ScriptContext -> Bool
mkValidator rp dat action ctx =
  case action of

    CancelR     ->  traceIfFalse "Cardano Recycling DApp manager has to sign"   signedByManager &&
                    traceIfFalse "Funds returned to both parties"               fundsToHousehold || fundsToCompany 
                    
    CancelH     ->  traceIfFalse "Only Household can Cancel This batch"         signedByHousehold  &&
                    traceIfFalse "Fees Paid"                                    recycleFeesPaid
                    
    
    CancelC     ->  traceIfFalse "Only Company can Cancel This batch"          signedByCompany  &&
                    traceIfFalse "Fees Paid"                                   recycleFeesPaid  

   Payout       ->  traceIfFalse "Only Company can Pay to Household"            signedByCompany &&
                    traceIfFalse "Household has to receive funds"               fundsToHousehold &&
                    traceIfFalse "Fees Paid"                                    recycleFeesPaid

 where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByManager :: Bool
    signedByManager = txSignedBy info $ carreDappAddress  rp

    signedByHousehold :: Bool
    signedByHousehold = txSignedBy info $ householdAddress  dat

    signedByCompany :: Bool
    signedByCompany = txSignedBy info $ providerAddress dat

-- Values Paid to Recycling Actors

    valueToHousehold :: Value
    valueToHousehold = valuePaidTo info $ householdAddress dat

    valueToRecylDapp :: Value
    valueToRecylDapp = valuePaidTo info $ carreDappAddress rp

    valueToCompany :: Value
    valueToCompany = valuePaidTo info $ companyAddress dat


    -- The value sent to Funder must be equal to what they deposited at the UTXO
    -- contributor must get tokenAmount bp of gimbals and lovelaceAmount bp...
    fundsToHousehold :: Bool
    fundsToHousehold = (getLovelace $ fromValue valueToHousehold) >= (lovelaceAmount dat)

    fundsToCompany :: Bool
    fundsToCompany = (getLovelace $ fromValue valueToCompany) >= (lovelaceAmount dat)

    recycleFeesPaid :: Bool
    recycleFeesPaid =(getLovelace $ fromValue valueToRecylDapp) >= (fees rp)

-- Done for the value transfer, now the constructors typing

data RecycleTypes

instance ValidatorTypes RecycleTypes where
    type DatumType RecycleTypes = RecycleDatum
    type RedeemerType RecycleTypes = RecycleAction

typedValidator :: RecycleParam -> TypedValidator RecycleTypes
typedValidator bp =
  mkTypedValidator @RecycleTypes
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode bp)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @RecycleDatum @RecycleAction

validator :: RecycleParam -> Validator
validator = validatorScript . typedValidator