{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Recycle.CarredappCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger

import Plutus.V1.Ledger.Api (Data (B, Constr, I, List, Map), ToData, toData)


-- import Carredapp Validator Script 
import Recycle.CarredappMkt

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (I n) = ScriptDataNumber n
dataToScriptData (B b) = ScriptDataBytes b
dataToScriptData (Map xs) = ScriptDataMap [(dataToScriptData k, dataToScriptData v) | (k, v) <- xs]
dataToScriptData (List xs) = ScriptDataList $ fmap dataToScriptData xs

writeJson :: ToData a => FilePath -> a -> IO ()
writeJson file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- writeValidatorScript :: IO (Either (FileError ()) ())
-- writeValidatorScript allows us to parameterized the Recycling script

writeRecycleEscrowScript :: IO (Either (FileError ()) ())
writeRecycleEscrowScript = writeValidator "src/output/recycle-v1.plutus" $ Recycle.CarredappMkt.validator $ RecycleParam
    {
      carreDappAddress = "9c68b6455daa791d56ba1a5c33fbc9e30c92041947a74eb82589cbe7"
    , fees     = 3000000
    }