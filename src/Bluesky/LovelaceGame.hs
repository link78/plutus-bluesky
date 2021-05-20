{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Bluesky.LovelaceGame (
    -- * LovelaceMachine parameters
      LovelaceMachine(..)
    , LGSchema
    , contributionScript
    , mkValidator
    , MachineActions(..)
    , collectionRange
    ) where

import           Control.Applicative      (Applicative (..))
import           Control.Monad            (void)
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)

import           Ledger                   (PubKeyHash, Slot, Validator, txId)
import qualified Ledger                   as Ledger
import qualified Ledger.Ada               as Ada
import qualified Ledger.Constraints       as Constraints
import           Ledger.Contexts          as V
import qualified Ledger.Interval          as Interval
import qualified Ledger.Scripts           as Scripts
import           Ledger.Slot              (SlotRange)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             (Value)
import qualified Ledger.Value             as Value
import           Plutus.Contract
import qualified Plutus.Contract.Typed.Tx as Typed
import           Plutus.Trace.Emulator    (ContractHandle, EmulatorTrace)
import qualified Plutus.Trace.Emulator    as Trace
import qualified PlutusTx                 as PlutusTx
import           PlutusTx.Prelude         hiding (Applicative (..), Semigroup (..), return, (<$>), (>>), (>>=))
import           Prelude                  (Semigroup (..))
import qualified Prelude                  as Haskell
import           Schema                   (ToArgument, ToSchema)
import           Wallet.Emulator          (Wallet (..))
import qualified Wallet.Emulator          as Emulator

{-
Lovelace Game is Plustus Smart Contract that enable users to play by sending only
1 ADA. This contract is based on randomness which mean that the random player will be the winner.
The winner will get 80% of the total fund and the 20% will use for maintenance fees.
Each cycle will last for 12960 slots or 3 days. After each cycle a winner will be selected and the fund will be sent to the user wallets.
Lovelace state will collect the 20% and the game restart again.
-}


data LovelaceMachine = LovelaceMachine 
    { deadline          :: Slot 
    -- deadline that the user can play
    , fundReceived      :: Value
    -- Total fund received
    , collectionDeadline :: Slot
    , machineOwner      :: PubKeyHash
    --Pubkey of the Lovelace Machine and will be used to retrieve fund
    }

PlutusTx.makeLift ''LovelaceMachine
-- MachineActions or redeemer are action taken during the process

data MachineActions =
    Collect 
    | Cancel -- cancel if the deadline has passed
    | Pay -- paid winner

PlutusTx.unstableMakeIsData ''MachineActions
PlutusTx.makeLift ''MachineActions

-- functions that wallet will perform
type LGSchema =
    BlockchainActions
        .\/ Endpoint "Collect fund" () -- MachineOwner collect 20%
        .\/ Endpoint "receive funding" Contribution -- send 1 ada to contribute to the game
        .\/ Endpoint "cancel-payment" () -- cancel any contribution after the deadline passed
        .\/ Endpoint "paid-winner" ()  -- payback 80% of total fund to seleceted winner

newtype Contribution = Contribution {
    contriValue      :: Value
} deriving stock (Haskell.Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)

mkLovelaceMachine :: Slot -> Value -> Slot -> Wallet -> LovelaceMachine
mkLovelaceMachine dl fund machineWallet =
    LovelaceMachine 
        { deadline = dl
        , fundReceived  = fund
        , collectionDeadline = collectdl
        , machineOwner  = pubKeyHash $ Emulator.walletPubKey machineWallet
        }


-- SlotRange during which the funds can be collected or refund

collectionRange :: LovelaceMachine -> SlotRange
collectionRange lgd =
    Interval.interval (deadline lgd) (collectionDeadline lgd)




data LovelaceFunding
instance Scripts.ScriptType LovelaceFunding where
    type instance RedeemerType LovelaceFunding = MachineActions
    type instance DatumType LovelaceFunding = PubKeyHash

scriptInstance :: LovelaceMachine -> Scripts.Scripts.ScriptInstance LovelaceFunding
scriptInstance = Scripts.validatorParam @LovelaceFunding
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where 
        wrap = Scripts.wrapValidator


{-# INLINABLE proposalExpired #-}
-- | Check to see if a 'Payment' has expired.
proposalExpired :: TxInfo -> LovelaceMachine -> Bool
proposalExpired TxInfo{txInfoValidRange} LovelaceMachine{deadline} =
    deadline `Interval.before` txInfoValidRange

{-# INLINABLE validCollection #-}
validCollection :: LovelaceMachine -> TxInfo -> Bool
validCollection machine txinfo =
    -- check to see that the Tx falls in the collection range of the game
    (collectionRange machine `Interval.contains` txInfoValidRange txinfo)
    -- Check that the transaction is signed by the machine owner
    $$ (txinfo `V.txSignedBy` machineOwner machine)

{-# INLINABLE validPaidback #-}
validPaidback :: LovelaceMachine -> TxInfo -> Bool
validPaidback machine txinfo =
    -- check to see that the Tx falls in the collection range of the game
    (collectionRange machine `Interval.contains` txInfoValidRange txinfo)
    -- Check that the transaction is signed by the machine owner
    $$ (txinfo `V.txSignedBy` machineOwner machine)
{-
How i need the total fund first?

-}
{-# INLINABLE mkValidator #-}
mkValidator :: LovelaceMachine -> PubKeyHash -> MachineActions -> ScriptContext -> Bool
mkValidator m con act ScriptContext{scriptContextTxInfo} = case act of
    -- collect 20% and payback 80%
    Collect -> validCollection m scriptContextTxInfo 
    Pay -> validPaidback m con scriptContextTxInfo


























































































