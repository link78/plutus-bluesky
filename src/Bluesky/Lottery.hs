{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
-- | A multisig contract written as a state machine.
module Bluesky.Lottery(
    -- $multisig
      Params(..)
    , Payment(..)
    , State
    , mkValidator
    , scriptInstance
    , MultiSigError(..)
    , MultiSigSchema
    , contract
    ) where

import           Control.Lens                 (makeClassyPrisms)
import           Control.Monad                (forever)
import           Data.Aeson                   (FromJSON, ToJSON)
import           GHC.Generics                 (Generic)
import           Ledger                       (PubKeyHash, Slot, pubKeyHash)
import           Ledger.Constraints           (TxConstraints)
import qualified Ledger.Constraints           as Constraints
import           Ledger.Contexts              (ScriptContext (..), TxInfo (..))
import qualified Ledger.Contexts              as Validation
import qualified Ledger.Interval              as Interval
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value                 (Value)
import qualified Ledger.Value                 as Value

import           Plutus.Contract
import           Plutus.Contract.StateMachine (AsSMContractError, State (..), StateMachine (..), TransitionResult (..),
                                               Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx                     as PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..))


{-
-- Blue sky or Lovelace Game smart contract or No gain No lost works a join account of X people
-- User play:
-- 1 by sending Lovelace
-- 2 contract save all Tx address
-- 3 select a random address as a winner and send 80% of X amount locked
-- requiring the consent of one payment
-- In he smart contract, signatories are represented by PubKeyHash
-- Blue sky expects Y signature on a single Tx and require an off-chain communication.
-- This will enable users to propose a Tx and attache their PubKeyHash(signatures)
-- to the proposal over a period og time using separate Tx.
-- All contract state is skept on-chain so there is no need for off-chain communication
-}


-- Payment or Play data under Bluesky schema
data Payment = Payment
    { paymentAmount       :: Value
    , paymentSig          :: PubKeyHash
    , paymentDeadline     :: Slot
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
 
instance Eq Payment where
    {-# INLINEABLE (==) #-}
    (Payment vl pk sl) == (Payment vl' pk' sl') = vl == vl' && pk == pk' && sl == sl'


data pragmas = Params
    { lgSignatories     :: [PubKeyHash]
    , lgRequiredSigs    :: Integer -- How many sig are require buy a Play
    }

-- State of LovelaceGame contract
data LGState =
    Holding
    -- Money is locked when the amount reaches X amount. 
    | CollectingSignatures Payment [PubKeyHash]
    -- Payment and Sig
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq LGState where
    {-# INLINEABLE (==) #-}
    Holding == Holding = True 
    (CollectingSignatures play pks) == (CollectingSignatures play' pks') = 
        play == play' && pks == pks'
    _ == _ = False

data Input =
    ProposePayment Payment
    -- Make a play: payment can be made as soon as enough sig collected
    | AddSignature PubKeyHash
    -- adding a sig to the Tx
    | Cancel -- cancel any payment if the deadline passed or amount reaches X amount
    | Pay -- Make a payment
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- PlayError

data SigsError =
    LGContractError ContractError
    | LGStateMachineError SM.SMContractError
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
makeClassyPrisms ''SigsError

instance AsContractError SigsError where
    _ContractError = _LGContractError

instance AsSMContractError SigsError where
    _SMContractError = _LGStateMachineError

-- LovelaveGame schema

type LovelaceGameSchema =
    BlockchainActions 
        .\/ Endpoint "play-game" Payment
        .\/ Endpoint "add-signature" ()
        .\/ Endpoint "cancel-play" ()
        .\/ Endpoint "play" ()
        .\/ Endpoint "locked" ()

{-# INLINEABLE isSignature #-}
-- check to see if the signature (PubKeyHash) is one of the signatories of the LG contract
isSignature :: PubKeyHash -> Params -> Bool
isSignature pkh (Params sigs _) = any (\pkh' -> pkh == pkh') sigs


{-# INLINEABLE containsPK #-}
-- check to see if the pub keys contains a given key
containsPK :: PubKeyHash -> [PubKeyHash] -> Bool
containsPK pk = any (\pk' -> pk == pk')

{-# INLINEABLE isValidProposal #-}
isValidProposal :: Value -> Payment -> Bool
isValidProposal vl (Payment amt _ _) = amt `Value.leq` vl

{-# INLINEABLE proposalExpired #-}
-- check to see if the deadline has passed
proposalExpired :: TxInfo -> Payment -> Bool
proposalExpired TxInfo{txInfoValidRange} Payment{paymentDeadline} = paymentDeadline `Interval.before` txInfoValidRange


{-# INLINEABLE proposalAccepted #-}
proposalAccepted :: Params -> [PubKeyHash] -> Bool
-- check to see if enought signatories have signed to each Tx or proposed payment
proposalAccepted (Params signatories numReq) pks =
    let numSigned = length (filter (\pk -> containsPK pk pks) signatories)
    in numSigned >= numReq






















































































































































