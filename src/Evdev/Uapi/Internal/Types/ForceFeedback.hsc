{-# LANGUAGE ForeignFunctionInterface , RecordWildCards #-}
module Evdev.Uapi.Internal.Types.ForceFeedback where

import Control.Applicative ((<$>), (<*>))
import Data.Int            (Int16, Int32)
import Data.Word           (Word16, Word32)
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (id, length)

#include <linux/input.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Effect =
    Rumble { id        :: !Int16
           , direction :: !Word16
           , trigger   :: !Trigger
           , replay    :: !Replay
           , rumble    :: !RumbleEffect
           }
  | Periodic { id        :: !Int16
             , direction :: !Word16
             , trigger   :: !Trigger
             , replay    :: !Replay
             , periodic  :: !PeriodicEffect
             }
  | Constant { id        :: !Int16
             , direction :: !Word16
             , trigger   :: !Trigger
             , replay    :: !Replay
             , constant  :: !ConstantEffect
             }
  | Spring { id        :: !Int16
           , direction :: !Word16
           , trigger   :: !Trigger
           , replay    :: !Replay
           , spring    :: !(Ptr ConditionEffect)
           }
  | Friction { id        :: !Int16
             , direction :: !Word16
             , trigger   :: !Trigger
             , replay    :: !Replay
             , friction  :: !(Ptr ConditionEffect)
             }
  | Damper { id        :: !Int16
           , direction :: !Word16
           , trigger   :: !Trigger
           , replay    :: !Replay
           }
  | Inertia { id        :: !Int16
            , direction :: !Word16
            , trigger   :: !Trigger
            , replay    :: !Replay
            }
  | Ramp  { id        :: !Int16
          , direction :: !Word16
          , trigger   :: !Trigger
          , replay    :: !Replay
          , ramp      :: !RampEffect
          }

instance Storable Effect where
  sizeOf    _ = (#alignment struct ff_effect)
  alignment _ = (#size struct ff_effect)
  peek ptr    = do
    typ <- (#peek struct ff_effect, type)      ptr  :: IO Word16
    idt <- (#peek struct ff_effect, id)        ptr
    dir <- (#peek struct ff_effect, direction) ptr
    trg <- (#peek struct ff_effect, trigger)   ptr
    rpl <- (#peek struct ff_effect, replay)    ptr
    let peekU :: Storable a => IO a
        peekU = (#peek struct ff_effect, u) ptr
    case typ of
      (#const FF_RUMBLE)   -> Rumble   idt dir trg rpl <$> peekU
      (#const FF_PERIODIC) -> Periodic idt dir trg rpl <$> peekU
      (#const FF_CONSTANT) -> Constant idt dir trg rpl <$> peekU
      (#const FF_SPRING)   -> Spring   idt dir trg rpl <$> peekU
      (#const FF_FRICTION) -> Friction idt dir trg rpl <$> peekU
      (#const FF_RAMP    ) -> Ramp     idt dir trg rpl <$> peekU
      (#const FF_DAMPER)   -> return (Damper   idt dir trg rpl)
      (#const FF_INERTIA)  -> return (Inertia  idt dir trg rpl)
      _                    -> error $ "Unknown event type: " ++ show typ
  poke ptr ef = do
      (#poke struct ff_effect, id)        ptr (id        ef)
      (#poke struct ff_effect, direction) ptr (direction ef)
      (#poke struct ff_effect, trigger)   ptr (trigger   ef)
      (#poke struct ff_effect, replay )   ptr (replay    ef)
      let pokeType :: Word16 -> IO ()
          pokeType t = (#poke struct ff_effect, type) ptr t
          pokeU :: Storable a => a -> IO ()
          pokeU u = (#poke struct ff_effect, u) ptr u
          pokeEf :: Storable a => Word16 -> a -> IO ()
          pokeEf t u = pokeType t >> pokeU u
      case ef of
        Rumble {..}   -> pokeEf (#const FF_RUMBLE) rumble
        Periodic {..} -> pokeEf (#const FF_PERIODIC) periodic
        Constant {..} -> pokeEf (#const FF_CONSTANT) constant
        Spring {..}   -> pokeEf (#const FF_SPRING) spring
        Friction {..} -> pokeEf (#const FF_FRICTION) friction
        Damper {}     -> pokeType (#const FF_DAMPER)
        Inertia {}    -> pokeType (#const FF_INERTIA)
        Ramp {..}     -> pokeEf (#const FF_RAMP) ramp

data Replay =
  Replay {
    length :: !Word16
  , delay  :: !Word16
  } deriving (Eq, Show)

instance Storable Replay where
  sizeOf _    = (#size struct ff_replay)
  alignment _ = (#alignment struct ff_replay)
  peek ptr    =
    Replay
      <$> (#peek struct ff_replay, length) ptr
      <*> (#peek struct ff_replay, delay)  ptr
  poke ptr (Replay {..}) = do
      (#poke struct ff_replay, length) ptr length
      (#poke struct ff_replay, delay)  ptr delay

data Trigger =
  Trigger {
    button   :: !Word16
  , interval :: !Word16
  } deriving (Eq, Show)

instance Storable Trigger where
  sizeOf _    = (#size struct ff_trigger)
  alignment _ = (#alignment struct ff_trigger)
  peek ptr    =
    Trigger
      <$> (#peek struct ff_trigger, button)   ptr
      <*> (#peek struct ff_trigger, interval) ptr
  poke ptr (Trigger {..}) = do
      (#poke struct ff_trigger, button)   ptr button
      (#poke struct ff_trigger, interval) ptr interval

data Envelope =
  Envelope {
    attackLength :: !Word16
  , attackLevel  :: !Word16
  , fadeLength   :: !Word16
  , fadeLevel    :: !Word16
  } deriving (Eq, Show)

instance Storable Envelope where
  sizeOf _    = (#size struct ff_envelope)
  alignment _ = (#alignment struct ff_envelope)
  peek ptr    =
    Envelope
      <$> (#peek struct ff_envelope, attack_length) ptr
      <*> (#peek struct ff_envelope, attack_level)  ptr
      <*> (#peek struct ff_envelope, fade_length)   ptr
      <*> (#peek struct ff_envelope, fade_level)    ptr
  poke ptr (Envelope {..}) = do
      (#poke struct ff_envelope, attack_length) ptr attackLength
      (#poke struct ff_envelope, attack_level)  ptr attackLevel
      (#poke struct ff_envelope, fade_length)   ptr fadeLength
      (#poke struct ff_envelope, fade_level)    ptr fadeLevel

data ConstantEffect =
  ConstantEffect {
    level                  :: !Int16
  , constantEffectEnvelope :: !Envelope
  } deriving (Eq, Show)

instance Storable ConstantEffect where
  sizeOf _    = (#size struct ff_constant_effect)
  alignment _ = (#alignment struct ff_constant_effect)
  peek ptr    =
    ConstantEffect
      <$> (#peek struct ff_constant_effect, level)    ptr
      <*> (#peek struct ff_constant_effect, envelope) ptr
  poke ptr (ConstantEffect {..}) = do
    (#poke struct ff_constant_effect, level)    ptr level
    (#poke struct ff_constant_effect, envelope) ptr constantEffectEnvelope

data RampEffect =
  RampEffect {
    startLevel           :: !Int16
  , endLevel             :: !Int16
  , rampEffectEnvelope   :: !Envelope
  } deriving (Eq, Show)

instance Storable RampEffect where
  sizeOf _    = (#size struct ff_ramp_effect)
  alignment _ = (#alignment struct ff_ramp_effect)
  peek ptr    =
    RampEffect
      <$> (#peek struct ff_ramp_effect, start_level) ptr
      <*> (#peek struct ff_ramp_effect, end_level)   ptr
      <*> (#peek struct ff_ramp_effect, envelope)    ptr
  poke ptr (RampEffect {..}) = do
      (#poke struct ff_ramp_effect, start_level) ptr startLevel
      (#poke struct ff_ramp_effect, end_level)   ptr endLevel
      (#poke struct ff_ramp_effect, envelope)    ptr rampEffectEnvelope

data ConditionEffect =
  ConditionEffect {
    rightSaturation :: !Word16
  , leftSaturation  :: !Word16
  , rightCoeff      :: !Int16
  , leftCoeff       :: !Int16
  , deadband        :: !Word16
  , center          :: !Int16
  } deriving (Eq, Show)

instance Storable ConditionEffect where
  sizeOf _    = (#size struct ff_condition_effect)
  alignment _ = (#alignment struct ff_condition_effect)
  peek ptr    =
    ConditionEffect
      <$> (#peek struct ff_condition_effect, right_saturation) ptr
      <*> (#peek struct ff_condition_effect, left_saturation)  ptr
      <*> (#peek struct ff_condition_effect, right_coeff)      ptr
      <*> (#peek struct ff_condition_effect, left_coeff)       ptr
      <*> (#peek struct ff_condition_effect, deadband)         ptr
      <*> (#peek struct ff_condition_effect, center)           ptr
  poke ptr (ConditionEffect {..}) = do
      (#poke struct ff_condition_effect, right_saturation) ptr rightSaturation
      (#poke struct ff_condition_effect, left_saturation)  ptr leftSaturation
      (#poke struct ff_condition_effect, right_coeff)      ptr rightCoeff
      (#poke struct ff_condition_effect, left_coeff)       ptr leftCoeff
      (#poke struct ff_condition_effect, deadband)         ptr deadband
      (#poke struct ff_condition_effect, center)           ptr center

data PeriodicEffect =
  PeriodicEffect {
    waveform               :: !Waveform
  , period                 :: !Word16
  , magnitude              :: !Int16
  , offset                 :: !Int16
  , phase                  :: !Word16
  , periodicEffectEnvelope :: !Envelope
  , customLen              :: !Word32
  , customData             :: !(Ptr Int16)
  } deriving (Eq, Show)

instance Storable PeriodicEffect where
  sizeOf _    = (#size struct ff_periodic_effect)
  alignment _ = (#alignment struct ff_periodic_effect)
  peek ptr    = do
    _waveform <- (#peek struct ff_periodic_effect, waveform) ptr :: IO Word16
    PeriodicEffect (Waveform _waveform)
      <$> (#peek struct ff_periodic_effect, period)      ptr
      <*> (#peek struct ff_periodic_effect, magnitude)   ptr
      <*> (#peek struct ff_periodic_effect, offset)      ptr
      <*> (#peek struct ff_periodic_effect, phase)       ptr
      <*> (#peek struct ff_periodic_effect, envelope)    ptr
      <*> (#peek struct ff_periodic_effect, custom_len)  ptr
      <*> (#peek struct ff_periodic_effect, custom_data) ptr
  poke ptr (PeriodicEffect {..}) = do
      (#poke struct ff_periodic_effect, waveform)    ptr (unWaveform waveform)
      (#poke struct ff_periodic_effect, period)      ptr period
      (#poke struct ff_periodic_effect, magnitude)   ptr magnitude
      (#poke struct ff_periodic_effect, offset)      ptr offset
      (#poke struct ff_periodic_effect, phase)       ptr phase
      (#poke struct ff_periodic_effect, envelope)    ptr periodicEffectEnvelope
      (#poke struct ff_periodic_effect, custom_len)  ptr customLen
      (#poke struct ff_periodic_effect, custom_data) ptr customData

data RumbleEffect =
  RumbleEffect {
    strongMagnitude :: !Word16
  , weakMagnitude   :: !Word16
  } deriving (Eq, Show)

instance Storable RumbleEffect where
  sizeOf _    = (#size struct ff_rumble_effect)
  alignment _ = (#alignment struct ff_rumble_effect)
  peek ptr    =
    RumbleEffect
      <$> (#peek struct ff_rumble_effect, strong_magnitude) ptr
      <*> (#peek struct ff_rumble_effect, weak_magnitude)   ptr
  poke ptr (RumbleEffect {..}) = do
      (#poke struct ff_rumble_effect, strong_magnitude) ptr strongMagnitude
      (#poke struct ff_rumble_effect, weak_magnitude)   ptr weakMagnitude

newtype EffectType = EffectType { unEffectType :: Word16 } deriving Eq
#{enum EffectType, EffectType,
  FF_RUMBLE,
  FF_PERIODIC,
  FF_CONSTANT,
  FF_SPRING,
  FF_FRICTION,
  FF_DAMPER,
  FF_INERTIA,
  FF_RAMP,
  FF_EFFECT_MIN,
  FF_EFFECT_MAX }

newtype Waveform = Waveform { unWaveform :: Word16 } deriving (Eq, Show)
#{enum Waveform, Waveform,
  FF_SQUARE,
  FF_TRIANGLE,
  FF_SINE,
  FF_SAW_UP,
  FF_SAW_DOWN,
  FF_CUSTOM,
  FF_WAVEFORM_MIN,
  FF_WAVEFORM_MAX }

newtype DeviceProperties = DeviceProperties Word16 deriving Eq
#{enum DeviceProperties, DeviceProperties,
  FF_GAIN,
  FF_AUTOCENTER,
  FF_MAX,
  FF_CNT }

newtype StatusCode = StatusCode { unStatusCode :: Int32 } deriving (Eq, Show)
#{enum StatusCode, StatusCode,
  FF_STATUS_STOPPED,
  FF_STATUS_PLAYING,
  FF_STATUS_MAX }

