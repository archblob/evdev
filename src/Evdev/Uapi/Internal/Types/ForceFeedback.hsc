{-# LANGUAGE ForeignFunctionInterface #-}
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
    case typ of
      (#const FF_RUMBLE)   -> Rumble   idt dir trg rpl <$> (#peek struct ff_effect, u) ptr
      (#const FF_PERIODIC) -> Periodic idt dir trg rpl <$> (#peek struct ff_effect, u) ptr
      (#const FF_CONSTANT) -> Constant idt dir trg rpl <$> (#peek struct ff_effect, u) ptr
      (#const FF_SPRING)   -> Spring   idt dir trg rpl <$> (#peek struct ff_effect, u) ptr
      (#const FF_FRICTION) -> Friction idt dir trg rpl <$> (#peek struct ff_effect, u) ptr
      (#const FF_RAMP    ) -> Ramp     idt dir trg rpl <$> (#peek struct ff_effect, u) ptr
      (#const FF_DAMPER)   -> return (Damper   idt dir trg rpl)
      (#const FF_INERTIA)  -> return (Inertia  idt dir trg rpl)
      _                    -> error $ "Unknown event type: " ++ show typ
  poke ptr ef = do
      (#poke struct ff_effect, id)        ptr (id        ef)
      (#poke struct ff_effect, direction) ptr (direction ef)
      (#poke struct ff_effect, trigger)   ptr (trigger   ef)
      (#poke struct ff_effect, replay )   ptr (replay    ef)
      case ef of
        Rumble {}   -> do
                      (#poke struct ff_effect, type)    ptr (unEffectType ffRumble)
                      (#poke struct ff_effect, u)       ptr (rumble ef)
        Periodic {} -> do
                      (#poke struct ff_effect, type)    ptr (unEffectType ffPeriodic)
                      (#poke struct ff_effect, u)       ptr (periodic ef)
        Constant {} -> do
                      (#poke struct ff_effect, type)    ptr (unEffectType ffConstant)
                      (#poke struct ff_effect, u)       ptr (constant ef)
        Spring {}   -> do
                      (#poke struct ff_effect, type)    ptr (unEffectType ffSpring)
                      (#poke struct ff_effect, u)       ptr (spring ef)
        Friction {} -> do
                      (#poke struct ff_effect, type)    ptr (unEffectType ffFriction)
                      (#poke struct ff_effect, u)       ptr (friction ef)
        Damper {}   -> (#poke struct ff_effect, type)   ptr (unEffectType ffDamper)
        Inertia {}  -> (#poke struct ff_effect, type)   ptr (unEffectType ffInertia)
        Ramp {}     -> do
                      (#poke struct ff_effect, type)    ptr (unEffectType ffRamp)
                      (#poke struct ff_effect, u)       ptr (ramp ef)

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
  poke ptr rp = do
      (#poke struct ff_replay, length) ptr (length rp)
      (#poke struct ff_replay, delay)  ptr (delay rp)

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
  poke ptr tr = do
      (#poke struct ff_trigger, button)   ptr (button tr)
      (#poke struct ff_trigger, interval) ptr (interval tr)

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
  poke ptr en = do
      (#poke struct ff_envelope, attack_length) ptr (attackLength en)
      (#poke struct ff_envelope, attack_level)  ptr (attackLevel en)
      (#poke struct ff_envelope, fade_length)   ptr (fadeLength en)
      (#poke struct ff_envelope, fade_level)    ptr (fadeLevel en)

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
  poke ptr ce = do
    (#poke struct ff_constant_effect, level)    ptr (level ce)
    (#poke struct ff_constant_effect, envelope) ptr (constantEffectEnvelope ce)

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
  poke ptr re = do
      (#poke struct ff_ramp_effect, start_level) ptr (startLevel re)
      (#poke struct ff_ramp_effect, end_level)   ptr (endLevel re)
      (#poke struct ff_ramp_effect, envelope)    ptr (rampEffectEnvelope re)

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
  poke ptr ce = do
      (#poke struct ff_condition_effect, right_saturation) ptr (rightSaturation ce)
      (#poke struct ff_condition_effect, left_saturation)  ptr (leftSaturation ce)
      (#poke struct ff_condition_effect, right_coeff)      ptr (rightCoeff ce)
      (#poke struct ff_condition_effect, left_coeff)       ptr (leftCoeff ce)
      (#poke struct ff_condition_effect, deadband)         ptr (deadband ce)
      (#poke struct ff_condition_effect, center)           ptr (center ce)

data PeriodicEffect =
  PeriodicEffect {
    waveform               :: Waveform
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
    PeriodicEffect (toWaveform _waveform)
      <$> (#peek struct ff_periodic_effect, period)      ptr
      <*> (#peek struct ff_periodic_effect, magnitude)   ptr
      <*> (#peek struct ff_periodic_effect, offset)      ptr
      <*> (#peek struct ff_periodic_effect, phase)       ptr
      <*> (#peek struct ff_periodic_effect, envelope)    ptr
      <*> (#peek struct ff_periodic_effect, custom_len)  ptr
      <*> (#peek struct ff_periodic_effect, custom_data) ptr
  poke ptr pe = do
      (#poke struct ff_periodic_effect, waveform)    ptr (fromWaveform (waveform pe))
      (#poke struct ff_periodic_effect, period)      ptr (period pe)
      (#poke struct ff_periodic_effect, magnitude)   ptr (magnitude pe)
      (#poke struct ff_periodic_effect, offset)      ptr (offset pe)
      (#poke struct ff_periodic_effect, phase)       ptr (phase pe)
      (#poke struct ff_periodic_effect, envelope)    ptr (periodicEffectEnvelope pe)
      (#poke struct ff_periodic_effect, custom_len)  ptr (customLen pe)
      (#poke struct ff_periodic_effect, custom_data) ptr (customData pe)

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
  poke ptr re = do
      (#poke struct ff_rumble_effect, strong_magnitude) ptr (strongMagnitude re)
      (#poke struct ff_rumble_effect, weak_magnitude)   ptr (weakMagnitude re)

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

data Waveform =
  Square | Triangle | Sine | SawUp | SawDown | Custom deriving (Eq, Show)

toWaveform :: Word16 -> Waveform
toWaveform wf =
  case wf of
    (#const FF_SQUARE)   -> Square
    (#const FF_TRIANGLE) -> Triangle
    (#const FF_SINE)     -> Sine
    (#const FF_SAW_UP)   -> SawUp
    (#const FF_SAW_DOWN) -> SawDown
    (#const FF_CUSTOM)   -> Custom
    _                    -> error $ "Unknown waveform type: " ++ show wf

fromWaveform :: Waveform -> Word16
fromWaveform wf =
  case wf of
    Square   -> (#const FF_SQUARE)
    Triangle -> (#const FF_TRIANGLE)
    Sine     -> (#const FF_SINE)
    SawUp    -> (#const FF_SAW_UP)
    SawDown  -> (#const FF_SAW_DOWN)
    Custom   -> (#const FF_CUSTOM)

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

