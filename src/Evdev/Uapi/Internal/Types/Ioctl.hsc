{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}
module Evdev.Uapi.Internal.Types.Ioctl where

-- Types used in ioctl calls

import Control.Applicative ((<$>), (<*>))
import Data.Int            (Int32)
import Data.Word           (Word, Word8, Word16, Word32)
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (minimum, maximum, product)

#include <linux/input.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data InputID =
  InputID {
    bustype :: !Word16
  , vendor  :: !Word16
  , product :: !Word16
  , version :: !Word16
  } deriving (Eq, Show)

instance Storable InputID where
  sizeOf _    = (#size struct input_id)
  alignment _ = (#alignment struct input_id)
  peek ptr    =
    InputID
      <$> (#peek struct input_id, bustype) ptr
      <*> (#peek struct input_id, vendor)  ptr
      <*> (#peek struct input_id, product) ptr
      <*> (#peek struct input_id, version) ptr
  poke ptr (InputID {..}) = do
      (#poke struct input_id, bustype) ptr bustype
      (#poke struct input_id, vendor)  ptr vendor
      (#poke struct input_id, product) ptr product
      (#poke struct input_id, version) ptr version

data InputKeymapEntry =
  InputKeymapEntry {
    flags    :: !Word8
  , len      :: !Word8
  , index    :: !Word16
  , keycode  :: !Word32
  , scancode :: !(Ptr Word8)
  }

instance Storable InputKeymapEntry where
  sizeOf _    = (#size struct input_keymap_entry)
  alignment _ = (#alignment struct input_keymap_entry)
  peek ptr    =
    InputKeymapEntry
      <$> (#peek struct input_keymap_entry, flags)    ptr
      <*> (#peek struct input_keymap_entry, len)      ptr
      <*> (#peek struct input_keymap_entry, index)    ptr
      <*> (#peek struct input_keymap_entry, keycode)  ptr
      <*> (#peek struct input_keymap_entry, scancode) ptr
  poke ptr (InputKeymapEntry {..}) = do
      (#poke struct input_keymap_entry, flags)    ptr flags
      (#poke struct input_keymap_entry, len)      ptr len
      (#poke struct input_keymap_entry, index)    ptr index
      (#poke struct input_keymap_entry, keycode)  ptr keycode
      (#poke struct input_keymap_entry, scancode) ptr scancode

data InputAbsInfo =
  InputAbsInfo {
    value      :: !Int32
  , minimum    :: !Int32
  , maximum    :: !Int32
  , fuzz       :: !Int32
  , flat       :: !Int32
  , resolution :: !Int32
  } deriving (Eq, Show)

instance Storable InputAbsInfo where
  sizeOf _    = (#size struct input_absinfo)
  alignment _ = (#alignment struct input_absinfo)
  peek ptr    =
    InputAbsInfo
      <$> (#peek struct input_absinfo, value)      ptr
      <*> (#peek struct input_absinfo, minimum)    ptr
      <*> (#peek struct input_absinfo, maximum)    ptr
      <*> (#peek struct input_absinfo, fuzz)       ptr
      <*> (#peek struct input_absinfo, flat)       ptr
      <*> (#peek struct input_absinfo, resolution) ptr
  poke ptr (InputAbsInfo {..}) = do
      (#poke struct input_absinfo, value)      ptr value
      (#poke struct input_absinfo, minimum)    ptr minimum
      (#poke struct input_absinfo, maximum)    ptr maximum
      (#poke struct input_absinfo, fuzz)       ptr fuzz
      (#poke struct input_absinfo, flat)       ptr flat
      (#poke struct input_absinfo, resolution) ptr resolution

newtype DeviceID = DeviceID { unDeviceId :: Word8 } deriving Eq
#{enum DeviceID, DeviceID,
  ID_BUS,
  ID_VENDOR,
  ID_PRODUCT,
  ID_VERSION }

newtype BusID = BusID { unBusId :: Word8 } deriving Eq
#{enum BusID, BusID,
  BUS_PCI,
  BUS_ISAPNP,
  BUS_USB,
  BUS_HIL,
  BUS_BLUETOOTH,
  BUS_VIRTUAL,
  BUS_ISA,
  BUS_I8042,
  BUS_XTKBD,
  BUS_RS232,
  BUS_GAMEPORT,
  BUS_PARPORT,
  BUS_AMIGA,
  BUS_ADB,
  BUS_I2C,
  BUS_HOST,
  BUS_GSC,
  BUS_ATARI,
  BUS_SPI }

newtype InputProperty = InputProperty { unInputProperty :: Word8 } deriving Eq
#{enum InputProperty, InputProperty,
  INPUT_PROP_POINTER,
  INPUT_PROP_DIRECT,
  INPUT_PROP_BUTTONPAD,
  INPUT_PROP_SEMI_MT,
  INPUT_PROP_MAX,
  INPUT_PROP_CNT }

newtype RepCode = RepCode { unRepCode :: Word16 } deriving (Eq, Show)
#{enum RepCode, RepCode,
  REP_DELAY,
  REP_PERIOD,
  REP_MAX,
  REP_CNT }

