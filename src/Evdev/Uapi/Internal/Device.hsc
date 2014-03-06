{-# LANGUAGE ForeignFunctionInterface #-}
module Evdev.Uapi.Internal.Device (
    eviocGetVersion
  , eviocGetID
  , eviocGetName
  , eviocGetPhys
  , eviocGetUniq
  , eviocGetProp
  , eviocGetKeyCode
  , eviocGetRep
  , eviocGetEffects
  ) where

import Evdev.Uapi.Internal.Types.Ioctl

import Control.Applicative ((<$>), (<*>))
import Foreign
import Foreign.C.Error       (throwErrnoIfMinus1_)
import Foreign.C.String      (CString, peekCString, peekCAString)
import Foreign.C.Types
import System.Posix.Types    (Fd)

#include <linux/input.h>
#include <sys/ioctl.h>

foreign import ccall "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

ioctl :: Storable a => Fd -> CInt -> Ptr a -> IO ()
ioctl fd c ptr =
  throwErrnoIfMinus1_ "ioctl" $ c_ioctl (fromIntegral fd) c (castPtr ptr)

eviocGetRep :: Fd -> IO (Word, Word)
eviocGetRep fd =
  let getRepSettings :: Ptr Word -> IO (Word, Word)
      getRepSettings ptr =
              ioctl fd (#const EVIOCGREP) ptr >>
                (,) <$> peekElemOff ptr 0
                    <*> peekElemOff ptr 1
  in alloca getRepSettings

eviocGetEffects :: Fd -> IO CInt
eviocGetEffects fd =
  let getEffects :: Ptr CInt -> IO CInt
      getEffects ptr = ioctl fd (#const EVIOCGEFFECTS) ptr >> peek ptr
  in alloca getEffects

-- | Get driver version.
eviocGetVersion :: Fd -> IO CInt
eviocGetVersion fd =
  let getVersion :: Ptr CInt -> IO CInt
      getVersion ptr = ioctl fd (#const EVIOCGVERSION) ptr >> peek ptr
  in alloca getVersion

-- | Get device ID.
eviocGetID :: Fd -> IO InputID
eviocGetID fd =
  let getID :: Ptr InputID -> IO InputID
      getID ptr = ioctl fd (#const EVIOCGID) ptr >> peek ptr
  in alloca getID

-- | Get device name.
-- TODO: Something is wrong with the encoding.
eviocGetName :: Fd -> IO String
eviocGetName fd =
  let getName :: CString -> IO String
      getName ptr = ioctl fd (#const EVIOCGNAME(0x100)) ptr >> peekCAString ptr
  in alloca getName

-- | Get device physical location.
eviocGetPhys :: Fd -> IO String
eviocGetPhys fd =
  let getPhys :: CString -> IO String
      getPhys ptr = ioctl fd (#const EVIOCGPHYS(0x100)) ptr >> peekCAString ptr
  in alloca getPhys

-- | Get keycode.
eviocGetKeyCode :: Fd -> IO InputKeymapEntry
eviocGetKeyCode fd =
  let getKC :: Ptr InputKeymapEntry -> IO InputKeymapEntry
      getKC ptr = ioctl fd (#const EVIOCGKEYCODE) ptr >> peek ptr
  in alloca getKC

-- | Get unique identifier
eviocGetUniq :: Fd -> IO String
eviocGetUniq fd =
  let getUQ :: CString -> IO String
      getUQ ptr = ioctl fd (#const EVIOCGUNIQ(0x100)) ptr >> peekCString ptr
  in alloca getUQ

-- | Get device properties
eviocGetProp :: Fd -> IO String
eviocGetProp fd =
  let getProp :: CString -> IO String
      getProp ptr = ioctl fd (#const EVIOCGPROP(0x100)) ptr >> peekCString ptr
  in alloca getProp
