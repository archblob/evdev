{-# LANGUAGE ForeignFunctionInterface #-}
module Evdev.Uapi.Internal.Device (
    eviocGetVersion
  , eviocGetID
  , eviocGetName
  ) where

import Evdev.Uapi.Internal.Types

import Data.ByteString       (ByteString, packCString)
import Foreign
import Foreign.C.Error       (throwErrnoIfMinus1_)
import Foreign.C.String      (CString, peekCAString)
import Foreign.C.Types
import System.Posix.Types    (Fd)

#include <linux/input.h>
#include <sys/ioctl.h>

foreign import ccall "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

ioctl :: Storable a => Fd -> CInt -> Ptr a -> IO ()
ioctl fd c ptr =
  throwErrnoIfMinus1_ "ioctl" $ c_ioctl (fromIntegral fd) c (castPtr ptr)

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
      getName ptr = ioctl fd (#const EVIOCGNAME(0xff)) ptr >> peekCAString ptr
  in alloca getName

-- | Get device physical location.
eviocGetPhys :: Fd -> IO ByteString
eviocGetPhys fd =
  let getPhys :: CString -> IO ByteString
      getPhys ptr = ioctl fd (#const EVIOCGNAME(0xff)) ptr >> packCString ptr
  in alloca getPhys

-- | Get keycode.
eviocGetKeyCode :: Fd -> IO InputKeymapEntry
eviocGetKeyCode fd =
  let getKC :: Ptr InputKeymapEntry -> IO InputKeymapEntry
      getKC ptr = ioctl fd (#const EVIOCGKEYCODE) ptr >> peek ptr
  in alloca getKC
