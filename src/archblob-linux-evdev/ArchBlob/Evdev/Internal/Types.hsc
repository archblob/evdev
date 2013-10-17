{-# LANGUAGE ForeignFunctionInterface #-}
module ArchBlob.Evdev.Internal.Types where

import Control.Applicative ((<$>), (<*>))
import Data.Int            (Int32, Int16)
import Data.UnixTime       (UnixTime(..))
import Data.Word           (Word16)
import Foreign.Storable

#include <linux/input.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable UnixTime where
  sizeOf _    = (#size struct timeval)
  alignment _ = (#alignment struct timeval)
  peek ptr    =
    UnixTime
      <$> (#peek struct timeval, tv_sec)  ptr
      <*> (#peek struct timeval, tv_usec) ptr
  poke ptr ut = do
      (#poke struct timeval, tv_sec)  ptr (utSeconds ut)
      (#poke struct timeval, tv_usec) ptr (utMicroSeconds ut)

data InputEvent = InputEvent {
      evTime  :: UnixTime
    , evType  :: !Word16
    , evCode  :: !Word16
    , evValue :: !Int32
    } deriving (Show, Eq)

instance Storable InputEvent where
  sizeOf _    = (#size struct input_event)
  alignment _ = (#alignment struct input_event)
  peek ptr    =
    InputEvent
      <$> (#peek struct input_event, time)  ptr
      <*> (#peek struct input_event, type)  ptr
      <*> (#peek struct input_event, code)  ptr
      <*> (#peek struct input_event, value) ptr
  poke ptr inpEv = do
      (#poke struct input_event, time)  ptr (evTime  inpEv)
      (#poke struct input_event, type)  ptr (evType  inpEv)
      (#poke struct input_event, code)  ptr (evCode  inpEv)
      (#poke struct input_event, value) ptr (evValue inpEv)

data InputID =
  InputID {
    inpBustype :: !Word16
  , inpVendor  :: !Word16
  , inpProduct :: !Word16
  , inpVersion :: !Word16
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
  poke ptr inpID = do
      (#poke struct input_id, bustype) ptr (inpBustype inpID)
      (#poke struct input_id, vendor)  ptr (inpVendor  inpID)
      (#poke struct input_id, product) ptr (inpProduct inpID)
      (#poke struct input_id, version) ptr (inpVersion inpID)

data InputAbsInfo =
  InputAbsInfo {
    absiValue      :: !Int32
  , absiMinimum    :: !Int32
  , absiMaximum    :: !Int32
  , absiFuzz       :: !Int32
  , absiFlat       :: !Int32
  , absiResolution :: !Int32
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
  poke ptr absInf = do
      (#poke struct input_absinfo, value)      ptr (absiValue      absInf)
      (#poke struct input_absinfo, minimum)    ptr (absiMinimum    absInf)
      (#poke struct input_absinfo, maximum)    ptr (absiMaximum    absInf)
      (#poke struct input_absinfo, fuzz)       ptr (absiFuzz       absInf)
      (#poke struct input_absinfo, flat)       ptr (absiFlat       absInf)
      (#poke struct input_absinfo, resolution) ptr (absiResolution absInf)

data FFReplay =
  FFReplay {
    ffReplayLength :: !Word16
  , ffReplayDelay  :: !Word16
  } deriving (Eq, Show)

instance Storable FFReplay where
  sizeOf _    = (#size struct ff_replay)
  alignment _ = (#alignment struct ff_replay)
  peek ptr    =
    FFReplay
      <$> (#peek struct ff_replay, length) ptr
      <*> (#peek struct ff_replay, delay)  ptr
  poke ptr rp = do
      (#poke struct ff_replay, length) ptr (ffReplayLength rp)
      (#poke struct ff_replay, delay)  ptr (ffReplayDelay rp)

data FFTrigger =
  FFTrigger {
    ffTriggerButton   :: !Word16
  , ffTriggerInterval :: !Word16
  } deriving (Eq, Show)

instance Storable FFTrigger where
  sizeOf _    = (#size struct ff_trigger)
  alignment _ = (#alignment struct ff_trigger)
  peek ptr    =
    FFTrigger
      <$> (#peek struct ff_trigger, button)   ptr
      <*> (#peek struct ff_trigger, interval) ptr
  poke ptr tr = do
      (#poke struct ff_trigger, button)   ptr (ffTriggerButton tr)
      (#poke struct ff_trigger, interval) ptr (ffTriggerInterval tr)

data FFEnvelope =
  FFEnvelope {
    ffAttackLength :: !Word16
  , ffAttackLevel  :: !Word16
  , ffFadeLength   :: !Word16
  , ffFadeLevel    :: !Word16
  } deriving (Eq, Show)

instance Storable FFEnvelope where
  sizeOf _    = (#size struct ff_envelope)
  alignment _ = (#alignment struct ff_envelope)
  peek ptr    =
    FFEnvelope
      <$> (#peek struct ff_envelope, attack_length) ptr
      <*> (#peek struct ff_envelope, attack_level)  ptr
      <*> (#peek struct ff_envelope, fade_length)   ptr
      <*> (#peek struct ff_envelope, fade_level)    ptr
  poke ptr en = do
      (#poke struct ff_envelope, attack_length) ptr (ffAttackLength en)
      (#poke struct ff_envelope, attack_level)  ptr (ffAttackLevel en)
      (#poke struct ff_envelope, fade_length)   ptr (ffFadeLength en)
      (#poke struct ff_envelope, fade_level)    ptr (ffFadeLevel en)

data FFConstantEffect =
  FFConstantEffect {
    ffConstantEffectLevel    :: !Int16
  , ffConstantEffectEnvelope :: FFEnvelope
  } deriving (Eq, Show)

instance Storable FFConstantEffect where
  sizeOf _    = (#size struct ff_constant_effect)
  alignment _ = (#alignment struct ff_constant_effect)
  peek ptr    =
    FFConstantEffect
      <$> (#peek struct ff_constant_effect, level)    ptr
      <*> (#peek struct ff_constant_effect, envelope) ptr
  poke ptr ce = do
    (#poke struct ff_constant_effect, level)    ptr (ffConstantEffectLevel ce)
    (#poke struct ff_constant_effect, envelope) ptr (ffConstantEffectEnvelope ce)

data FFRampEffect =
  FFRampEffect {
    ffRampEffectStartLevel :: !Int16
  , ffRampEffectEndLevel   :: !Int16
  , ffRampEffectEnvelope   :: FFEnvelope
  } deriving (Eq, Show)

instance Storable FFRampEffect where
  sizeOf _    = (#size struct ff_ramp_effect)
  alignment _ = (#alignment struct ff_ramp_effect)
  peek ptr    =
    FFRampEffect
      <$> (#peek struct ff_ramp_effect, start_level) ptr
      <*> (#peek struct ff_ramp_effect, end_level)   ptr
      <*> (#peek struct ff_ramp_effect, envelope)    ptr
  poke ptr re = do
      (#poke struct ff_ramp_effect, start_level) ptr (ffRampEffectStartLevel re)
      (#poke struct ff_ramp_effect, end_level)   ptr (ffRampEffectEndLevel re)
      (#poke struct ff_ramp_effect, envelope)    ptr (ffRampEffectEnvelope re)

data FFConditionEffect =
  FFConditionEffect {
    ffConditionEffectRightSaturation :: !Word16
  , ffConditionEffectLeftSaturation  :: !Word16
  , ffConditionEffectRightCoeff      :: !Int16
  , ffConditionEffectLeftCoeff       :: !Int16
  , ffConditionEffectDeadband        :: !Word16
  , ffConditionEffectCenter          :: !Int16
  } deriving (Eq, Show)

instance Storable FFConditionEffect where
  sizeOf _    = (#size struct ff_condition_effect)
  alignment _ = (#alignment struct ff_condition_effect)
  peek ptr    =
    FFConditionEffect
      <$> (#peek struct ff_condition_effect, right_saturation) ptr
      <*> (#peek struct ff_condition_effect, left_saturation)  ptr
      <*> (#peek struct ff_condition_effect, right_coeff)      ptr
      <*> (#peek struct ff_condition_effect, left_coeff)       ptr
      <*> (#peek struct ff_condition_effect, deadband)         ptr
      <*> (#peek struct ff_condition_effect, center)           ptr
  poke ptr ce = do
      (#poke struct ff_condition_effect, right_saturation) ptr (ffConditionEffectRightSaturation ce)
      (#poke struct ff_condition_effect, left_saturation)  ptr (ffConditionEffectLeftSaturation ce)
      (#poke struct ff_condition_effect, right_coeff)      ptr (ffConditionEffectRightCoeff ce)
      (#poke struct ff_condition_effect, left_coeff)       ptr (ffConditionEffectLeftCoeff ce)
      (#poke struct ff_condition_effect, deadband)         ptr (ffConditionEffectDeadband ce)
      (#poke struct ff_condition_effect, center)           ptr (ffConditionEffectCenter ce)

data FFRumbleEffect =
  FFRumbleEffect {
    ffRumbleEffectStrongMagnitude :: !Word16
  , ffRumbleEffectWeakMagnitude :: !Word16
  } deriving (Eq, Show)

instance Storable FFRumbleEffect where
  sizeOf _    = (#size struct ff_rumble_effect)
  alignment _ = (#alignment struct ff_rumble_effect)
  peek ptr    =
    FFRumbleEffect
      <$> (#peek struct ff_rumble_effect, strong_magnitude) ptr
      <*> (#peek struct ff_rumble_effect, weak_magnitude)   ptr
  poke ptr re = do
      (#poke struct ff_rumble_effect, strong_magnitude) ptr (ffRumbleEffectStrongMagnitude re)
      (#poke struct ff_rumble_effect, weak_magnitude)   ptr (ffRumbleEffectWeakMagnitude re) 

newtype EventType = EventType Word16 deriving (Eq, Show)
#{enum EventType, EventType
 , ev_syn       = EV_SYN
 , ev_key       = EV_KEY
 , ev_rel       = EV_REL
 , ev_abs       = EV_ABS
 , ev_msc       = EV_MSC
 , ev_sw        = EV_SW
 , ev_led       = EV_LED
 , ev_snd       = EV_SND
 , ev_rep       = EV_REP
 , ev_ff        = EV_FF
 , ev_pwr       = EV_PWR
 , ev_ff_status = EV_FF_STATUS
 }

newtype SyncType = SyncType Word16 deriving (Eq, Show)
#{enum SyncType, SyncType
 , syn_report    = SYN_REPORT
 , syn_config    = SYN_CONFIG
 , syn_mt_report = SYN_MT_REPORT
 , syn_dropped   = SYN_DROPPED
 }

newtype RelAxes = RelAxes Word16 deriving (Eq, Show)
#{enum RelAxes, RelAxes
 , rel_x      = REL_X
 , rel_y      = REL_Y
 , rel_z      = REL_Z
 , rel_rx     = REL_RX
 , rel_ry     = REL_RY
 , rel_rz     = REL_RZ
 , rel_hwheel = REL_HWHEEL
 , rel_dial   = REL_DIAL
 , rel_wheel  = REL_WHEEL
 , rel_misc   = REL_MISC
 }

newtype AbsAxes = AbsAxes Word16 deriving (Eq, Show)
#{enum AbsAxes, AbsAxes
 , abs_x              = ABS_X
 , abs_y              = ABS_Y
 , abs_z              = ABS_Z
 , abs_rx             = ABS_RX
 , abs_ry             = ABS_RY
 , abs_rz             = ABS_RZ
 , abs_throttle       = ABS_THROTTLE
 , abs_rudder         = ABS_RUDDER
 , abs_wheel          = ABS_WHEEL
 , abs_gas            = ABS_GAS
 , abs_brake          = ABS_BRAKE
 , abs_hat0x          = ABS_HAT0X
 , abs_hat0y          = ABS_HAT0Y
 , abs_hat1x          = ABS_HAT1X
 , abs_hat1y          = ABS_HAT1Y
 , abs_hat2x          = ABS_HAT2X
 , abs_hat2y          = ABS_HAT2Y
 , abs_hat3x          = ABS_HAT3X
 , abs_hat3y          = ABS_HAT3Y
 , abs_pressure       = ABS_PRESSURE
 , abs_distance       = ABS_DISTANCE
 , abs_tilt_x         = ABS_TILT_X
 , abs_tilt_y         = ABS_TILT_Y
 , abs_tool_width     = ABS_TOOL_WIDTH
 , abs_volume         = ABS_VOLUME
 , abs_misc           = ABS_MISC
 , abs_mt_slot        = ABS_MT_SLOT
 , abs_mt_touch_major = ABS_MT_TOUCH_MAJOR
 , abs_mt_touch_minor = ABS_MT_TOUCH_MINOR
 , abs_mt_width_major = ABS_MT_WIDTH_MAJOR
 , abs_mt_width_minor = ABS_MT_WIDTH_MINOR
 , abs_mt_orientation = ABS_MT_ORIENTATION
 , abs_mt_position_x  = ABS_MT_POSITION_X
 , abs_mt_position_y  = ABS_MT_POSITION_Y
 , abs_mt_tool_type   = ABS_MT_TOOL_TYPE
 , abs_mt_blob_id     = ABS_MT_BLOB_ID
 , abs_mt_tracking_id = ABS_MT_TRACKING_ID
 , abs_mt_pressure    = ABS_MT_PRESSURE
 , abs_mt_distance    = ABS_MT_DISTANCE
 , abs_mt_tool_x      = ABS_MT_TOOL_X
 , abs_mt_tool_y      = ABS_MT_TOOL_Y
 }

newtype SWType = SWType Word16 deriving (Eq, Show)
#{enum SWType, SWType
 , sw_lid                  = SW_LID
 , sw_tablet_mode          = SW_TABLET_MODE
 , sw_headphone_insert     = SW_HEADPHONE_INSERT
 , sw_rfkill_all           = SW_RFKILL_ALL
 , sw_radio                = SW_RFKILL_ALL
 , sw_microphone_insert    = SW_MICROPHONE_INSERT
 , sw_dock                 = SW_DOCK
 , sw_lineout_insert       = SW_LINEOUT_INSERT
 , sw_jack_physical_insert = SW_JACK_PHYSICAL_INSERT
 , sw_videoout_insert      = SW_VIDEOOUT_INSERT
 , sw_camera_lens_cover    = SW_CAMERA_LENS_COVER
 , sw_keypad_slide         = SW_KEYPAD_SLIDE
 , sw_front_proximity      = SW_FRONT_PROXIMITY
 , sw_rotate_lock          = SW_ROTATE_LOCK
 , sw_linein_insert        = SW_LINEIN_INSERT
 }

newtype MSCType = MSCType Word16 deriving (Eq, Show)
#{enum MSCType, MSCType
 , msc_serial    = MSC_SERIAL
 , msc_pulseled  = MSC_PULSELED
 , msc_gesture   = MSC_GESTURE
 , msc_raw       = MSC_RAW
 , msc_scan      = MSC_SCAN
 , msc_timestamp = MSC_TIMESTAMP
 }

newtype LEDType = LEDType Word16 deriving (Eq, Show)
#{enum LEDType, LEDType
 , led_numl     = LED_NUML
 , led_capsl    = LED_CAPSL
 , led_scrolll  = LED_SCROLLL
 , led_compose  = LED_COMPOSE
 , led_kana     = LED_KANA
 , led_sleep    = LED_SLEEP
 , led_suspend  = LED_SUSPEND
 , led_mute     = LED_MUTE
 , led_misc     = LED_MISC
 , led_mail     = LED_MAIL
 , led_charging = LED_CHARGING
 }

newtype Key = Key Word16 deriving (Eq, Show)
#{enum Key, Key
 , key_reserved         = KEY_RESERVED
 , key_esc              = KEY_ESC
 , key_1                = KEY_1
 , key_2                = KEY_2
 , key_3                = KEY_3
 , key_4                = KEY_4
 , key_5                = KEY_5
 , key_6                = KEY_6
 , key_7                = KEY_7
 , key_8                = KEY_8
 , key_9                = KEY_9
 , key_0                = KEY_0
 , key_minus            = KEY_MINUS
 , key_equal            = KEY_EQUAL
 , key_backspace        = KEY_BACKSPACE
 , key_tab              = KEY_TAB
 , key_q                = KEY_Q
 , key_w                = KEY_W
 , key_e                = KEY_E
 , key_r                = KEY_R
 , key_t                = KEY_T
 , key_y                = KEY_Y
 , key_u                = KEY_U
 , key_i                = KEY_I
 , key_o                = KEY_O
 , key_p                = KEY_P
 , key_leftbrace        = KEY_LEFTBRACE
 , key_rightbrace       = KEY_RIGHTBRACE
 , key_enter            = KEY_ENTER
 , key_leftctrl         = KEY_LEFTCTRL
 , key_a                = KEY_A
 , key_s                = KEY_S
 , key_d                = KEY_D
 , key_f                = KEY_F
 , key_g                = KEY_G
 , key_h                = KEY_H
 , key_j                = KEY_J
 , key_k                = KEY_K
 , key_l                = KEY_L
 , key_semicolon        = KEY_SEMICOLON
 , key_apostrophe       = KEY_APOSTROPHE
 , key_grave            = KEY_GRAVE
 , key_leftshift        = KEY_LEFTSHIFT
 , key_backslash        = KEY_BACKSLASH
 , key_z                = KEY_Z
 , key_x                = KEY_X
 , key_c                = KEY_C
 , key_v                = KEY_V
 , key_b                = KEY_B
 , key_n                = KEY_N
 , key_m                = KEY_M
 , key_comma            = KEY_COMMA
 , key_dot              = KEY_DOT
 , key_slash            = KEY_SLASH
 , key_rightshift       = KEY_RIGHTSHIFT
 , key_kpasterisk       = KEY_KPASTERISK
 , key_leftalt          = KEY_LEFTALT
 , key_space            = KEY_SPACE
 , key_capslock         = KEY_CAPSLOCK
 , key_f1               = KEY_F1
 , key_f2               = KEY_F2
 , key_f3               = KEY_F3
 , key_f4               = KEY_F4
 , key_f5               = KEY_F5
 , key_f6               = KEY_F6
 , key_f7               = KEY_F7
 , key_f8               = KEY_F8
 , key_f9               = KEY_F9
 , key_f10              = KEY_F10
 , key_numlock          = KEY_NUMLOCK
 , key_scrolllock       = KEY_SCROLLLOCK
 , key_kp7              = KEY_KP7
 , key_kp8              = KEY_KP8
 , key_kp9              = KEY_KP9
 , key_kpminus          = KEY_KPMINUS
 , key_kp4              = KEY_KP4
 , key_kp5              = KEY_KP5
 , key_kp6              = KEY_KP6
 , key_kpplus           = KEY_KPPLUS
 , key_kp1              = KEY_KP1
 , key_kp2              = KEY_KP2
 , key_kp3              = KEY_KP3
 , key_kp0              = KEY_KP0
 , key_kpdot            = KEY_KPDOT
 , key_zenkakuhankaku   = KEY_ZENKAKUHANKAKU
 , key_102nd            = KEY_102ND
 , key_f11              = KEY_F11
 , key_f12              = KEY_F12
 , key_ro               = KEY_RO
 , key_katakana         = KEY_KATAKANA
 , key_hiragana         = KEY_HIRAGANA
 , key_henkan           = KEY_HENKAN
 , key_katakanahiragana = KEY_KATAKANAHIRAGANA
 , key_muhenkan         = KEY_MUHENKAN
 , key_kpjpcomma        = KEY_KPJPCOMMA
 , key_kpenter          = KEY_KPENTER
 , key_rightctrl        = KEY_RIGHTCTRL
 , key_kpslash          = KEY_KPSLASH
 , key_sysrq            = KEY_SYSRQ
 , key_rightalt         = KEY_RIGHTALT
 , key_linefeed         = KEY_LINEFEED
 , key_home             = KEY_HOME
 , key_up               = KEY_UP
 , key_pageup           = KEY_PAGEUP
 , key_left             = KEY_LEFT
 , key_right            = KEY_RIGHT
 , key_end              = KEY_END
 , key_down             = KEY_DOWN
 , key_pagedown         = KEY_PAGEDOWN
 , key_insert           = KEY_INSERT
 , key_delete           = KEY_DELETE
 , key_macro            = KEY_MACRO
 , key_mute             = KEY_MUTE
 , key_volumedown       = KEY_VOLUMEDOWN
 , key_volumeup         = KEY_VOLUMEUP
 , key_power            = KEY_POWER
 , key_kpequal          = KEY_KPEQUAL
 , key_kpplusminus      = KEY_KPPLUSMINUS
 , key_pause            = KEY_PAUSE
 , key_scale            = KEY_SCALE
 , key_kpcomma          = KEY_KPCOMMA
 , key_hangeul          = KEY_HANGEUL
 , key_hanguel          = KEY_HANGUEL
 , key_hanja            = KEY_HANJA
 , key_yen              = KEY_YEN
 , key_leftmeta         = KEY_LEFTMETA
 , key_rightmeta        = KEY_RIGHTMETA
 , key_compose          = KEY_COMPOSE
 , key_stop             = KEY_STOP
 , key_again            = KEY_AGAIN
 , key_props            = KEY_PROPS
 , key_undo             = KEY_UNDO
 , key_front            = KEY_FRONT
 , key_copy             = KEY_COPY
 , key_open             = KEY_OPEN
 , key_paste            = KEY_PASTE
 , key_find             = KEY_FIND
 , key_cut              = KEY_CUT
 , key_help             = KEY_HELP
 , key_menu             = KEY_MENU
 , key_calc             = KEY_CALC
 , key_setup            = KEY_SETUP
 , key_sleep            = KEY_SLEEP
 , key_wakeup           = KEY_WAKEUP
 , key_file             = KEY_FILE
 , key_sendfile         = KEY_SENDFILE
 , key_deletefile       = KEY_DELETEFILE
 , key_xfer             = KEY_XFER
 , key_prog1            = KEY_PROG1
 , key_prog2            = KEY_PROG2
 , key_www              = KEY_WWW
 , key_msdos            = KEY_MSDOS
 , key_coffee           = KEY_COFFEE
 , key_screenlock       = KEY_SCREENLOCK
 , key_direction        = KEY_DIRECTION
 , key_cyclewindows     = KEY_CYCLEWINDOWS
 , key_mail             = KEY_MAIL
 , key_bookmarks        = KEY_BOOKMARKS
 , key_computer         = KEY_COMPUTER
 , key_back             = KEY_BACK
 , key_forward          = KEY_FORWARD
 , key_closecd          = KEY_CLOSECD
 , key_ejectcd          = KEY_EJECTCD
 , key_ejectclosecd     = KEY_EJECTCLOSECD
 , key_nextsong         = KEY_NEXTSONG
 , key_playpause        = KEY_PLAYPAUSE
 , key_previoussong     = KEY_PREVIOUSSONG
 , key_stopcd           = KEY_STOPCD
 , key_record           = KEY_RECORD
 , key_rewind           = KEY_REWIND
 , key_phone            = KEY_PHONE
 , key_iso              = KEY_ISO
 , key_config           = KEY_CONFIG
 , key_homepage         = KEY_HOMEPAGE
 , key_refresh          = KEY_REFRESH
 , key_exit             = KEY_EXIT
 , key_move             = KEY_MOVE
 , key_edit             = KEY_EDIT
 , key_scrollup         = KEY_SCROLLUP
 , key_scrolldown       = KEY_SCROLLDOWN
 , key_kpleftparen      = KEY_KPLEFTPAREN
 , key_kprightparen     = KEY_KPRIGHTPAREN
 , key_new              = KEY_NEW
 , key_redo             = KEY_REDO
 , key_f13              = KEY_F13
 , key_f14              = KEY_F14
 , key_f15              = KEY_F15
 , key_f16              = KEY_F16
 , key_f17              = KEY_F17
 , key_f18              = KEY_F18
 , key_f19              = KEY_F19
 , key_f20              = KEY_F20
 , key_f21              = KEY_F21
 , key_f22              = KEY_F22
 , key_f23              = KEY_F23
 , key_f24              = KEY_F24
 , key_playcd           = KEY_PLAYCD
 , key_pausecd          = KEY_PAUSECD
 , key_prog3            = KEY_PROG3
 , key_prog4            = KEY_PROG4
 , key_dashboard        = KEY_DASHBOARD
 , key_suspend          = KEY_SUSPEND
 , key_close            = KEY_CLOSE
 , key_play             = KEY_PLAY
 , key_fastforward      = KEY_FASTFORWARD
 , key_bassboost        = KEY_BASSBOOST
 , key_print            = KEY_PRINT
 , key_hp               = KEY_HP
 , key_camera           = KEY_CAMERA
 , key_sound            = KEY_SOUND
 , key_question         = KEY_QUESTION
 , key_email            = KEY_EMAIL
 , key_chat             = KEY_CHAT
 , key_search           = KEY_SEARCH
 , key_connect          = KEY_CONNECT
 , key_finance          = KEY_FINANCE
 , key_sport            = KEY_SPORT
 , key_shop             = KEY_SHOP
 , key_alterase         = KEY_ALTERASE
 , key_cancel           = KEY_CANCEL
 , key_brightnessdown   = KEY_BRIGHTNESSDOWN
 , key_brightnessup     = KEY_BRIGHTNESSUP
 , key_media            = KEY_MEDIA
 , key_switchvideomode  = KEY_SWITCHVIDEOMODE
 , key_kbdillumtoggle   = KEY_KBDILLUMTOGGLE
 , key_kbdillumdown     = KEY_KBDILLUMDOWN
 , key_kbdillumup       = KEY_KBDILLUMUP
 , key_send             = KEY_SEND
 , key_reply            = KEY_REPLY
 , key_forwardmail      = KEY_FORWARDMAIL
 , key_save             = KEY_SAVE
 , key_documents        = KEY_DOCUMENTS
 , key_battery          = KEY_BATTERY
 , key_bluetooth        = KEY_BLUETOOTH
 , key_wlan             = KEY_WLAN
 , key_uwb              = KEY_UWB
 , key_unknown          = KEY_UNKNOWN
 , key_video_next       = KEY_VIDEO_NEXT
 , key_video_prev       = KEY_VIDEO_PREV
 , key_brightness_cycle = KEY_BRIGHTNESS_CYCLE
 , key_brightness_zero  = KEY_BRIGHTNESS_ZERO
 , key_display_off      = KEY_DISPLAY_OFF
 , key_wimax            = KEY_WIMAX
 , key_rfkill           = KEY_RFKILL
 , key_micmute          = KEY_MICMUTE
 , btn_misc             = BTN_MISC
 , btn_0                = BTN_0
 , btn_1                = BTN_1
 , btn_2                = BTN_2
 , btn_3                = BTN_3
 , btn_4                = BTN_4
 , btn_5                = BTN_5
 , btn_6                = BTN_6
 , btn_7                = BTN_7
 , btn_8                = BTN_8
 , btn_9                = BTN_9
 , btn_mouse            = BTN_MOUSE
 , btn_left             = BTN_LEFT
 , btn_right            = BTN_RIGHT
 , btn_middle           = BTN_MIDDLE
 , btn_side             = BTN_SIDE
 , btn_extra            = BTN_EXTRA
 , btn_forward          = BTN_FORWARD
 , btn_back             = BTN_BACK
 , btn_task             = BTN_TASK
 , btn_joystick         = BTN_JOYSTICK
 , btn_trigger          = BTN_TRIGGER
 , btn_thumb            = BTN_THUMB
 , btn_thumb2           = BTN_THUMB2
 , btn_top              = BTN_TOP
 , btn_top2             = BTN_TOP2
 , btn_pinkie           = BTN_PINKIE
 , btn_base             = BTN_BASE
 , btn_base2            = BTN_BASE2
 , btn_base3            = BTN_BASE3
 , btn_base4            = BTN_BASE4
 , btn_base5            = BTN_BASE5
 , btn_base6            = BTN_BASE6
 , btn_dead             = BTN_DEAD
 , btn_gamepad          = BTN_GAMEPAD
 , btn_a                = BTN_A
 , btn_b                = BTN_B
 , btn_c                = BTN_C
 , btn_x                = BTN_X
 , btn_y                = BTN_Y
 , btn_z                = BTN_Z
 , btn_tl               = BTN_TL
 , btn_tr               = BTN_TR
 , btn_tl2              = BTN_TL2
 , btn_tr2              = BTN_TR2
 , btn_select           = BTN_SELECT
 , btn_start            = BTN_START
 , btn_mode             = BTN_MODE
 , btn_thumbl           = BTN_THUMBL
 , btn_thumbr           = BTN_THUMBR
 , btn_digi             = BTN_DIGI
 , btn_tool_pen         = BTN_TOOL_PEN
 , btn_tool_rubber      = BTN_TOOL_RUBBER
 , btn_tool_brush       = BTN_TOOL_BRUSH
 , btn_tool_pencil      = BTN_TOOL_PENCIL
 , btn_tool_airbrush    = BTN_TOOL_AIRBRUSH
 , btn_tool_finger      = BTN_TOOL_FINGER
 , btn_tool_mouse       = BTN_TOOL_MOUSE
 , btn_tool_lens        = BTN_TOOL_LENS
 , btn_tool_quinttap    = BTN_TOOL_QUINTTAP
 , btn_touch            = BTN_TOUCH
 , btn_stylus           = BTN_STYLUS
 , btn_stylus2          = BTN_STYLUS2
 , btn_tool_doubletap   = BTN_TOOL_DOUBLETAP
 , btn_tool_tripletap   = BTN_TOOL_TRIPLETAP
 , btn_tool_quadtap     = BTN_TOOL_QUADTAP
 , btn_wheel            = BTN_WHEEL
 , btn_gear_down        = BTN_GEAR_DOWN
 , btn_gear_up          = BTN_GEAR_UP
 , key_ok               = KEY_OK
 , key_select           = KEY_SELECT
 , key_goto             = KEY_GOTO
 , key_clear            = KEY_CLEAR
 , key_power2           = KEY_POWER2
 , key_option           = KEY_OPTION
 , key_info             = KEY_INFO
 , key_time             = KEY_TIME
 , key_vendor           = KEY_VENDOR
 , key_archive          = KEY_ARCHIVE
 , key_program          = KEY_PROGRAM
 , key_channel          = KEY_CHANNEL
 , key_favorites        = KEY_FAVORITES
 , key_epg              = KEY_EPG
 , key_pvr              = KEY_PVR
 , key_mhp              = KEY_MHP
 , key_language         = KEY_LANGUAGE
 , key_title            = KEY_TITLE
 , key_subtitle         = KEY_SUBTITLE
 , key_angle            = KEY_ANGLE
 , key_zoom             = KEY_ZOOM
 , key_mode             = KEY_MODE
 , key_keyboard         = KEY_KEYBOARD
 , key_screen           = KEY_SCREEN
 , key_pc               = KEY_PC
 , key_tv               = KEY_TV
 , key_tv2              = KEY_TV2
 , key_vcr              = KEY_VCR
 , key_vcr2             = KEY_VCR2
 , key_sat              = KEY_SAT
 , key_sat2             = KEY_SAT2
 , key_cd               = KEY_CD
 , key_tape             = KEY_TAPE
 , key_radio            = KEY_RADIO
 , key_tuner            = KEY_TUNER
 , key_player           = KEY_PLAYER
 , key_text             = KEY_TEXT
 , key_dvd              = KEY_DVD
 , key_aux              = KEY_AUX
 , key_mp3              = KEY_MP3
 , key_audio            = KEY_AUDIO
 , key_video            = KEY_VIDEO
 , key_directory        = KEY_DIRECTORY
 , key_list             = KEY_LIST
 , key_memo             = KEY_MEMO
 , key_calendar         = KEY_CALENDAR
 , key_red              = KEY_RED
 , key_green            = KEY_GREEN
 , key_yellow           = KEY_YELLOW
 , key_blue             = KEY_BLUE
 , key_channelup        = KEY_CHANNELUP
 , key_channeldown      = KEY_CHANNELDOWN
 , key_first            = KEY_FIRST
 , key_last             = KEY_LAST
 , key_ab               = KEY_AB
 , key_next             = KEY_NEXT
 , key_restart          = KEY_RESTART
 , key_slow             = KEY_SLOW
 , key_shuffle          = KEY_SHUFFLE
 , key_break            = KEY_BREAK
 , key_previous         = KEY_PREVIOUS
 , key_digits           = KEY_DIGITS
 , key_teen             = KEY_TEEN
 , key_twen             = KEY_TWEN
 , key_videophone       = KEY_VIDEOPHONE
 , key_games            = KEY_GAMES
 , key_zoomin           = KEY_ZOOMIN
 , key_zoomout          = KEY_ZOOMOUT
 , key_zoomreset        = KEY_ZOOMRESET
 , key_wordprocessor    = KEY_WORDPROCESSOR
 , key_editor           = KEY_EDITOR
 , key_spreadsheet      = KEY_SPREADSHEET
 , key_graphicseditor   = KEY_GRAPHICSEDITOR
 , key_presentation     = KEY_PRESENTATION
 , key_database         = KEY_DATABASE
 , key_news             = KEY_NEWS
 , key_voicemail        = KEY_VOICEMAIL
 , key_addressbook      = KEY_ADDRESSBOOK
 , key_messenger        = KEY_MESSENGER
 , key_displaytoggle    = KEY_DISPLAYTOGGLE
 , key_spellcheck       = KEY_SPELLCHECK
 , key_logoff           = KEY_LOGOFF
 , key_dollar           = KEY_DOLLAR
 , key_euro             = KEY_EURO
 , key_frameback        = KEY_FRAMEBACK
 , key_frameforward     = KEY_FRAMEFORWARD
 , key_context_menu     = KEY_CONTEXT_MENU
 , key_media_repeat     = KEY_MEDIA_REPEAT
 , key_10channelsup     = KEY_10CHANNELSUP
 , key_10channelsdown   = KEY_10CHANNELSDOWN
 , key_images           = KEY_IMAGES
 , key_del_eol          = KEY_DEL_EOL
 , key_del_eos          = KEY_DEL_EOS
 , key_ins_line         = KEY_INS_LINE
 , key_del_line         = KEY_DEL_LINE
 , key_fn               = KEY_FN
 , key_fn_esc           = KEY_FN_ESC
 , key_fn_f1            = KEY_FN_F1
 , key_fn_f2            = KEY_FN_F2
 , key_fn_f3            = KEY_FN_F3
 , key_fn_f4            = KEY_FN_F4
 , key_fn_f5            = KEY_FN_F5
 , key_fn_f6            = KEY_FN_F6
 , key_fn_f7            = KEY_FN_F7
 , key_fn_f8            = KEY_FN_F8
 , key_fn_f9            = KEY_FN_F9
 , key_fn_f10           = KEY_FN_F10
 , key_fn_f11           = KEY_FN_F11
 , key_fn_f12           = KEY_FN_F12
 , key_fn_1             = KEY_FN_1
 , key_fn_2             = KEY_FN_2
 , key_fn_d             = KEY_FN_D
 , key_fn_e             = KEY_FN_E
 , key_fn_f             = KEY_FN_F
 , key_fn_s             = KEY_FN_S
 , key_fn_b             = KEY_FN_B
 , key_brl_dot1         = KEY_BRL_DOT1
 , key_brl_dot2         = KEY_BRL_DOT2
 , key_brl_dot3         = KEY_BRL_DOT3
 , key_brl_dot4         = KEY_BRL_DOT4
 , key_brl_dot5         = KEY_BRL_DOT5
 , key_brl_dot6         = KEY_BRL_DOT6
 , key_brl_dot7         = KEY_BRL_DOT7
 , key_brl_dot8         = KEY_BRL_DOT8
 , key_brl_dot9         = KEY_BRL_DOT9
 , key_brl_dot10        = KEY_BRL_DOT10
 , key_numeric_0        = KEY_NUMERIC_0
 , key_numeric_1        = KEY_NUMERIC_1
 , key_numeric_2        = KEY_NUMERIC_2
 , key_numeric_3        = KEY_NUMERIC_3
 , key_numeric_4        = KEY_NUMERIC_4
 , key_numeric_5        = KEY_NUMERIC_5
 , key_numeric_6        = KEY_NUMERIC_6
 , key_numeric_7        = KEY_NUMERIC_7
 , key_numeric_8        = KEY_NUMERIC_8
 , key_numeric_9        = KEY_NUMERIC_9
 , key_numeric_star     = KEY_NUMERIC_STAR
 , key_numeric_pound    = KEY_NUMERIC_POUND
 , key_camera_focus     = KEY_CAMERA_FOCUS
 , key_wps_button       = KEY_WPS_BUTTON
 , key_touchpad_toggle  = KEY_TOUCHPAD_TOGGLE
 , key_touchpad_on      = KEY_TOUCHPAD_ON
 , key_touchpad_off     = KEY_TOUCHPAD_OFF
 , key_camera_zoomin    = KEY_CAMERA_ZOOMIN
 , key_camera_zoomout   = KEY_CAMERA_ZOOMOUT
 , key_camera_up        = KEY_CAMERA_UP
 , key_camera_down      = KEY_CAMERA_DOWN
 , key_camera_left      = KEY_CAMERA_LEFT
 , key_camera_right     = KEY_CAMERA_RIGHT
 , btn_trigger_happy    = BTN_TRIGGER_HAPPY
 , btn_trigger_happy1   = BTN_TRIGGER_HAPPY1
 , btn_trigger_happy2   = BTN_TRIGGER_HAPPY2
 , btn_trigger_happy3   = BTN_TRIGGER_HAPPY3
 , btn_trigger_happy4   = BTN_TRIGGER_HAPPY4
 , btn_trigger_happy5   = BTN_TRIGGER_HAPPY5
 , btn_trigger_happy6   = BTN_TRIGGER_HAPPY6
 , btn_trigger_happy7   = BTN_TRIGGER_HAPPY7
 , btn_trigger_happy8   = BTN_TRIGGER_HAPPY8
 , btn_trigger_happy9   = BTN_TRIGGER_HAPPY9
 , btn_trigger_happy10  = BTN_TRIGGER_HAPPY10
 , btn_trigger_happy11  = BTN_TRIGGER_HAPPY11
 , btn_trigger_happy12  = BTN_TRIGGER_HAPPY12
 , btn_trigger_happy13  = BTN_TRIGGER_HAPPY13
 , btn_trigger_happy14  = BTN_TRIGGER_HAPPY14
 , btn_trigger_happy15  = BTN_TRIGGER_HAPPY15
 , btn_trigger_happy16  = BTN_TRIGGER_HAPPY16
 , btn_trigger_happy17  = BTN_TRIGGER_HAPPY17
 , btn_trigger_happy18  = BTN_TRIGGER_HAPPY18
 , btn_trigger_happy19  = BTN_TRIGGER_HAPPY19
 , btn_trigger_happy20  = BTN_TRIGGER_HAPPY20
 , btn_trigger_happy21  = BTN_TRIGGER_HAPPY21
 , btn_trigger_happy22  = BTN_TRIGGER_HAPPY22
 , btn_trigger_happy23  = BTN_TRIGGER_HAPPY23
 , btn_trigger_happy24  = BTN_TRIGGER_HAPPY24
 , btn_trigger_happy25  = BTN_TRIGGER_HAPPY25
 , btn_trigger_happy26  = BTN_TRIGGER_HAPPY26
 , btn_trigger_happy27  = BTN_TRIGGER_HAPPY27
 , btn_trigger_happy28  = BTN_TRIGGER_HAPPY28
 , btn_trigger_happy29  = BTN_TRIGGER_HAPPY29
 , btn_trigger_happy30  = BTN_TRIGGER_HAPPY30
 , btn_trigger_happy31  = BTN_TRIGGER_HAPPY31
 , btn_trigger_happy32  = BTN_TRIGGER_HAPPY32
 , btn_trigger_happy33  = BTN_TRIGGER_HAPPY33
 , btn_trigger_happy34  = BTN_TRIGGER_HAPPY34
 , btn_trigger_happy35  = BTN_TRIGGER_HAPPY35
 , btn_trigger_happy36  = BTN_TRIGGER_HAPPY36
 , btn_trigger_happy37  = BTN_TRIGGER_HAPPY37
 , btn_trigger_happy38  = BTN_TRIGGER_HAPPY38
 , btn_trigger_happy39  = BTN_TRIGGER_HAPPY39
 , btn_trigger_happy40  = BTN_TRIGGER_HAPPY40
 }
