{-# LANGUAGE ForeignFunctionInterface , RecordWildCards #-}
module System.Linux.Input.Evdev.Uapi.Internal.Types.InputEvent where

import Control.Applicative ((<$>), (<*>))
import Data.Int            (Int32)
import Data.UnixTime       (UnixTime(..))
import Data.Word           (Word16, Word)
import Foreign.Storable

#include <linux/input.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

{-
  www.kernel.org/doc/Documentation/input/input.txt

  Should you want to add event device support into any application (X, gpm,
  svgalib ...) I <vojtech@ucw.cz> will be happy to provide you any help I
  can. Here goes a description of the current state of things, which is going
  to be extended, but not changed incompatibly as time goes:

  You can use blocking and nonblocking reads, also select() on the
  /dev/input/eventX devices, and you'll always get a whole number of input
  events on a read. Their layout is:

  struct input_event {
    struct timeval time;
    unsigned short type;
    unsigned short code;
    unsigned int value;
  };

    'time' is the timestamp, it returns the time at which the event happened.
  Type is for example EV_REL for relative moment, EV_KEY for a keypress or
  release. More types are defined in include/linux/input.h.

    'code' is event code, for example REL_X or KEY_BACKSPACE, again a complete
  list is in include/linux/input.h.

    'value' is the value the event carries. Either a relative change for
  EV_REL, absolute new value for EV_ABS (joysticks ...), or 0 for EV_KEY for
  release, 1 for keypress and 2 for autorepeat.
-}

{-
  www.kernel.org/doc/Documentation/input/ff.txt

  3.4 Controlling the playback of effects
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Control of playing is done with write(). Below is an example:

  #include <linux/input.h>
  #include <unistd.h>

    struct input_event play;
    struct input_event stop;
    struct ff_effect effect;
    int fd;
    ...
    fd = open("/dev/input/eventXX", O_RDWR);
    ...
    /* Play three times */
    play.type = EV_FF;
    play.code = effect.id;
    play.value = 3;
    write(fd, (const void*) &play, sizeof(play));
    ...
    /* Stop an effect */
    stop.type = EV_FF;
    stop.code = effect.id;
    stop.value = 0;

    write(fd, (const void*) &play, sizeof(stop));

  3.5 Setting the gain
  ~~~~~~~~~~~~~~~~~~~~
  Not all devices have the same strength. Therefore, users should set a gain
  factor depending on how strong they want effects to be. This setting is
  persistent across access to the driver.

  /* Set the gain of the device */
  int gain;		/* between 0 and 100 */
  struct input_event ie;	/* structure used to communicate with the driver */

  ie.type = EV_FF;
  ie.code = FF_GAIN;
  ie.value = 0xFFFFUL * gain / 100;

  if (write(fd, &ie, sizeof(ie)) == -1)
    perror("set gain");

  3.6 Enabling/Disabling autocenter
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  The autocenter feature quite disturbs the rendering of effects in my opinion,
  and I think it should be an effect, which computation depends on the game
  type. But you can enable it if you want.

  int autocenter;		/* between 0 and 100 */
  struct input_event ie;

  ie.type = EV_FF;
  ie.code = FF_AUTOCENTER;
  ie.value = 0xFFFFUL * autocenter / 100;

  if (write(fd, &ie, sizeof(ie)) == -1)
    perror("set auto-center");

  A value of 0 means "no auto-center".

  3.7 Dynamic update of an effect
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Proceed as if you wanted to upload a new effect, except that instead of
  setting the id field to -1, you set it to the wanted effect id.
  Normally, the effect is not stopped and restarted. However, depending on the
  type of device, not all parameters can be dynamically updated. For example,
  the direction of an effect cannot be updated with iforce devices. In this
  case, the driver stops the effect, up-load it, and restart it.

  Therefore it is recommended to dynamically change direction while the effect
  is playing only when it is ok to restart the effect with a replay count of 1.

  3.8 Information about the status of effects
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Every time the status of an effect is changed, an event is sent. The values
  and meanings of the fields of the event are as follows:

  struct input_event {
  /* When the status of the effect changed */
  	struct timeval time;

  /* Set to EV_FF_STATUS */
  	unsigned short type;

  /* Contains the id of the effect */
  	unsigned short code;

  /* Indicates the status */
  	unsigned int value;
  };

  FF_STATUS_STOPPED	The effect stopped playing
  FF_STATUS_PLAYING	The effect started to play

  NOTE: Status feedback is only supported by iforce driver. If you have
        a really good reason to use this, please contact
        linux-joystick@atrey.karlin.mff.cuni.cz or anssi.hannula@gmail.com
-}

data InputEvent =
  InputEvent {
    ieTime  :: !UnixTime
  , ieType  :: !Word16
  , ieCode  :: !Word16
  , ieValue :: !Word
  } deriving (Eq)

instance Storable InputEvent where
  sizeOf _    = (#size struct input_event)
  alignment _ = (#alignment struct input_event)
  peek ptr    =
    InputEvent
        <$> (#peek struct input_event, time)  ptr
        <*> (#peek struct input_event, type)  ptr
        <*> (#peek struct input_event, code)  ptr
        <*> (#peek struct input_event, value) ptr
  poke ptr (InputEvent {..}) = do
      (#poke struct input_event, time)  ptr ieTime
      (#poke struct input_event, type)  ptr ieType
      (#poke struct input_event, code)  ptr ieCode
      (#poke struct input_event, value) ptr ieValue

newtype EventType = EventType { unEventType :: Word16 } deriving (Eq, Show)
#{enum EventType, EventType,
  EV_SYN,
  EV_KEY,
  EV_REL,
  EV_ABS,
  EV_MSC,
  EV_SW,
  EV_LED,
  EV_SND,
  EV_REP,
  EV_FF,
  EV_PWR,
  EV_FF_STATUS,
  EV_MAX,
  EV_CNT }

newtype SynCode = SynCode { unSynCode :: Word16 } deriving (Eq, Show)
#{enum SynCode, SynCode,
  SYN_REPORT,
  SYN_CONFIG,
  SYN_MT_REPORT,
  SYN_DROPPED,
  SYN_MAX,
  SYN_CNT }

newtype RelAxesCode = RelAxesCode { unRelAxesCode :: Word16 } deriving (Eq, Show)
#{enum RelAxesCode, RelAxesCode,
  REL_X,
  REL_Y,
  REL_Z,
  REL_RX,
  REL_RY,
  REL_RZ,
  REL_HWHEEL,
  REL_DIAL,
  REL_WHEEL,
  REL_MISC,
  REL_MAX,
  REL_CNT }

newtype AbsAxesCode = AbsAxesCode { unAbsAxesCode :: Word16 } deriving (Eq, Show)
#{enum AbsAxesCode, AbsAxesCode,
  ABS_X,
  ABS_Y,
  ABS_Z,
  ABS_RX,
  ABS_RY,
  ABS_RZ,
  ABS_THROTTLE,
  ABS_RUDDER,
  ABS_WHEEL,
  ABS_GAS,
  ABS_BRAKE,
  ABS_HAT0X,
  ABS_HAT0Y,
  ABS_HAT1X,
  ABS_HAT1Y,
  ABS_HAT2X,
  ABS_HAT2Y,
  ABS_HAT3X,
  ABS_HAT3Y,
  ABS_PRESSURE,
  ABS_DISTANCE,
  ABS_TILT_X,
  ABS_TILT_Y,
  ABS_TOOL_WIDTH,
  ABS_VOLUME,
  ABS_MISC,
  ABS_MT_SLOT,
  ABS_MT_TOUCH_MAJOR,
  ABS_MT_TOUCH_MINOR,
  ABS_MT_WIDTH_MAJOR,
  ABS_MT_WIDTH_MINOR,
  ABS_MT_ORIENTATION,
  ABS_MT_POSITION_X,
  ABS_MT_POSITION_Y,
  ABS_MT_TOOL_TYPE,
  ABS_MT_BLOB_ID,
  ABS_MT_TRACKING_ID,
  ABS_MT_PRESSURE,
  ABS_MT_DISTANCE,
  ABS_MT_TOOL_X,
  ABS_MT_TOOL_Y,
  ABS_MAX,
  ABS_CNT }

newtype SWCode = SWCode { unSWCode :: Word16 } deriving (Eq, Show)
#{enum SWCode, SWCode,
  SW_LID,
  SW_TABLET_MODE,
  SW_HEADPHONE_INSERT,
  SW_RFKILL_ALL,
  SW_MICROPHONE_INSERT,
  SW_DOCK,
  SW_LINEOUT_INSERT,
  SW_JACK_PHYSICAL_INSERT,
  SW_VIDEOOUT_INSERT,
  SW_CAMERA_LENS_COVER,
  SW_KEYPAD_SLIDE,
  SW_FRONT_PROXIMITY,
  SW_ROTATE_LOCK,
  SW_LINEIN_INSERT,
  SW_MAX,
  SW_CNT }

newtype MSCCode = MSCCode { unMSCCode :: Word16 } deriving (Eq, Show)
#{enum MSCCode, MSCCode,
  MSC_SERIAL,
  MSC_PULSELED,
  MSC_GESTURE,
  MSC_RAW,
  MSC_SCAN,
  MSC_TIMESTAMP,
  MSC_MAX,
  MSC_CNT }

newtype LEDCode = LEDCode { unLEDCode :: Word16 } deriving (Eq, Show)
#{enum LEDCode, LEDCode,
  LED_NUML,
  LED_CAPSL,
  LED_SCROLLL,
  LED_COMPOSE,
  LED_KANA,
  LED_SLEEP,
  LED_SUSPEND,
  LED_MUTE,
  LED_MISC,
  LED_MAIL,
  LED_CHARGING,
  LED_MAX,
  LED_CNT }

data KeyValue = KeyValue { unKeyValue :: Int32 } deriving (Eq, Show)

keyRelease :: KeyValue
keyRelease = KeyValue 0

keyPress :: KeyValue
keyPress = KeyValue 1

keyAutorepeat :: KeyValue
keyAutorepeat = KeyValue 2

newtype KeyCode = KeyCode { unKeyCode :: Word16 } deriving (Eq, Show)
#{enum KeyCode, KeyCode,
  KEY_RESERVED,
  KEY_ESC,
  KEY_1,
  KEY_2,
  KEY_3,
  KEY_4,
  KEY_5,
  KEY_6,
  KEY_7,
  KEY_8,
  KEY_9,
  KEY_0,
  KEY_MINUS,
  KEY_EQUAL,
  KEY_BACKSPACE,
  KEY_TAB,
  KEY_Q,
  KEY_W,
  KEY_E,
  KEY_R,
  KEY_T,
  KEY_Y,
  KEY_U,
  KEY_I,
  KEY_O,
  KEY_P,
  KEY_LEFTBRACE,
  KEY_RIGHTBRACE,
  KEY_ENTER,
  KEY_LEFTCTRL,
  KEY_A,
  KEY_S,
  KEY_D,
  KEY_F,
  KEY_G,
  KEY_H,
  KEY_J,
  KEY_K,
  KEY_L,
  KEY_SEMICOLON,
  KEY_APOSTROPHE,
  KEY_GRAVE,
  KEY_LEFTSHIFT,
  KEY_BACKSLASH,
  KEY_Z,
  KEY_X,
  KEY_C,
  KEY_V,
  KEY_B,
  KEY_N,
  KEY_M,
  KEY_COMMA,
  KEY_DOT,
  KEY_SLASH,
  KEY_RIGHTSHIFT,
  KEY_KPASTERISK,
  KEY_LEFTALT,
  KEY_SPACE,
  KEY_CAPSLOCK,
  KEY_F1,
  KEY_F2,
  KEY_F3,
  KEY_F4,
  KEY_F5,
  KEY_F6,
  KEY_F7,
  KEY_F8,
  KEY_F9,
  KEY_F10,
  KEY_NUMLOCK,
  KEY_SCROLLLOCK,
  KEY_KP7,
  KEY_KP8,
  KEY_KP9,
  KEY_KPMINUS,
  KEY_KP4,
  KEY_KP5,
  KEY_KP6,
  KEY_KPPLUS,
  KEY_KP1,
  KEY_KP2,
  KEY_KP3,
  KEY_KP0,
  KEY_KPDOT,
  KEY_ZENKAKUHANKAKU,
  KEY_102ND,
  KEY_F11,
  KEY_F12,
  KEY_RO,
  KEY_KATAKANA,
  KEY_HIRAGANA,
  KEY_HENKAN,
  KEY_KATAKANAHIRAGANA,
  KEY_MUHENKAN,
  KEY_KPJPCOMMA,
  KEY_KPENTER,
  KEY_RIGHTCTRL,
  KEY_KPSLASH,
  KEY_SYSRQ,
  KEY_RIGHTALT,
  KEY_LINEFEED,
  KEY_HOME,
  KEY_UP,
  KEY_PAGEUP,
  KEY_LEFT,
  KEY_RIGHT,
  KEY_END,
  KEY_DOWN,
  KEY_PAGEDOWN,
  KEY_INSERT,
  KEY_DELETE,
  KEY_MACRO,
  KEY_MUTE,
  KEY_VOLUMEDOWN,
  KEY_VOLUMEUP,
  KEY_POWER,
  KEY_KPEQUAL,
  KEY_KPPLUSMINUS,
  KEY_PAUSE,
  KEY_SCALE,
  KEY_KPCOMMA,
  KEY_HANGEUL,
  KEY_HANGUEL,
  KEY_HANJA,
  KEY_YEN,
  KEY_LEFTMETA,
  KEY_RIGHTMETA,
  KEY_COMPOSE,
  KEY_STOP,
  KEY_AGAIN,
  KEY_PROPS,
  KEY_UNDO,
  KEY_FRONT,
  KEY_COPY,
  KEY_OPEN,
  KEY_PASTE,
  KEY_FIND,
  KEY_CUT,
  KEY_HELP,
  KEY_MENU,
  KEY_CALC,
  KEY_SETUP,
  KEY_SLEEP,
  KEY_WAKEUP,
  KEY_FILE,
  KEY_SENDFILE,
  KEY_DELETEFILE,
  KEY_XFER,
  KEY_PROG1,
  KEY_PROG2,
  KEY_WWW,
  KEY_MSDOS,
  KEY_COFFEE,
  KEY_SCREENLOCK,
  KEY_DIRECTION,
  KEY_CYCLEWINDOWS,
  KEY_MAIL,
  KEY_BOOKMARKS,
  KEY_COMPUTER,
  KEY_BACK,
  KEY_FORWARD,
  KEY_CLOSECD,
  KEY_EJECTCD,
  KEY_EJECTCLOSECD,
  KEY_NEXTSONG,
  KEY_PLAYPAUSE,
  KEY_PREVIOUSSONG,
  KEY_STOPCD,
  KEY_RECORD,
  KEY_REWIND,
  KEY_PHONE,
  KEY_ISO,
  KEY_CONFIG,
  KEY_HOMEPAGE,
  KEY_REFRESH,
  KEY_EXIT,
  KEY_MOVE,
  KEY_EDIT,
  KEY_SCROLLUP,
  KEY_SCROLLDOWN,
  KEY_KPLEFTPAREN,
  KEY_KPRIGHTPAREN,
  KEY_NEW,
  KEY_REDO,
  KEY_F13,
  KEY_F14,
  KEY_F15,
  KEY_F16,
  KEY_F17,
  KEY_F18,
  KEY_F19,
  KEY_F20,
  KEY_F21,
  KEY_F22,
  KEY_F23,
  KEY_F24,
  KEY_PLAYCD,
  KEY_PAUSECD,
  KEY_PROG3,
  KEY_PROG4,
  KEY_DASHBOARD,
  KEY_SUSPEND,
  KEY_CLOSE,
  KEY_PLAY,
  KEY_FASTFORWARD,
  KEY_BASSBOOST,
  KEY_PRINT,
  KEY_HP,
  KEY_CAMERA,
  KEY_SOUND,
  KEY_QUESTION,
  KEY_EMAIL,
  KEY_CHAT,
  KEY_SEARCH,
  KEY_CONNECT,
  KEY_FINANCE,
  KEY_SPORT,
  KEY_SHOP,
  KEY_ALTERASE,
  KEY_CANCEL,
  KEY_BRIGHTNESSDOWN,
  KEY_BRIGHTNESSUP,
  KEY_MEDIA,
  KEY_SWITCHVIDEOMODE,
  KEY_KBDILLUMTOGGLE,
  KEY_KBDILLUMDOWN,
  KEY_KBDILLUMUP,
  KEY_SEND,
  KEY_REPLY,
  KEY_FORWARDMAIL,
  KEY_SAVE,
  KEY_DOCUMENTS,
  KEY_BATTERY,
  KEY_BLUETOOTH,
  KEY_WLAN,
  KEY_UWB,
  KEY_UNKNOWN,
  KEY_VIDEO_NEXT,
  KEY_VIDEO_PREV,
  KEY_BRIGHTNESS_CYCLE,
  KEY_BRIGHTNESS_ZERO,
  KEY_DISPLAY_OFF,
  KEY_WIMAX,
  KEY_RFKILL,
  KEY_MICMUTE,
  BTN_MISC,
  BTN_0,
  BTN_1,
  BTN_2,
  BTN_3,
  BTN_4,
  BTN_5,
  BTN_6,
  BTN_7,
  BTN_8,
  BTN_9,
  BTN_MOUSE,
  BTN_LEFT,
  BTN_RIGHT,
  BTN_MIDDLE,
  BTN_SIDE,
  BTN_EXTRA,
  BTN_FORWARD,
  BTN_BACK,
  BTN_TASK,
  BTN_JOYSTICK,
  BTN_TRIGGER,
  BTN_THUMB,
  BTN_THUMB2,
  BTN_TOP,
  BTN_TOP2,
  BTN_PINKIE,
  BTN_BASE,
  BTN_BASE2,
  BTN_BASE3,
  BTN_BASE4,
  BTN_BASE5,
  BTN_BASE6,
  BTN_DEAD,
  BTN_GAMEPAD,
  BTN_A,
  BTN_B,
  BTN_C,
  BTN_X,
  BTN_Y,
  BTN_Z,
  BTN_TL,
  BTN_TR,
  BTN_TL2,
  BTN_TR2,
  BTN_SELECT,
  BTN_START,
  BTN_MODE,
  BTN_THUMBL,
  BTN_THUMBR,
  BTN_DIGI,
  BTN_TOOL_PEN,
  BTN_TOOL_RUBBER,
  BTN_TOOL_BRUSH,
  BTN_TOOL_PENCIL,
  BTN_TOOL_AIRBRUSH,
  BTN_TOOL_FINGER,
  BTN_TOOL_MOUSE,
  BTN_TOOL_LENS,
  BTN_TOOL_QUINTTAP,
  BTN_TOUCH,
  BTN_STYLUS,
  BTN_STYLUS2,
  BTN_TOOL_DOUBLETAP,
  BTN_TOOL_TRIPLETAP,
  BTN_TOOL_QUADTAP,
  BTN_WHEEL,
  BTN_GEAR_DOWN,
  BTN_GEAR_UP,
  KEY_OK,
  KEY_SELECT,
  KEY_GOTO,
  KEY_CLEAR,
  KEY_POWER2,
  KEY_OPTION,
  KEY_INFO,
  KEY_TIME,
  KEY_VENDOR,
  KEY_ARCHIVE,
  KEY_PROGRAM,
  KEY_CHANNEL,
  KEY_FAVORITES,
  KEY_EPG,
  KEY_PVR,
  KEY_MHP,
  KEY_LANGUAGE,
  KEY_TITLE,
  KEY_SUBTITLE,
  KEY_ANGLE,
  KEY_ZOOM,
  KEY_MODE,
  KEY_KEYBOARD,
  KEY_SCREEN,
  KEY_PC,
  KEY_TV,
  KEY_TV2,
  KEY_VCR,
  KEY_VCR2,
  KEY_SAT,
  KEY_SAT2,
  KEY_CD,
  KEY_TAPE,
  KEY_RADIO,
  KEY_TUNER,
  KEY_PLAYER,
  KEY_TEXT,
  KEY_DVD,
  KEY_AUX,
  KEY_MP3,
  KEY_AUDIO,
  KEY_VIDEO,
  KEY_DIRECTORY,
  KEY_LIST,
  KEY_MEMO,
  KEY_CALENDAR,
  KEY_RED,
  KEY_GREEN,
  KEY_YELLOW,
  KEY_BLUE,
  KEY_CHANNELUP,
  KEY_CHANNELDOWN,
  KEY_FIRST,
  KEY_LAST,
  KEY_AB,
  KEY_NEXT,
  KEY_RESTART,
  KEY_SLOW,
  KEY_SHUFFLE,
  KEY_BREAK,
  KEY_PREVIOUS,
  KEY_DIGITS,
  KEY_TEEN,
  KEY_TWEN,
  KEY_VIDEOPHONE,
  KEY_GAMES,
  KEY_ZOOMIN,
  KEY_ZOOMOUT,
  KEY_ZOOMRESET,
  KEY_WORDPROCESSOR,
  KEY_EDITOR,
  KEY_SPREADSHEET,
  KEY_GRAPHICSEDITOR,
  KEY_PRESENTATION,
  KEY_DATABASE,
  KEY_NEWS,
  KEY_VOICEMAIL,
  KEY_ADDRESSBOOK,
  KEY_MESSENGER,
  KEY_DISPLAYTOGGLE,
  KEY_SPELLCHECK,
  KEY_LOGOFF,
  KEY_DOLLAR,
  KEY_EURO,
  KEY_FRAMEBACK,
  KEY_FRAMEFORWARD,
  KEY_CONTEXT_MENU,
  KEY_MEDIA_REPEAT,
  KEY_10CHANNELSUP,
  KEY_10CHANNELSDOWN,
  KEY_IMAGES,
  KEY_DEL_EOL,
  KEY_DEL_EOS,
  KEY_INS_LINE,
  KEY_DEL_LINE,
  KEY_FN,
  KEY_FN_ESC,
  KEY_FN_F1,
  KEY_FN_F2,
  KEY_FN_F3,
  KEY_FN_F4,
  KEY_FN_F5,
  KEY_FN_F6,
  KEY_FN_F7,
  KEY_FN_F8,
  KEY_FN_F9,
  KEY_FN_F10,
  KEY_FN_F11,
  KEY_FN_F12,
  KEY_FN_1,
  KEY_FN_2,
  KEY_FN_D,
  KEY_FN_E,
  KEY_FN_F,
  KEY_FN_S,
  KEY_FN_B,
  KEY_BRL_DOT1,
  KEY_BRL_DOT2,
  KEY_BRL_DOT3,
  KEY_BRL_DOT4,
  KEY_BRL_DOT5,
  KEY_BRL_DOT6,
  KEY_BRL_DOT7,
  KEY_BRL_DOT8,
  KEY_BRL_DOT9,
  KEY_BRL_DOT10,
  KEY_NUMERIC_0,
  KEY_NUMERIC_1,
  KEY_NUMERIC_2,
  KEY_NUMERIC_3,
  KEY_NUMERIC_4,
  KEY_NUMERIC_5,
  KEY_NUMERIC_6,
  KEY_NUMERIC_7,
  KEY_NUMERIC_8,
  KEY_NUMERIC_9,
  KEY_NUMERIC_STAR,
  KEY_NUMERIC_POUND,
  KEY_CAMERA_FOCUS,
  KEY_WPS_BUTTON,
  KEY_TOUCHPAD_TOGGLE,
  KEY_TOUCHPAD_ON,
  KEY_TOUCHPAD_OFF,
  KEY_CAMERA_ZOOMIN,
  KEY_CAMERA_ZOOMOUT,
  KEY_CAMERA_UP,
  KEY_CAMERA_DOWN,
  KEY_CAMERA_LEFT,
  KEY_CAMERA_RIGHT,
  BTN_TRIGGER_HAPPY,
  BTN_TRIGGER_HAPPY1,
  BTN_TRIGGER_HAPPY2,
  BTN_TRIGGER_HAPPY3,
  BTN_TRIGGER_HAPPY4,
  BTN_TRIGGER_HAPPY5,
  BTN_TRIGGER_HAPPY6,
  BTN_TRIGGER_HAPPY7,
  BTN_TRIGGER_HAPPY8,
  BTN_TRIGGER_HAPPY9,
  BTN_TRIGGER_HAPPY10,
  BTN_TRIGGER_HAPPY11,
  BTN_TRIGGER_HAPPY12,
  BTN_TRIGGER_HAPPY13,
  BTN_TRIGGER_HAPPY14,
  BTN_TRIGGER_HAPPY15,
  BTN_TRIGGER_HAPPY16,
  BTN_TRIGGER_HAPPY17,
  BTN_TRIGGER_HAPPY18,
  BTN_TRIGGER_HAPPY19,
  BTN_TRIGGER_HAPPY20,
  BTN_TRIGGER_HAPPY21,
  BTN_TRIGGER_HAPPY22,
  BTN_TRIGGER_HAPPY23,
  BTN_TRIGGER_HAPPY24,
  BTN_TRIGGER_HAPPY25,
  BTN_TRIGGER_HAPPY26,
  BTN_TRIGGER_HAPPY27,
  BTN_TRIGGER_HAPPY28,
  BTN_TRIGGER_HAPPY29,
  BTN_TRIGGER_HAPPY30,
  BTN_TRIGGER_HAPPY31,
  BTN_TRIGGER_HAPPY32,
  BTN_TRIGGER_HAPPY33,
  BTN_TRIGGER_HAPPY34,
  BTN_TRIGGER_HAPPY35,
  BTN_TRIGGER_HAPPY36,
  BTN_TRIGGER_HAPPY37,
  BTN_TRIGGER_HAPPY38,
  BTN_TRIGGER_HAPPY39,
  BTN_TRIGGER_HAPPY40,
  KEY_MAX,
  KEY_CNT }

newtype MTToolCode = MTToolCode { unMTToolCode :: Word16 } deriving (Eq, Show)
#{enum MTToolCode, MTToolCode,
  MT_TOOL_FINGER,
  MT_TOOL_PEN,
  MT_TOOL_MAX}

newtype SndCode = SndCode { unSndCode :: Word16 } deriving (Eq, Show)
#{enum SndCode, SndCode,
  SND_CLICK,
  SND_BELL,
  SND_TONE,
  SND_MAX,
  SND_CNT }

