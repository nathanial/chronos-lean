/-
  Chronos.DateTime
  Broken-down date/time components.
-/

import Chronos.Timestamp

namespace Chronos

/-- Broken-down date/time with nanosecond precision.
    Represents a calendar date and time of day. -/
structure DateTime where
  /-- Full year (e.g., 2025). Can be negative for BCE dates. -/
  year : Int32
  /-- Month of year [1, 12]. -/
  month : UInt8
  /-- Day of month [1, 31]. -/
  day : UInt8
  /-- Hour of day [0, 23]. -/
  hour : UInt8
  /-- Minute of hour [0, 59]. -/
  minute : UInt8
  /-- Second of minute [0, 59]. Note: leap seconds not supported. -/
  second : UInt8
  /-- Nanosecond of second [0, 999999999]. -/
  nanosecond : UInt32
  deriving Repr, BEq, Inhabited

namespace DateTime

-- ============================================================================
-- FFI declarations
-- ============================================================================

/-- Raw FFI return type: nested tuple of DateTime fields -/
private abbrev DateTimeTuple :=
  Int32 × UInt8 × UInt8 × UInt8 × UInt8 × UInt8 × UInt32

/-- Raw FFI: Convert timestamp to UTC date/time components. -/
@[extern "chronos_to_utc"]
private opaque toUtcFFI (seconds : Int) (nanos : UInt32) : IO DateTimeTuple

/-- Raw FFI: Convert timestamp to local date/time components. -/
@[extern "chronos_to_local"]
private opaque toLocalFFI (seconds : Int) (nanos : UInt32) : IO DateTimeTuple

/-- Raw FFI: Convert UTC date/time components to timestamp. -/
@[extern "chronos_from_utc"]
private opaque fromUtcFFI
  (year : Int32) (month : UInt8) (day : UInt8)
  (hour : UInt8) (minute : UInt8) (second : UInt8)
  (nanosecond : UInt32) : IO (Int × UInt32)

/-- Raw FFI: Get current timezone offset in seconds. -/
@[extern "chronos_get_timezone_offset"]
private opaque getTimezoneOffsetFFI : IO Int32

-- ============================================================================
-- Tuple conversion helpers
-- ============================================================================

private def fromTuple (t : DateTimeTuple) : DateTime :=
  let (year, month, day, hour, minute, second, nanosecond) := t
  { year, month, day, hour, minute, second, nanosecond }

-- ============================================================================
-- Public API: Conversion from Timestamp
-- ============================================================================

/-- Convert a timestamp to UTC date/time. -/
def fromTimestampUtc (ts : Timestamp) : IO DateTime := do
  let tuple ← toUtcFFI ts.seconds ts.nanoseconds
  return fromTuple tuple

/-- Convert a timestamp to local date/time. -/
def fromTimestampLocal (ts : Timestamp) : IO DateTime := do
  let tuple ← toLocalFFI ts.seconds ts.nanoseconds
  return fromTuple tuple

/-- Convert a UTC date/time back to a timestamp. -/
def toTimestamp (dt : DateTime) : IO Timestamp := do
  let (secs, nanos) ← fromUtcFFI dt.year dt.month dt.day
                                  dt.hour dt.minute dt.second dt.nanosecond
  return { seconds := secs, nanoseconds := nanos }

-- ============================================================================
-- Public API: Current time
-- ============================================================================

/-- Get the current UTC date/time. -/
def nowUtc : IO DateTime := do
  let ts ← Timestamp.now
  fromTimestampUtc ts

/-- Get the current local date/time. -/
def nowLocal : IO DateTime := do
  let ts ← Timestamp.now
  fromTimestampLocal ts

/-- Get the current timezone offset in seconds (local - UTC).
    Positive for east of UTC, negative for west. -/
def getTimezoneOffset : IO Int32 :=
  getTimezoneOffsetFFI

-- ============================================================================
-- Formatting helpers
-- ============================================================================

private def padZero2 (n : UInt8) : String :=
  let s := toString n.toNat
  if s.length == 1 then "0" ++ s else s

private def padZero4 (n : Int32) : String :=
  let s := toString n.toInt
  if n >= 0 && n < 10 then "000" ++ s
  else if n >= 0 && n < 100 then "00" ++ s
  else if n >= 0 && n < 1000 then "0" ++ s
  else s

/-- Format as ISO 8601 date: YYYY-MM-DD -/
def toDateString (dt : DateTime) : String :=
  s!"{padZero4 dt.year}-{padZero2 dt.month}-{padZero2 dt.day}"

/-- Format as ISO 8601 time: HH:MM:SS -/
def toTimeString (dt : DateTime) : String :=
  s!"{padZero2 dt.hour}:{padZero2 dt.minute}:{padZero2 dt.second}"

/-- Format as ISO 8601 date and time: YYYY-MM-DDTHH:MM:SS -/
def toIso8601 (dt : DateTime) : String :=
  s!"{dt.toDateString}T{dt.toTimeString}"

/-- Format as ISO 8601 with nanoseconds: YYYY-MM-DDTHH:MM:SS.NNNNNNNNN -/
def toIso8601Full (dt : DateTime) : String :=
  let nanoStr := toString dt.nanosecond.toNat
  let padded := String.ofList (List.replicate (9 - nanoStr.length) '0') ++ nanoStr
  s!"{dt.toIso8601}.{padded}"

instance : ToString DateTime where
  toString := toIso8601

-- ============================================================================
-- Date utilities
-- ============================================================================

/-- Check if a year is a leap year. -/
def isLeapYear (year : Int32) : Bool :=
  let y := year.toInt
  (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)

/-- Get the number of days in a month. -/
def daysInMonth (year : Int32) (month : UInt8) : UInt8 :=
  match month.toNat with
  | 1 => 31  -- January
  | 2 => if isLeapYear year then 29 else 28
  | 3 => 31  -- March
  | 4 => 30  -- April
  | 5 => 31  -- May
  | 6 => 30  -- June
  | 7 => 31  -- July
  | 8 => 31  -- August
  | 9 => 30  -- September
  | 10 => 31 -- October
  | 11 => 30 -- November
  | 12 => 31 -- December
  | _ => 0   -- Invalid month

-- ============================================================================
-- Comparison
-- ============================================================================

instance : Ord DateTime where
  compare a b :=
    match compare a.year b.year with
    | .eq =>
      match compare a.month b.month with
      | .eq =>
        match compare a.day b.day with
        | .eq =>
          match compare a.hour b.hour with
          | .eq =>
            match compare a.minute b.minute with
            | .eq =>
              match compare a.second b.second with
              | .eq => compare a.nanosecond b.nanosecond
              | other => other
            | other => other
          | other => other
        | other => other
      | other => other
    | other => other

instance : LT DateTime := ltOfOrd
instance : LE DateTime := leOfOrd

end DateTime

end Chronos
