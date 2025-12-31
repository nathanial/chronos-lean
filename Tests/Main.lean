/-
  Chronos Tests
-/

import Chronos
import Crucible

open Crucible
open Chronos

-- ============================================================================
-- Timestamp Tests
-- ============================================================================

testSuite "Chronos.Timestamp"

test "now returns reasonable values" := do
  let ts ← Timestamp.now
  -- Should be after 2024-01-01 (timestamp ~1704067200)
  let minTimestamp : Int := 1704067200
  -- Should be before 2100-01-01 (timestamp ~4102444800)
  let maxTimestamp : Int := 4102444800
  shouldSatisfy (ts.seconds > minTimestamp) "seconds > min"
  shouldSatisfy (ts.seconds < maxTimestamp) "seconds < max"
  -- Nanoseconds should be in valid range
  shouldSatisfy (ts.nanoseconds < 1000000000) "nanoseconds < 1e9"

test "epoch is zero" := do
  Timestamp.epoch.seconds ≡ 0
  Timestamp.epoch.nanoseconds ≡ 0

test "fromSeconds works" := do
  let ts := Timestamp.fromSeconds 1234567890
  ts.seconds ≡ 1234567890
  ts.nanoseconds ≡ 0

test "addSeconds works" := do
  let ts := Timestamp.fromSeconds 1000
  let ts2 := ts.addSeconds 500
  ts2.seconds ≡ 1500

test "subSeconds works" := do
  let ts := Timestamp.fromSeconds 1000
  let ts2 := ts.subSeconds 300
  ts2.seconds ≡ 700

test "comparison works" := do
  let a := Timestamp.fromSeconds 1000
  let b := Timestamp.fromSeconds 2000
  shouldSatisfy (a < b) "a < b"
  shouldSatisfy (b > a) "b > a"
  shouldSatisfy (a == a) "a == a"

test "toNanoseconds and fromNanoseconds roundtrip" := do
  let ts := { seconds := 1234, nanoseconds := 567890123 : Timestamp }
  let nanos := ts.toNanoseconds
  let ts2 := Timestamp.fromNanoseconds nanos
  ts2.seconds ≡ ts.seconds
  ts2.nanoseconds ≡ ts.nanoseconds

#generate_tests

-- ============================================================================
-- DateTime Tests
-- ============================================================================

namespace DateTimeTests

testSuite "Chronos.DateTime"

test "nowUtc returns valid date" := do
  let dt ← DateTime.nowUtc
  -- Year should be reasonable
  shouldSatisfy (dt.year >= 2024) "year >= 2024"
  shouldSatisfy (dt.year < 2100) "year < 2100"
  -- Month 1-12
  shouldSatisfy (dt.month >= 1) "month >= 1"
  shouldSatisfy (dt.month <= 12) "month <= 12"
  -- Day 1-31
  shouldSatisfy (dt.day >= 1) "day >= 1"
  shouldSatisfy (dt.day <= 31) "day <= 31"
  -- Hour 0-23
  shouldSatisfy (dt.hour <= 23) "hour <= 23"
  -- Minute 0-59
  shouldSatisfy (dt.minute <= 59) "minute <= 59"
  -- Second 0-59
  shouldSatisfy (dt.second <= 59) "second <= 59"
  -- Nanosecond 0-999999999
  shouldSatisfy (dt.nanosecond < 1000000000) "nanosecond < 1e9"

test "nowLocal returns valid date" := do
  let dt ← DateTime.nowLocal
  shouldSatisfy (dt.year >= 2024) "year >= 2024"
  shouldSatisfy (dt.month >= 1) "month >= 1"
  shouldSatisfy (dt.month <= 12) "month <= 12"
  shouldSatisfy (dt.day >= 1) "day >= 1"
  shouldSatisfy (dt.day <= 31) "day <= 31"

test "UTC/Timestamp roundtrip" := do
  let ts ← Timestamp.now
  let dt ← DateTime.fromTimestampUtc ts
  let ts2 ← dt.toTimestamp
  -- Should get back the same second (nanoseconds might differ due to rounding)
  ts2.seconds ≡ ts.seconds

test "toIso8601 formats correctly" := do
  let dt : DateTime := {
    year := 2025
    month := 12
    day := 27
    hour := 14
    minute := 30
    second := 45
    nanosecond := 0
  }
  dt.toIso8601 ≡ "2025-12-27T14:30:45"

test "toDateString formats correctly" := do
  let dt : DateTime := {
    year := 2025
    month := 1
    day := 5
    hour := 0
    minute := 0
    second := 0
    nanosecond := 0
  }
  dt.toDateString ≡ "2025-01-05"

test "isLeapYear works" := do
  shouldSatisfy (DateTime.isLeapYear 2024) "2024 is leap year"
  shouldSatisfy (!DateTime.isLeapYear 2023) "2023 is not leap year"
  shouldSatisfy (DateTime.isLeapYear 2000) "2000 is leap year"
  shouldSatisfy (!DateTime.isLeapYear 1900) "1900 is not leap year"

test "daysInMonth works" := do
  DateTime.daysInMonth 2024 2 ≡ 29  -- Leap year February
  DateTime.daysInMonth 2023 2 ≡ 28  -- Non-leap year February
  DateTime.daysInMonth 2024 1 ≡ 31  -- January
  DateTime.daysInMonth 2024 4 ≡ 30  -- April

test "comparison works" := do
  let a : DateTime := { year := 2024, month := 1, day := 1, hour := 0, minute := 0, second := 0, nanosecond := 0 }
  let b : DateTime := { year := 2024, month := 1, day := 2, hour := 0, minute := 0, second := 0, nanosecond := 0 }
  shouldSatisfy (a < b) "a < b"
  shouldSatisfy (a == a) "a == a"

test "getTimezoneOffset returns reasonable value" := do
  let offset ← DateTime.getTimezoneOffset
  -- Timezone offsets are typically between -12 and +14 hours
  -- That's -43200 to +50400 seconds
  shouldSatisfy (offset >= -43200) "offset >= -43200"
  shouldSatisfy (offset <= 50400) "offset <= 50400"

#generate_tests

end DateTimeTests

-- ============================================================================
-- Duration Tests
-- ============================================================================

namespace DurationTests

testSuite "Chronos.Duration"

test "zero is zero" := do
  Duration.zero.nanoseconds ≡ 0
  shouldSatisfy Duration.zero.isZero "zero.isZero"

test "fromSeconds creates correct nanoseconds" := do
  let d := Duration.fromSeconds 5
  d.nanoseconds ≡ 5000000000

test "fromHours creates correct nanoseconds" := do
  let d := Duration.fromHours 2
  d.nanoseconds ≡ 2 * 60 * 60 * 1000000000

test "fromDays creates correct nanoseconds" := do
  let d := Duration.fromDays 1
  d.nanoseconds ≡ 24 * 60 * 60 * 1000000000

test "toSeconds roundtrip" := do
  let d := Duration.fromSeconds 3600
  d.toSeconds ≡ 3600

test "toMinutes works" := do
  let d := Duration.fromMinutes 90
  d.toMinutes ≡ 90

test "add durations" := do
  let a := Duration.fromMinutes 30
  let b := Duration.fromMinutes 45
  (a + b).toMinutes ≡ 75

test "subtract durations" := do
  let a := Duration.fromHours 2
  let b := Duration.fromMinutes 30
  (a - b).toMinutes ≡ 90

test "negative duration" := do
  let a := Duration.fromMinutes 30
  let b := Duration.fromHours 1
  let diff := a - b
  shouldSatisfy diff.isNegative "30m - 1h is negative"
  diff.toMinutes ≡ -30

test "multiply by scalar" := do
  let d := Duration.fromHours 2
  (d * 3).toHours ≡ (6 : Int)

test "divide by scalar" := do
  let d := Duration.fromHours 6
  (d / 2).toHours ≡ (3 : Int)

test "comparison works" := do
  let a := Duration.fromMinutes 30
  let b := Duration.fromHours 1
  shouldSatisfy (a < b) "30m < 1h"
  shouldSatisfy (b > a) "1h > 30m"

test "toHumanString formats correctly" := do
  (Duration.fromSeconds 3661).toHumanString ≡ "1h 1m 1s"
  (Duration.fromDays 2).toHumanString ≡ "2d"
  Duration.zero.toHumanString ≡ "0s"
  (Duration.fromHours 25).toHumanString ≡ "1d 1h"

test "toHumanString handles sub-second" := do
  (Duration.fromMilliseconds 500).toHumanString ≡ "500ms"
  (Duration.fromNanoseconds 1000).toHumanString ≡ "1000ns"

test "negative duration formatting" := do
  let d := Duration.fromHours (-2)
  shouldSatisfy (d.toHumanString.startsWith "-") "negative prefix"

test "abs works" := do
  let d := Duration.fromHours (-5)
  d.abs.toHours ≡ 5
  shouldSatisfy d.abs.isPositive "abs is positive"

#generate_tests

end DurationTests

-- ============================================================================
-- Timestamp-Duration Integration Tests
-- ============================================================================

namespace TimestampDurationTests

testSuite "Chronos.Timestamp.Duration"

test "add duration to timestamp" := do
  let ts := Timestamp.fromSeconds 1000
  let d := Duration.fromSeconds 500
  let result := ts + d
  result.seconds ≡ 1500

test "subtract duration from timestamp" := do
  let ts := Timestamp.fromSeconds 1000
  let d := Duration.fromSeconds 300
  let result := ts - d
  result.seconds ≡ 700

test "duration between timestamps" := do
  let a := Timestamp.fromSeconds 1500
  let b := Timestamp.fromSeconds 1000
  let d := Timestamp.duration a b
  d.toSeconds ≡ 500

test "duration can be negative" := do
  let a := Timestamp.fromSeconds 1000
  let b := Timestamp.fromSeconds 1500
  let d := Timestamp.duration a b
  d.toSeconds ≡ -500

test "add hours to timestamp" := do
  let ts := Timestamp.fromSeconds 0
  let d := Duration.fromHours 1
  let result := ts + d
  result.seconds ≡ 3600

#generate_tests

end TimestampDurationTests

-- ============================================================================
-- DateTime Parsing Tests
-- ============================================================================

namespace ParsingTests

testSuite "Chronos.DateTime.Parsing"

test "parseIso8601 basic" := do
  match DateTime.parseIso8601 "2025-12-27T14:30:45" with
  | .ok dt =>
    dt.year ≡ 2025
    dt.month ≡ 12
    dt.day ≡ 27
    dt.hour ≡ 14
    dt.minute ≡ 30
    dt.second ≡ 45
  | .error e => throw (IO.userError s!"parse failed: {e}")

test "parseIso8601 date only" := do
  match DateTime.parseIso8601 "2025-01-15" with
  | .ok dt =>
    dt.year ≡ 2025
    dt.month ≡ 1
    dt.day ≡ 15
    dt.hour ≡ 0
    dt.minute ≡ 0
    dt.second ≡ 0
  | .error e => throw (IO.userError s!"parse failed: {e}")

test "parseIso8601 with space separator" := do
  match DateTime.parseIso8601 "2025-01-01 12:00:00" with
  | .ok dt =>
    dt.hour ≡ 12
    dt.minute ≡ 0
  | .error e => throw (IO.userError s!"parse failed: {e}")

test "parseIso8601 with nanoseconds" := do
  match DateTime.parseIso8601 "2025-01-01T00:00:00.123456789" with
  | .ok dt => dt.nanosecond ≡ 123456789
  | .error e => throw (IO.userError s!"parse failed: {e}")

test "parseIso8601 with partial fractional seconds" := do
  match DateTime.parseIso8601 "2025-01-01T00:00:00.1" with
  | .ok dt => dt.nanosecond ≡ 100000000
  | .error e => throw (IO.userError s!"parse failed: {e}")

test "parseIso8601 rejects invalid month" := do
  match DateTime.parseIso8601 "2025-13-01T00:00:00" with
  | .ok _ => throw (IO.userError "should have failed")
  | .error _ => pure ()

test "parseIso8601 rejects invalid day" := do
  match DateTime.parseIso8601 "2025-02-29T00:00:00" with
  | .ok _ => throw (IO.userError "should have failed: 2025 is not leap year")
  | .error _ => pure ()

test "parseIso8601 accepts Feb 29 in leap year" := do
  match DateTime.parseIso8601 "2024-02-29T00:00:00" with
  | .ok dt => dt.day ≡ 29
  | .error e => throw (IO.userError s!"parse failed: {e}")

test "parseIso8601 rejects invalid hour" := do
  match DateTime.parseIso8601 "2025-01-01T24:00:00" with
  | .ok _ => throw (IO.userError "should have failed")
  | .error _ => pure ()

test "parseIso8601 rejects empty input" := do
  match DateTime.parseIso8601 "" with
  | .ok _ => throw (IO.userError "should have failed")
  | .error _ => pure ()

test "parseDate works" := do
  match DateTime.parseDate "2025-06-15" with
  | .ok dt =>
    dt.year ≡ 2025
    dt.month ≡ 6
    dt.day ≡ 15
  | .error e => throw (IO.userError s!"parse failed: {e}")

test "parseTime works" := do
  match DateTime.parseTime "14:30:45" with
  | .ok dt =>
    dt.hour ≡ 14
    dt.minute ≡ 30
    dt.second ≡ 45
    dt.year ≡ 1970  -- Default epoch year
  | .error e => throw (IO.userError s!"parse failed: {e}")

test "parseTime with nanoseconds" := do
  match DateTime.parseTime "12:00:00.5" with
  | .ok dt => dt.nanosecond ≡ 500000000
  | .error e => throw (IO.userError s!"parse failed: {e}")

#generate_tests

end ParsingTests

-- ============================================================================
-- DateTime Arithmetic Tests
-- ============================================================================

namespace ArithmeticTests

testSuite "Chronos.DateTime.Arithmetic"

private def mkDate (y : Int32) (m d : UInt8) : DateTime :=
  { year := y, month := m, day := d, hour := 0, minute := 0, second := 0, nanosecond := 0 }

private def mkDateTime (y : Int32) (mo d h mi s : UInt8) : DateTime :=
  { year := y, month := mo, day := d, hour := h, minute := mi, second := s, nanosecond := 0 }

test "addDays positive" := do
  let dt := mkDate 2025 1 15
  let result := dt.addDaysPure 10
  result.day ≡ 25
  result.month ≡ 1

test "addDays crosses month boundary" := do
  let dt := mkDate 2025 1 25
  let result := dt.addDaysPure 10
  result.day ≡ 4
  result.month ≡ 2

test "addDays negative" := do
  let dt := mkDate 2025 2 5
  let result := dt.addDaysPure (-10)
  result.day ≡ 26
  result.month ≡ 1

test "addDays crosses year boundary" := do
  let dt := mkDate 2025 12 25
  let result := dt.addDaysPure 10
  result.year ≡ 2026
  result.month ≡ 1
  result.day ≡ 4

test "addMonths basic" := do
  let dt := mkDate 2025 1 15
  let result := dt.addMonthsPure 3
  result.month ≡ 4
  result.year ≡ 2025

test "addMonths clamps day (Jan 31 + 1 month)" := do
  let dt := mkDate 2025 1 31
  let result := dt.addMonthsPure 1
  result.month ≡ 2
  result.day ≡ 28  -- 2025 is not a leap year

test "addMonths clamps to leap day" := do
  let dt := mkDate 2024 1 31
  let result := dt.addMonthsPure 1
  result.month ≡ 2
  result.day ≡ 29  -- 2024 is a leap year

test "addMonths crosses year boundary" := do
  let dt := mkDate 2025 11 15
  let result := dt.addMonthsPure 3
  result.month ≡ 2
  result.year ≡ 2026

test "addMonths negative" := do
  let dt := mkDate 2025 3 15
  let result := dt.addMonthsPure (-2)
  result.month ≡ 1
  result.year ≡ 2025

test "addYears basic" := do
  let dt := mkDate 2025 6 15
  let result := dt.addYearsPure 5
  result.year ≡ 2030
  result.month ≡ 6

test "addYears from leap day to non-leap year" := do
  let dt := mkDate 2024 2 29
  let result := dt.addYearsPure 1
  result.year ≡ 2025
  result.month ≡ 2
  result.day ≡ 28  -- Clamped because 2025 is not leap year

test "addHours basic" := do
  let dt := mkDateTime 2025 1 1 10 0 0
  let result := dt.addHoursPure 5
  result.hour ≡ 15

test "addHours crosses day boundary" := do
  let dt := mkDateTime 2025 1 1 23 0 0
  let result := dt.addHoursPure 3
  result.day ≡ 2
  result.hour ≡ 2

test "addHours negative" := do
  let dt := mkDateTime 2025 1 2 2 0 0
  let result := dt.addHoursPure (-5)
  result.day ≡ 1
  result.hour ≡ 21

test "addMinutes basic" := do
  let dt := mkDateTime 2025 1 1 12 30 0
  let result := dt.addMinutesPure 45
  result.hour ≡ 13
  result.minute ≡ 15

test "addMinutes crosses hour boundary" := do
  let dt := mkDateTime 2025 1 1 12 45 0
  let result := dt.addMinutesPure 30
  result.hour ≡ 13
  result.minute ≡ 15

test "addSeconds crosses minute boundary" := do
  let dt := mkDateTime 2025 1 1 12 59 45
  let result := dt.addSecondsPure 30
  result.hour ≡ 13
  result.minute ≡ 0
  result.second ≡ 15

test "addDuration with hours" := do
  let dt := mkDateTime 2025 1 1 10 0 0
  let d := Duration.fromHours 5
  let result := dt.addDurationPure d
  result.hour ≡ 15

test "addDuration crosses day" := do
  let dt := mkDateTime 2025 1 1 20 0 0
  let d := Duration.fromHours 10
  let result := dt.addDurationPure d
  result.day ≡ 2
  result.hour ≡ 6

#generate_tests

end ArithmeticTests

-- ============================================================================
-- Main
-- ============================================================================

def main : IO UInt32 := runAllSuites
