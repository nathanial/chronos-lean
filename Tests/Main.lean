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
-- Main
-- ============================================================================

def main : IO UInt32 := runAllSuites
