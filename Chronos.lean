/-
  Chronos - Wall clock time library for Lean 4

  Provides access to system wall clock time with nanosecond precision.

  ## Quick Start

  ```lean
  import Chronos

  def main : IO Unit := do
    -- Get current timestamp
    let ts ← Chronos.Timestamp.now
    IO.println s!"Unix timestamp: {ts.seconds}.{ts.nanoseconds}"

    -- Get current local date/time
    let dt ← Chronos.DateTime.nowLocal
    IO.println s!"Local time: {dt}"

    -- Get current UTC date/time
    let utc ← Chronos.DateTime.nowUtc
    IO.println s!"UTC time: {utc}"

    -- Work with durations
    let oneHour := Chronos.Duration.fromHours 1
    let later := ts + oneHour
    IO.println s!"One hour later: {later.seconds}"

    -- Parse date/time strings
    match Chronos.DateTime.parseIso8601 "2025-01-15T14:30:00" with
    | .ok dt => IO.println s!"Parsed: {dt}"
    | .error e => IO.println s!"Parse error: {e}"

    -- Date arithmetic
    let tomorrow := dt.addDaysPure 1
    IO.println s!"Tomorrow: {tomorrow}"
  ```
-/

import Chronos.Duration
import Chronos.Timestamp
import Chronos.DateTime
import Chronos.Monotonic

namespace Chronos

-- Re-export main functions at the Chronos namespace level for convenience

/-- Get the current wall clock time as a Unix timestamp. -/
def now : IO Timestamp := Timestamp.now

/-- Get the current local date/time. -/
def nowLocal : IO DateTime := DateTime.nowLocal

/-- Get the current UTC date/time. -/
def nowUtc : IO DateTime := DateTime.nowUtc

-- Duration convenience constructors

/-- Create a duration from seconds. -/
def seconds (n : Int) : Duration := Duration.fromSeconds n

/-- Create a duration from minutes. -/
def minutes (n : Int) : Duration := Duration.fromMinutes n

/-- Create a duration from hours. -/
def hours (n : Int) : Duration := Duration.fromHours n

/-- Create a duration from days. -/
def days (n : Int) : Duration := Duration.fromDays n

-- Monotonic timing utilities

/-- Time an IO action, returning the result and elapsed duration. -/
def timeAction (action : IO α) : IO (α × Duration) := time action

/-- Run an action N times and return the average duration. -/
def benchmarkAction (n : Nat) (action : IO α) : IO Duration := benchmark n action

end Chronos
