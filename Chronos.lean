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
  ```
-/

import Chronos.Timestamp
import Chronos.DateTime

namespace Chronos

-- Re-export main functions at the Chronos namespace level for convenience

/-- Get the current wall clock time as a Unix timestamp. -/
def now : IO Timestamp := Timestamp.now

/-- Get the current local date/time. -/
def nowLocal : IO DateTime := DateTime.nowLocal

/-- Get the current UTC date/time. -/
def nowUtc : IO DateTime := DateTime.nowUtc

end Chronos
