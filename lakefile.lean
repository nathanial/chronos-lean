import Lake
open Lake DSL System

package chronos where
  version := v!"0.1.0"

require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.8"

@[default_target]
lean_lib Chronos where
  roots := #[`Chronos]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe chronos_tests where
  root := `Tests.Main

lean_exe chronos_demo where
  root := `Main

-- FFI: Build C code
target chronos_ffi_o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "chronos_ffi.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "chronos_ffi.c"
  let leanIncludeDir ← getLeanIncludeDir
  let weakArgs := #["-I", leanIncludeDir.toString]
  buildO oFile srcJob weakArgs #["-fPIC", "-O2"] "cc" getLeanTrace

extern_lib chronos_native pkg := do
  let name := nameToStaticLib "chronos_native"
  let ffiO ← chronos_ffi_o.fetch
  buildStaticLib (pkg.buildDir / "lib" / name) #[ffiO]
