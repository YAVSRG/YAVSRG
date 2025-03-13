namespace YAVSRG.CLI.Features

open System.IO
open System.IO.Compression
open YAVSRG.CLI.Utils

module Releases =

    let README = """
Interlude - Keyboard-based vertically scrolling rhythm game.
Copyright (C) 2018-2025 Percyqaz

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

----

Thank you for playing Interlude!
Information, future updates and support available at:
    https://yavsrg.net
    OR https://discord.com/invite/tA22tWR
    OR https://github.com/YAVSRG/YAVSRG
    """

    type BuildPlatformInfo =
        {
            Name: string
            RuntimeId: string
            BassLibraryFile: string
            BassFxLibraryFile: string
            GLFWLibraryFile: string
            SQLiteLibraryFile: string
            ExecutableFile: string
        }
        static member WINDOWS_X64 =
            {
                Name = "win64"
                RuntimeId = "win-x64"
                BassLibraryFile = "bass.dll"
                BassFxLibraryFile = "bass_fx.dll"
                GLFWLibraryFile = "glfw3.dll"
                SQLiteLibraryFile = "e_sqlite3.dll"
                ExecutableFile = "Interlude.exe"
            }
        static member LINUX_X64 =
            {
                Name = "linux-x64"
                RuntimeId = "linux-x64"
                BassLibraryFile = "libbass.so"
                BassFxLibraryFile = "libbass_fx.so"
                GLFWLibraryFile = "libglfw.so.3.3"
                SQLiteLibraryFile = "libe_sqlite3.so"
                ExecutableFile = "Interlude"
            }
        static member OSX_X64 =
            {
                Name = "osx-x64"
                RuntimeId = "osx-x64"
                BassLibraryFile = "libbass.dylib"
                BassFxLibraryFile = "libbass_fx.dylib"
                GLFWLibraryFile = "libglfw.3.dylib"
                SQLiteLibraryFile = "libe_sqlite3.dylib"
                ExecutableFile = "Interlude"
            }
        static member OSX_ARM64 =
            {
                Name = "osx-arm64"
                RuntimeId = "osx-arm64"
                BassLibraryFile = "libbass.dylib"
                BassFxLibraryFile = "libbass_fx.dylib"
                GLFWLibraryFile = "libglfw.3.dylib"
                SQLiteLibraryFile = "libe_sqlite3.dylib"
                ExecutableFile = "Interlude"
            }

    let build_platform (info: BuildPlatformInfo) =

        let build_dir =
            Path.Combine(INTERLUDE_SOURCE_PATH, "bin", "Release", "net9.0", info.RuntimeId)

        let clean_dir =
            Path.Combine(YAVSRG_PATH, "interlude", "releases", $"Interlude-{info.Name}")

        try
            Directory.Delete(build_dir, true)
        with _ ->
            ()

        try
            Directory.Delete(clean_dir, true)
        with _ ->
            ()

        exec_at INTERLUDE_SOURCE_PATH
            "dotnet"
            $"publish --configuration Release -r {info.RuntimeId} -p:PublishSingleFile=True --self-contained true"

        Directory.CreateDirectory clean_dir |> ignore

        let rec copy source target =
            Directory.CreateDirectory target |> ignore

            for file in Directory.GetFiles source do
                match Path.GetExtension(file).ToLower() with
                | ".dll"
                | ".so"
                | ".dylib"
                | ".txt" -> File.Copy(file, Path.Combine(target, Path.GetFileName file))
                | _ -> ()

        File.Copy(
            Path.Combine(YAVSRG_PATH, "engine", "lib", info.RuntimeId, info.BassLibraryFile),
            Path.Combine(clean_dir, info.BassLibraryFile)
        )

        File.Copy(
            Path.Combine(YAVSRG_PATH, "engine", "lib", info.RuntimeId, info.BassFxLibraryFile),
            Path.Combine(clean_dir, info.BassFxLibraryFile)
        )

        File.Copy(Path.Combine(build_dir, "publish", info.ExecutableFile), Path.Combine(clean_dir, info.ExecutableFile))

        File.Copy(
            Path.Combine(build_dir, "publish", info.GLFWLibraryFile),
            Path.Combine(clean_dir, info.GLFWLibraryFile)
        )

        File.Copy(
            Path.Combine(build_dir, "publish", info.SQLiteLibraryFile),
            Path.Combine(clean_dir, info.SQLiteLibraryFile)
        )

        copy (Path.Combine(build_dir, "Locale")) (Path.Combine(clean_dir, "Locale"))

        File.WriteAllText(Path.Combine(clean_dir, "README.txt"), README.Trim() + "\n")
        if not (info.ExecutableFile.EndsWith "exe") then
            File.WriteAllText(Path.Combine(clean_dir, "launch.sh"), "chmod +x ./Interlude\n./Interlude")

        printfn "Outputted to: %s" clean_dir

        if File.Exists(clean_dir + ".zip") then
            File.Delete(clean_dir + ".zip")

        ZipFile.CreateFromDirectory(clean_dir, clean_dir + ".zip")
        printfn "Zipped to: %s.zip" clean_dir

    let build_win_x64 () = build_platform BuildPlatformInfo.WINDOWS_X64

    let build_linux_x64 () = build_platform BuildPlatformInfo.LINUX_X64

    let build_osx_x64 () = build_platform BuildPlatformInfo.OSX_X64

    let build_osx_arm64 () = build_platform BuildPlatformInfo.OSX_ARM64