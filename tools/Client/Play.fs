namespace Interlude.Tools.Features

open System.IO
open Interlude.Tools.Utils

module Play =

    let debug_run () =
        exec_at INTERLUDE_SOURCE_PATH "dotnet" "build --configuration Debug -v q"
        exec_at (Path.Combine(INTERLUDE_SOURCE_PATH, "bin", "Debug", "net8.0")) "dotnet" "run --project ../../.."

    let update () =
        exec "git" "checkout main"
        exec "git" "pull"
        exec "git" "fetch --tags"

        let tag_digest = eval "git" "rev-list --tags --max-count=1"
        let tag_name = eval "git" (sprintf "describe --tags \"%s\"" tag_digest)
        exec "git" (sprintf "checkout %s" tag_name)