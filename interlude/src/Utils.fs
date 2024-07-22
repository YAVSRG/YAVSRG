namespace Interlude

open System
open System.Reflection
open System.IO
open Prelude

module Utils =

    let get_resource_stream name =
        Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream("Interlude.Resources." + name)

    let get_resource_text name =
        use s = get_resource_stream name
        use tr = new StreamReader(s)
        tr.ReadToEnd()

    let splash_message_picker (name) =
        let r = new Random()
        let text = get_resource_text name
        let lines = text.Split("\n")

        fun () -> lines.[r.Next lines.Length]