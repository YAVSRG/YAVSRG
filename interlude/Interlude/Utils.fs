namespace Interlude

open System.Reflection

module Utils =
    let version = "Interlude v0.4.0"

    let K x _ = x

    let getResourceStream name =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Resources." + name)