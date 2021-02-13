namespace Interlude

open System.Reflection
open System.Diagnostics

module Utils =
    let version =
        let v = Assembly.GetExecutingAssembly().GetName()
        let v2 = Assembly.GetExecutingAssembly().Location |> FileVersionInfo.GetVersionInfo
        sprintf "%s %s (%s)" v.Name (v.Version.ToString(3)) v2.ProductVersion

    let K x _ = x

    let getResourceStream name =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Resources." + name)