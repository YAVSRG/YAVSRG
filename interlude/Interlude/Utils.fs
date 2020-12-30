namespace Interlude

open System
open System.Reflection
open System.Diagnostics

module Utils =
    let version =
        let v = Assembly.GetExecutingAssembly().GetName()
        let v2 = Assembly.GetExecutingAssembly().Location |> FileVersionInfo.GetVersionInfo
        //let buildDate = DateTime(2000, 1, 1).AddDays(float version.Build).AddSeconds(float version.Revision * 2.0)
        sprintf "%s %s (%s)" v.Name (v.Version.ToString(3)) v2.ProductVersion

    let K x _ = x

    let getResourceStream name =
        Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Resources." + name)

    let otkColor (color: System.Drawing.Color) : OpenTK.Color =
        OpenTK.Color.FromArgb(int color.A, int color.R, int color.G, int color.B)