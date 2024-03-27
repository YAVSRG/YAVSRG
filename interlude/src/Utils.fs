namespace Interlude

open System
open System.Reflection
open System.Diagnostics
open System.IO
open Percyqaz.Common
open Prelude.Common

module Utils =

    /// Numeric version e.g. "0.5.16"
    let short_version =
        let v = Assembly.GetExecutingAssembly().GetName()

        if v.Version.Revision <> 0 then
            v.Version.ToString(4)
        else
            v.Version.ToString(3)

    /// Full version string e.g. "Interlude 0.5.16 (20220722)"
    let version =
        let v = Assembly.GetExecutingAssembly().GetName()

        if DEV_MODE then
            sprintf "%s %s (dev build)" v.Name short_version
        else
            sprintf "%s %s" v.Name short_version

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

    let format_duration_ms (ms: Time) =
        if ms > 3600_000f<ms> then
            sprintf
                "%i:%02i:%02i"
                (ms / 3600_000f<ms> |> floor |> int)
                ((ms / 60_000f<ms>) % 60f |> floor |> int)
                ((ms / 1_000f<ms>) % 60f |> floor |> int)
        else
            sprintf "%i:%02i" ((ms / 60_000f<ms>) % 60f |> floor |> int) ((ms / 1_000f<ms>) % 60f |> floor |> int)

    let format_timespan (ts: TimeSpan) =

        if ts < TimeSpan.Zero then
            "IN THE FUTURE?"
        elif ts.TotalDays > 365.0 then
            sprintf "%.0fy" (ts.TotalDays / 365.0)
        elif ts.TotalDays > 30.0 then
            sprintf "%.0fmo" (ts.TotalDays / 30.0)
        elif ts.TotalDays > 7.0 then
            sprintf "%.0fw" (ts.TotalDays / 7.0)
        elif ts.TotalDays > 1.0 then
            sprintf "%.0fd" ts.TotalDays
        elif ts.TotalHours > 1.0 then
            sprintf "%.0fh" ts.TotalHours
        elif ts.TotalMinutes > 1.0 then
            sprintf "%.0fm" ts.TotalMinutes
        else
            sprintf "%.0fs" ts.TotalSeconds

    let open_directory (path: string) =
        ProcessStartInfo("file://" + System.IO.Path.GetFullPath path, UseShellExecute = true)
        |> Process.Start
        |> ignore

    let open_url (url: string) =
        try
            Process.Start(ProcessStartInfo(url, UseShellExecute = true)) |> ignore
        with err ->
            Logging.Debug("Failed to open url in browser: " + url, err)

    type Drawing.Color with
        static member FromHsv(H: float32, S: float32, V: float32) =
            let C = V * S
            let X = C * (1.0f - MathF.Abs((H * 6.0f) %% 2.0f - 1.0f))
            let m = V - C

            let (r, g, b) =
                if H < 1.0f / 6.0f then (C, X, 0.0f)
                elif H < 2.0f / 6.0f then (X, C, 0.0f)
                elif H < 3.0f / 6.0f then (0.0f, C, X)
                elif H < 4.0f / 6.0f then (0.0f, X, C)
                elif H < 5.0f / 6.0f then (X, 0.0f, C)
                else (C, 0.0f, X)

            Color.FromArgb((r + m) * 255.0f |> int, (g + m) * 255.0f |> int, (b + m) * 255.0f |> int)

        /// Doesn't include alpha
        member this.ToHsv() : float32 * float32 * float32 =
            let R = float32 this.R / 255.0f
            let G = float32 this.G / 255.0f
            let B = float32 this.B / 255.0f
            let Cmax = max R G |> max B
            let Cmin = min R G |> min B
            let d = Cmax - Cmin

            let H =
                if d = 0.0f then 0.0f
                elif Cmax = R then (((G - B) / d) %% 6.0f) / 6.0f
                elif Cmax = G then (((B - R) / d) + 2.0f) / 6.0f
                else (((R - G) / d) + 4.0f) / 6.0f

            let S = if Cmax = 0.0f then 0.0f else d / Cmax

            let V = Cmax

            (H, S, V)

        member this.ToHex() : string =
            if this.A = 255uy then
                sprintf "#%02x%02x%02x" this.R this.G this.B
            else
                sprintf "#%02x%02x%02x%02x" this.R this.G this.B this.A

        static member FromHex(s: string) : Color =
            if s.Length = 9 && s.[0] = '#' then
                let alpha = Convert.ToByte(s.Substring(7), 16)
                Color.FromArgb(int alpha, Drawing.ColorTranslator.FromHtml(s.Substring(0, 7)))
            elif s.Length = 7 && s.[0] = '#' then
                Drawing.ColorTranslator.FromHtml(s)
            elif s.Length > 0 && s.[0] <> '#' then
                Drawing.ColorTranslator.FromHtml(s)
            else
                failwithf "Invalid color code: %s" s
