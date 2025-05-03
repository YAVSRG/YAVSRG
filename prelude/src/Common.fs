namespace Prelude

open System
open System.IO
open System.Diagnostics
open SixLabors.ImageSharp
open System.Drawing
open System.Collections.Generic
open Percyqaz.Data
open Percyqaz.Common

[<AutoOpen>]
module Common =

#if DEBUG
    let DEV_MODE = true
#else
    let DEV_MODE = false
#endif

    type SettingCodec<'T, 'Config>() =
        inherit Json.Codec<Setting<'T, 'Config>>()

        override this.To(ctx: Json.Context) =
            let cdc = ctx.GetCodec<'T>()
            fun o -> cdc.To o.Value

        override this.From(ctx: Json.Context) =
            let cdc = ctx.GetCodec<'T>()

            fun o json ->
                o.Value <- cdc.From o.Value json
                o

        override this.Default(ctx: Json.Context) =
            let cdc = ctx.GetCodec<'T>()

            fun () ->
                let v = cdc.Default()

                {
                    Set = ignore
                    Get = K v
                    Config = Unchecked.defaultof<_>
                }

    type ColorCodec() =
        inherit Json.Codec<Color>()

        override this.To(ctx: Json.Context) =
            let cdc = ctx.GetCodec<byte * byte * byte * byte>()
            fun col -> cdc.To(col.A, col.R, col.G, col.B)

        override this.From(ctx: Json.Context) =
            let cdc = ctx.GetCodec<byte * byte * byte * byte>()

            fun _ json ->
                let (a, r, g, b) = cdc.FromDefault json
                Color.FromArgb(int a, int r, int g, int b)

        override this.Default(ctx: Json.Context) = fun () -> Color.White

    /// Takes a function and returns an equivalent function BUT if given the same input repeatedly it will reuse the previous value instead of recalculating
    /// Used to optimise repeated calls to chart calculations where you are likely to make several for one selected chart before changing to another
    let internal cached (f: 'A -> 'B) : 'A -> 'B =
        let LOCK_OBJ = obj()
        let mutable previous : ('A * 'B) option = None
        fun (x: 'A) ->
            match
                lock LOCK_OBJ (fun () ->
                    match previous with
                    | Some (_x, _y) when x = _x -> Some _y
                    | _ -> None
                )
            with
            | Some cached_calculation -> cached_calculation
            | None ->
                let res = f x
                lock LOCK_OBJ (fun () -> previous <- Some (x, res))
                res

    let internal cached2 (f: 'A -> 'B -> 'C) : 'A -> 'B -> 'C =
        let LOCK_OBJ = obj()
        let mutable previous : ('A * 'B * 'C) option = None
        fun (a: 'A) (b: 'B) ->
            match
                lock LOCK_OBJ (fun () ->
                    match previous with
                    | Some (_a, _b, _c) when a = _a && b = _b -> Some _c
                    | _ -> None
                )
            with
            | Some cached_calculation -> cached_calculation
            | None ->
                let res = f a b
                lock LOCK_OBJ (fun () -> previous <- Some (a, b, res))
                res

    type Bitmap = Image<PixelFormats.Rgba32>

    module Bitmap =

        //do Configuration.Default.PreferContiguousImageBuffers <- true

        let from_stream (close_stream: bool) (stream: Stream) : Bitmap option =
            let img =
                try
                    Some(Bitmap.Load<PixelFormats.Rgba32> stream)
                with
                | :? UnknownImageFormatException -> None
                | :? InvalidImageContentException -> None

            if close_stream then
                stream.Dispose()

            img

        let from_stream_async (close_stream: bool) (stream: Stream) : Async<Bitmap option> =
            async {
                match! Bitmap.LoadAsync<PixelFormats.Rgba32> stream |> Async.AwaitTask |> Async.Catch with
                | Choice1Of2 success ->
                    if close_stream then
                        stream.Dispose()

                    return Some success
                | Choice2Of2 exn ->
                    if close_stream then
                        stream.Dispose()

                    return
                        match exn with
                        | :? UnknownImageFormatException -> None
                        | :? InvalidImageContentException -> None
                        | _ -> raise exn
            }

    type Color = Drawing.Color

    type Drawing.Color with

        static member FromHsv(H: float32, S: float32, V: float32) : Color =
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

        static member FromHex(s: string) : Color option =
            try
                if s.Length = 9 && s.[0] = '#' then
                    let alpha = Convert.ToByte(s.Substring(7), 16)
                    Color.FromArgb(int alpha, ColorTranslator.FromHtml(s.Substring(0, 7))) |> Some
                elif s.Length = 7 && s.[0] = '#' then
                    ColorTranslator.FromHtml(s) |> Some
                elif s.Length > 0 && s.[0] <> '#' then
                    ColorTranslator.FromHtml(s) |> Some
                else
                    None
            with _ -> None

    let open_directory (path: string) =
        ProcessStartInfo("file://" + System.IO.Path.GetFullPath path, UseShellExecute = true)
        |> Process.Start
        |> ignore

    let open_url (url: string) =
        try
            Process.Start(ProcessStartInfo(url, UseShellExecute = true)) |> ignore
        with err ->
            Logging.Debug "Failed to open url '%s' in browser: %O" url err

    let get_game_folder (name: string) : string =
        let p = Path.Combine(Directory.GetCurrentDirectory(), name)
        Directory.CreateDirectory p |> ignore
        p

    (*
        Misc helpers (mostly data storage)
    *)

    let JSON =
        Json(Json.Settings.Default)
            .WithDefaults()
            .WithCodec<SettingCodec<_, _>>()
            .WithCodec<ColorCodec>()

    let load_important_json_file<'T> (name: string) (path: string) (prompt: bool) : 'T =
        if File.Exists path then
            match JSON.FromFile path with
            | Ok data -> data
            | Error err ->
                Logging.Critical "Could not load '%s'! Maybe it is corrupt?\n%O" (Path.GetFileName path) err

                if prompt then
                    Logging.Critical "This is likely a typo from manually editing the JSON yourself."
                    Logging.Critical "Please correct the typo, or if you can't, delete the file and a fresh new one will be created."

                    failwithf "Unable to parse JSON in %s" path

                JSON.Default<'T>()
        else
            Logging.Info "No %s file found, creating it." name
            JSON.Default<'T>()

    let save_important_json_file<'T> (path: string) (data: 'T) =
        let write = Path.ChangeExtension(path, ".new")
        let bak = Path.ChangeExtension(path, ".bak")
        let bak2 = Path.ChangeExtension(path, ".bak2")
        JSON.ToFile (write, true) data

        if File.Exists bak2 then
            File.Delete bak2

        if File.Exists bak then
            File.Move(bak, bak2)

        if File.Exists path then
            File.Move(path, bak)

        File.Move(write, path)

    let inline format_duration_ms (ms: 'T) : string =
        let ms = float32 ms
        if ms > 3600_000f then
            sprintf
                "%i:%02i:%02i"
                (ms / 3600_000f |> floor |> int)
                ((ms / 60_000f) % 60f |> floor |> int)
                ((ms / 1_000f) % 60f |> floor |> int)
        else
            sprintf "%i:%02i" ((ms / 60_000f) % 60f |> floor |> int) ((ms / 1_000f) % 60f |> floor |> int)

    let format_timespan (ts: TimeSpan) : string =

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