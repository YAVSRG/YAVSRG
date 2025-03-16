namespace Prelude.Skins.Themes

open System.IO
open System.IO.Compression
open Percyqaz.Data
open Prelude
open Prelude.Skins

(*
    Default config values for themes, textures, noteskins, widget layouts
*)

// todo: themes let you override the hard coded palette values in Percyqaz.Flux.UI.Style for UI color themes
[<Json.AutoCodec(false)>]
type ThemeConfig =
    {
        Name: string
        Font: string
        DefaultAccentColor: Color
        AlwaysUseDefaultAccentColor: bool
        AlwaysUseDefaultBackground: bool
        CursorSize: float32
    }
    static member Default: ThemeConfig =
        {
            Name = "Unnamed Theme"
            Font = "Interlude"
            DefaultAccentColor = Color.FromArgb(0, 160, 200)
            AlwaysUseDefaultAccentColor = false
            AlwaysUseDefaultBackground = false
            CursorSize = 50.0f
        }

    member this.Validate: ThemeConfig = this

type Theme(storage) as this =
    inherit Storage(storage)

    let mutable config: ThemeConfig = ThemeConfig.Default

    do
        this.ReloadFromDisk()

    member this.Config
        with set conf =
            config <- conf
            this.WriteJson(config, "theme.json")
        and get () = config

    override this.ReloadFromDisk() =
        base.ReloadFromDisk()
        config <-
            match this.TryGetJson<ThemeConfig>(true, "theme.json") with
            | Some data -> data.Validate
            | None -> failwith "theme.json was missing or didn't load properly"

    member this.GetTexture(name: string) : TextureLoadResult =
        let name =
            if
                (name = "logo" || name = "rain")
                && (let dayOfYear = System.DateTime.Today.DayOfYear in dayOfYear < 5 || dayOfYear > 350)
            then
                name + "-winterlude"
            else
                name

        let rules =
            {
                IsRequired = true
                MustBeSquare = name <> "background"
                MaxGridSize = (1, 1)
            }

        this.LoadTexture(name, rules, "Textures")

    member this.GetSound(name: string) : Stream option =
        this.TryReadFile("Sounds", name + ".wav")

    member this.GetFonts() : Stream seq =
        seq {
            for file in this.GetFiles "Fonts" do
                match Path.GetExtension(file).ToLower() with
                | ".otf"
                | ".ttf" ->
                    match this.TryReadFile("Fonts", file) with
                    | Some s ->
                        // Font loading requires seek
                        use ms = new MemoryStream()
                        s.CopyTo ms
                        ms.Position <- 0
                        yield ms
                        s.Dispose()
                    | None -> ()
                | _ -> ()
        }

    static member FromZipStream(stream: Stream) =
        new Theme(Embedded(new ZipArchive(stream)))

    static member FromPath(path: string) = new Theme(Folder path)

    static member FromFolderName(name: string) =
        Theme.FromPath(get_game_folder (Path.Combine("Themes", name)))

module Theme =

    let TEXTURES = [| "background"; "rain"; "logo"; "cursor" |]

    let SOUNDS =
        [|
            "hello"
            "click"
            "hover"
            "text-open"
            "text-close"
            "key"
            "notify-error"
            "notify-info"
            "notify-system"
            "notify-task"
        |]