namespace Prelude.Skinning

open System.IO
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Skinning.Noteskins
open Prelude.Skinning.HudLayouts
open Prelude.Skinning

[<Json.AutoCodec>]
type SkinMetadata =
    {
        Name: string
        Author: string
        Editor: string option
    }
    static member Default =
        {
            Name = "Unnamed Skin"
            Author = "Unknown"
            Editor = None
        }

type Skin(storage) as this =
    inherit Storage(storage)

    let mutable meta: SkinMetadata = SkinMetadata.Default

    do
        this.ReloadFromDisk()

    member this.Metadata
        with set new_meta =
            meta <- new_meta
            this.WriteJson(meta, "skin.json")
        and get () = meta

    override this.ReloadFromDisk() =
        base.ReloadFromDisk()
        meta <-
            match this.TryGetJson<SkinMetadata>(true, "skin.json") with
            | Some data -> data
            | _ -> failwith "skin.json was missing or didn't load properly"

    member this.GetIcon() : TextureLoadResult =
        this.LoadTexture("note", { IsRequired = false; MustBeSquare = true; MaxGridSize = (16, 32) }, "Noteskin")

    static member FromPath(path: string) =
        try new Skin(Folder path) |> Ok
        with err -> Error err

    member this.NoteskinFolder() : string option =
        match storage with
        | Folder path -> 
            let noteskin_path = Path.Combine(path, "Noteskin") 
            if Noteskin.Exists noteskin_path then
                Some noteskin_path
            else None
        | _ -> failwith "impossible"

    member this.HudFolder() : string option =
        match storage with
        | Folder path -> 
            let hud_path = Path.Combine(path, "HUD") 
            if HudLayout.Exists hud_path then
                Some hud_path
            else None
        | _ -> failwith "impossible"

    static member Exists(path: string) =
        Directory.Exists path && File.Exists (Path.Combine(path, "skin.json"))

    /// Call to create default User folder with HUD data
    static member CreateDefault (default_skin_meta: SkinMetadata) (path: string) : HudLayout * Skin =
        let hud_path = Path.Combine(path, "HUD")
        if not (Directory.Exists hud_path) then
            Directory.CreateDirectory hud_path |> ignore

        let hud_settings = Path.Combine(hud_path, "hud.json")
        if File.Exists hud_settings then
            match JSON.FromFile<HudConfig>(hud_settings) with
            | Ok _ -> ()
            | Error _ -> 

            Logging.Critical("Your default User HUD's hud.json doesn't parse! Did you make a typo?\nIn future, use the ingame editor to avoid formatting mistakes.")
            Logging.Critical("If you want to FULLY reset your HUD file to defaults, type 'reset' now, otherwise go and fix it and then relaunch the game.")

            if System.Console.ReadLine().Trim().ToLower() <> "reset" then
                failwith "User chose to crash the game so they can fix their HUD config"
            
        JSON.ToFile(hud_settings, true) HudConfig.Default

        let skin_settings = Path.Combine(path, "skin.json")
        if File.Exists skin_settings then
            match JSON.FromFile<SkinMetadata>(skin_settings) with
            | Ok _ -> ()
            | Error _ -> 
                Logging.Warn("Resetting User skin.json because it didn't parse - Maybe you foolishly made a typo when editing it manually")
                JSON.ToFile(skin_settings, true) default_skin_meta
        else
            JSON.ToFile(skin_settings, true) default_skin_meta

        let hud =
            match HudLayout.FromPath hud_path with
            | Ok hud -> hud
            | Error err -> 
                Logging.Critical("Something terrible has happened while creating a default hud", err)
                raise err

        let skin =
            match Skin.FromPath path with
            | Ok skin -> skin
            | Error err -> 
                Logging.Critical("Something terrible has happened while creating a default skin", err)
                raise err

        hud, skin

module NoteskinToSkinMigration =

    let folder_should_migrate (folder_path: string) =
        not (Skin.Exists folder_path) && Noteskin.Exists folder_path

    let migrate_folder (folder_path: string) : Result<unit, string> =
        match JSON.FromFile<SkinMetadata> (Path.Combine(folder_path, "noteskin.json")) with
        | Error reason -> Error (sprintf "Parsing noteskin.json to skin metadata failed: %s" reason.Message)
        | Ok meta ->

        try
            let temporary_location = Path.Combine(Path.GetDirectoryName(folder_path), "_converting_to_skin_")
            Directory.Move(folder_path, temporary_location)
            Directory.CreateDirectory(folder_path) |> ignore
            let noteskin_location = Path.Combine(folder_path, "Noteskin")
            Directory.Move(temporary_location, noteskin_location)
            JSON.ToFile(Path.Combine(folder_path, "skin.json"), true) meta
            Ok()
        with err ->
            Error (sprintf "Error changing noteskin folder to make a skin folder: %s" err.Message)
