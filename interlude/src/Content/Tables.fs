namespace Interlude.Content

open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Backbeat

module Tables =

    let mutable initialised = false
    /// Table IDs are the name of the file under the Data/Tables folder, without the extension
    let private _selected_id: Setting<string option> = Setting.simple None
    let mutable current: Table option = None
    let private loaded = new Dictionary<string, Table>()

    /// Once called: All tables from the Data/Tables folder have been read; The valid ones are loaded into the available list
    /// Can be called multiple times to re-load with the latest data
    let private load () : unit =
        loaded.Clear()

        for file in Directory.EnumerateFiles(get_game_folder <| Path.Combine("Data", "Tables")) do
            if Path.GetExtension(file).ToLower() = ".table" then
                let id = Path.GetFileNameWithoutExtension(file)

                match Table.load id with
                | Some table -> loaded.Add(id, table)
                | None -> Logging.Info "Table '%s' is out of date" id

    let install_or_update (table: Table) : unit =
        Table.save table
        loaded.[table.Id] <- table

        if _selected_id.Value = Some table.Id then
            current <- Some table

    let init () =
        load ()

        match _selected_id.Value with
        | Some id when loaded.ContainsKey id -> current <- Some loaded.[id]
        | Some id ->
            Logging.Warn "Table '%s' not found" id
            _selected_id.Value <- None
        | None -> ()

        initialised <- true

    let selected_id : Setting<string option> =
        Setting.make
            (fun new_id ->
                if initialised then
                    match new_id with
                    | Some id when loaded.ContainsKey id -> _selected_id.Value <- Some id
                    | Some id ->
                        Logging.Warn "Table '%s' not found" id
                        _selected_id.Value <- None
                    | None -> _selected_id.Value <- None

                    match _selected_id.Value with
                    | Some id when loaded.ContainsKey id -> current <- Some loaded.[id]
                    | _ -> current <- None
                else
                    _selected_id.Value <- new_id
            )
            (fun () -> _selected_id.Value)

    let by_id (id: string) : Table option =
        if loaded.ContainsKey id then Some loaded.[id] else None

    let list () : Table list = loaded.Values |> List.ofSeq