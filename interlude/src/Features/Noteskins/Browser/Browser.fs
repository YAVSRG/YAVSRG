namespace Interlude.Features.Noteskins.Browser

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins.Repo
open Prelude.Data
open Interlude.UI
open Interlude.UI.Menu

type NoteskinsBrowserPage() =
    inherit Page()

    let mutable loading = true
    let mutable error = false
    let mutable selected_group = None

    let noteskin_items =
        GridFlowContainer<GroupDisplay>(100.0f, 2, WrapNavigation = false, Spacing = (15.0f, 15.0f))

    let version_items =
        FlowContainer.Vertical<VersionDisplay>(520.0f, Spacing = 15.0f)

    let search_groups = 
        NavigationContainer.Column()
        |+ Dummy(NodeType.Leaf)
        |+ ScrollContainer(noteskin_items, Margin = Style.PADDING, Position = Position.TrimTop(70.0f))
        |>> (fun nt -> Container(nt, Position = { Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Right = 0.65f %- 10.0f }))
        |+ (SearchBox(
                Setting.simple "",
                (fun (query: string) -> noteskin_items.Filter <- GroupDisplay.Filter query),
                Position = Position.SliceTop 60.0f,
                Fill = K Colors.cyan.O3,
                Border = K Colors.cyan_accent,
                TextColor = K Colors.text_cyan
            )
            |+ LoadingIndicator.Border(fun () -> loading))
        |+ EmptyState(Icons.X, %"noteskins.browser.error").Conditional(fun () -> error)

    let pick_versions =
        ScrollContainer(version_items, Margin = Style.PADDING, Position = Position.TrimTop(70.0f))
        |>> (fun nt -> Container(nt, Position = { Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Left = 0.65f %+ 10.0f }))
        |+ Text(%"noteskins.browser.install_hint", Color = K Colors.text_subheading, Align = Alignment.CENTER, Position = Position.SliceTop(70.0f).Margin(10.0f)).Conditional(fun () -> selected_group.IsSome)

    let select_group(group: NoteskinGroup) =
        selected_group <- Some group
        version_items.Clear()
        for version in group.Versions do
            version_items.Add <| VersionDisplay(group, version)

    member this.Refresh() =
        loading <- true
        error <- false
        noteskin_items.Clear()
        WebServices.download_json (
            sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/backbeat/noteskins/index.json" "githubusercontent",
            fun data ->
                match data with
                | Some(d: NoteskinRepo) ->
                    defer (fun () ->
                        for group in d.Noteskins do
                            let is_selected = Setting.make (fun _ -> select_group group) (fun _ -> selected_group = Some group)
                            noteskin_items.Add <| GroupDisplay(group, is_selected)
                        loading <- false
                    )
                | None ->
                    defer (fun () ->
                        error <- true
                        loading <- false
                    )
        )

    override this.Content() =

        this.Refresh()

        NavigationContainer.Row()
        |+ search_groups
        |+ pick_versions
        :> Widget

    override this.Title = %"noteskins.browser"

    override this.OnClose() = ()