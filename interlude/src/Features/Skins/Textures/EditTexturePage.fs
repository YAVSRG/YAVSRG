namespace Interlude.Features.Skins

open Percyqaz.Flux.UI
open Prelude.Skins
open Interlude.UI

type EditTexturePage(source: Storage, texture_id: string) =
    inherit Page()

    let texture_editor = TextureEditGrid(source, texture_id)
    let selected_texture_actions = TextureActions.Create(source, texture_id, texture_editor)

    override this.Content() =
        source.SplitTexture(texture_id)

        NavigationContainer.Column()
            .With(
                texture_editor
                    .Position(Position.Box(0.5f, 0.0f, -375.0f, 200.0f, 750.0f, 750.0f)),
                selected_texture_actions
                    .Position(Position.SliceR(400.0f).Shrink(50.0f))
            )

    override this.Title = Icons.IMAGE + " " + texture_id