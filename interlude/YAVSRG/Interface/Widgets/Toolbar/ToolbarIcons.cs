using System.Drawing;
using Interlude.Interface.Screens;

namespace Interlude.Interface.Widgets.Toolbar
{
    class ToolbarIcons : FlowContainer
    {
        public ToolbarIcons()
        {
            ColumnSpacing = 0; MarginX = 0; MarginY = 0; FlowFromRight = true; Frame = 0; BackColor = () => Color.Transparent;
            AddChild(
                new SpriteButton("buttoninfo", () => { ((Interface.Toolbar)Parent).Chat.Expand(); }, null) { Tooltip = "Chat and notifications" }
                .Reposition(-80, 1, 0, 0, 0, 1, 80, 0));
            AddChild(
                new SpriteButton("buttonmusic", () => { if (Game.Gameplay.CurrentCachedChart != null) Game.Screens.AddScreen(new ScreenVisualiser()); }, () => Game.Options.General.Keybinds.Music) { Tooltip = "Music visualiser" }
                .Reposition(-160, 1, 0, 0, -80, 1, 80, 0));
            AddChild(
                new SpriteButton("buttonoptions", () => { Game.Screens.AddScreen(new ScreenOldOptions()); }, () => Game.Options.General.Keybinds.Options) { Tooltip = "Options" }
                .Reposition(-240, 1, 0, 0, -160, 1, 80, 0));
            AddChild(
                new SpriteButton("buttonimport", () => { Game.Screens.AddScreen(new ScreenImport()); }, () => Game.Options.General.Keybinds.Import) { Tooltip = "Import charts" }
                .Reposition(-320, 1, 0, 0, -240, 1, 80, 0));
            AddChild(
                new SpriteButton("buttononline", () => { Game.Screens.AddScreen(new ScreenLobby()); }, null) { Tooltip = "Multiplayer lobby screen (NYI)" }
                .Reposition(-400, 1, 0, 0, -320, 1, 80, 0));
        }

        public void Filter(byte b)
        {
            for (int i = 0; i < Children.Count; i++)
            {
                Children[i].SetState((b & 1) > 0 ? WidgetState.NORMAL : WidgetState.DISABLED);
                b >>= 1;
            }
        }
    }
}
