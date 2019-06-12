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
                new SpriteButton("buttoninfo", "Notifications", () => { ((Interface.Toolbar)Parent).Chat.Expand(); })
                .Reposition(-80, 1, 0, 0, 0, 1, 80, 0));
            AddChild(
                new SpriteButton("buttonmusic", "Visualiser", () => { if (Game.Gameplay.CurrentCachedChart != null) Game.Screens.AddScreen(new ScreenVisualiser()); })
                .Reposition(-160, 1, 0, 0, -80, 1, 80, 0));
            AddChild(
                new SpriteButton("buttonoptions", "Options", () => { Game.Screens.AddScreen(new ScreenOptions()); })
                .Reposition(-240, 1, 0, 0, -160, 1, 80, 0));
            AddChild(
                new SpriteButton("buttonimport", "Import", () => { Game.Screens.AddScreen(new ScreenImport()); })
                .Reposition(-320, 1, 0, 0, -240, 1, 80, 0));
            AddChild(
                new SpriteButton("buttononline", "Multiplayer", () => { Game.Screens.AddScreen(new ScreenLobby()); })
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
