using Interlude.Interface.Widgets;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Screens
{
    class ScreenLobby : Screen
    {
        //will be repurposed for actual multi lobbies managed by the server
        public ScreenLobby()
        {
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Font1.DrawJustifiedText("All previous test code has been removed for now. Check back in future versions :)", 30f, bounds.Right - 10, bounds.Bottom - 60, Game.Options.Theme.MenuFont);
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Screens.Toolbar.Icons.Filter(0b00001111);
        }
    }
}
