using System;
using Interlude.IO;

namespace Interlude.Interface.Widgets.Toolbar
{
    public class MusicControls : ToolbarWidget
    {
        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (!ToolbarCollapsed && Input.KeyPress(Game.Options.General.Binds.Volume))
            {
                float v = Game.Options.General.AudioVolume + Input.MouseScroll * 0.02f;
                v = Math.Max(0, Math.Min(1, v));
                if (v != Game.Options.General.AudioVolume)
                {
                    Game.Screens.Toolbar.AddNotification("Audio volume: " + ((int)(100 * v)).ToString()+"%", System.Drawing.Color.White);
                    Game.Options.General.AudioVolume = v;
                    Game.Audio.SetVolume(v);
                }
            }
        }
    }
}
