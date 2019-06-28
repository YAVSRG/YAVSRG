using System;
using Interlude.IO;

namespace Interlude.Interface.Widgets.Toolbar
{
    class MusicControls : Widget
    {
        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (((Interface.Toolbar)Parent).State != WidgetState.DISABLED && Game.Options.General.Keybinds.Volume.Held())
            {
                float v = Game.Options.General.AudioVolume + Input.MouseScroll * 0.02f;
                v = Math.Max(0, Math.Min(1, v));
                if (v != Game.Options.General.AudioVolume)
                {
                    Game.Screens.Toolbar.AddNotification("Audio volume: " + ((int)(100 * v)).ToString() + "%");
                    Game.Options.General.AudioVolume = v;
                    Game.Audio.SetVolume(v);
                }
            }
        }
    }
}
