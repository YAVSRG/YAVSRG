using System;
using System.Collections.Generic;
using Prelude.Utilities;

namespace Interlude.Gameplay.Collections
{
    public class PlaylistData
    {
        public float Rate;
        public Dictionary<string, DataGroup> Mods;

        public void Apply()
        {
            Game.Gameplay.SelectedMods = new Dictionary<string, DataGroup>(Mods);
            Game.Options.Profile.Rate = Rate;
        }
    }
}
