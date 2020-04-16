using System.Collections.Generic;

namespace Interlude.Gameplay.Collections
{
    //Represents a collection of charts. May include additional data such as preferred rate and mods.
    public class Collection
    {
        //Invariant: these lists are the same length and PlaylistData[i] is the corresponding data for Entries[i] for all indices i, OR PlaylistData is null, signifying no playlist
        public List<string> Entries;
        public List<PlaylistData> PlaylistData;
        public bool IsPlaylist => PlaylistData != null;

        public Collection()
        {
            Entries = new List<string>();
            PlaylistData = null;
        }

        public void MakePlaylist()
        {
            if (IsPlaylist) return;
            PlaylistData = new List<PlaylistData>();
            foreach (string entry in Entries)
            {
                PlaylistData.Add(DefaultPlaylistData);
            }
        }

        //A dialog should be used to confirm this action before doing so
        public void UnmakePlaylist()
        {
            PlaylistData = null;
        }

        public PlaylistData GetPlaylistData(int index)
        {
            if (IsPlaylist)
            {
                return PlaylistData[index];
            }
            return null;
        }

        //Currently does not support adding duplicates of the same chart
        public void AddItem(CachedChart c)
        {
            string id = c.GetFileIdentifier();
            int i = Entries.IndexOf(id);
            if (i < 0)
            {
                Entries.Add(id);
                if (IsPlaylist) PlaylistData.Add(DefaultPlaylistData);
            }
            else if (IsPlaylist)
            {
                PlaylistData[i] = DefaultPlaylistData;
            }
        }

        public void RemoveItem(CachedChart c)
        {
            string id = c.GetFileIdentifier();
            int i = Entries.IndexOf(id);
            if (i >= 0)
            {
                Entries.RemoveAt(i);
                if (IsPlaylist) PlaylistData.RemoveAt(i);
            }
        }

        static PlaylistData DefaultPlaylistData => new PlaylistData() { Mods = new Dictionary<string, Prelude.Utilities.DataGroup>(Game.Gameplay.SelectedMods), Rate = (float)Game.Options.Profile.Rate };
    }
}
