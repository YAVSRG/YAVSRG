using System.Collections.Generic;
using System.IO;
using Prelude.Utilities;

namespace Interlude.Gameplay.Collections
{
    public class CollectionsManager
    {
        public Dictionary<string, Collection> Collections = new Dictionary<string, Collection>();
        public string SelectedCollection = "Favourites";

        public static CollectionsManager LoadCollections()
        {
            string path = Path.Combine(Game.WorkingDirectory, "Data", "Collections.json");
            if (File.Exists(path))
            {
                return Utils.LoadObject<CollectionsManager>(path);
            }
            return new CollectionsManager();
        }

        public void CreateNewCollection(string name, bool playlist)
        {
            if (!Collections.ContainsKey(name))
            {
                Collections.Add(name, new Collection());
                if (playlist) GetCollection(name).MakePlaylist();
            }
            else
            {
                Logging.Log("This collection already exists!", "", Logging.LogType.Warning);
            }
        }

        public Collection GetCollection(string name)
        {
            if (!Collections.ContainsKey(name))
            {
                Collections.Add(name, new Collection());
            }
            return Collections[name];
        }

        public void DeleteCollection(string name)
        {
            if (Collections.ContainsKey(name))
            {
                Collections.Remove(name);
            }
        }

        public void Save()
        {
            string path = Path.Combine(Game.WorkingDirectory, "Data", "Collections.json");
            Utils.SaveObject(this, path);
        }
    }
}
