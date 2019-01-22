using System.Collections.Generic;
using System.IO;

namespace YAVSRG.Gameplay.Charts.Collections
{
    public class CollectionsManager
    {
        public Dictionary<string, Collection> Collections = new Dictionary<string, Collection>();

        public static CollectionsManager LoadCollections()
        {
            string path = Path.Combine(Game.WorkingDirectory, "Data", "Collections.json");
            if (File.Exists(path))
            {
                return Utils.LoadObject<CollectionsManager>(path);
            }
            return new CollectionsManager();
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
