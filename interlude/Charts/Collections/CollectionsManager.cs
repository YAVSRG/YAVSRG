using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace YAVSRG.Charts.Collections
{
    public class CollectionsManager
    {
        public Dictionary<string, Collection> Collections = new Dictionary<string, Collection>();

        public static CollectionsManager LoadCollections()
        {
            string path = Path.Combine(Options.Options.General.WorkingDirectory, "Data", "Collections.json");
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

        public void Save()
        {
            string path = Path.Combine(Options.Options.General.WorkingDirectory, "Data", "Collections.json");
            Utils.SaveObject(this, path);
        }
    }
}
