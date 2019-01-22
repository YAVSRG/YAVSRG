using System.Collections.Generic;

namespace YAVSRG.Gameplay.Charts.Collections
{
    public class Collection
    {
        public List<string> Entries; //list of absolute paths

        public Collection()
        {
            Entries = new List<string>();
        }

        public void AddItem(CachedChart c)
        {
            string id = c.GetFileIdentifier();
            if (!Entries.Contains(id))
            {
                Entries.Add(id);
            }
        }

        public void RemoveItem(CachedChart c)
        {
            string id = c.GetFileIdentifier();
            if (Entries.Contains(id))
            {
                Entries.Remove(id);
            }
        }
    }
}
