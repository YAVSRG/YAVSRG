using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.Collections
{
    public class Collection
    {
        public List<string> Entries; //list of absolute paths
        //todo: prevent crash when entry no longer in collection

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
    }
}
