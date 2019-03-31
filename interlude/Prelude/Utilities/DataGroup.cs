using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Utilities
{
    //this is just a dictionary of objects with some methods to easily retrieve a value and give defaults if not present
    //used as dynamic way to store config for anything + ability to save and load it easily
    public class DataGroup : Dictionary<string, object> //todo: find better name
    {
        public T GetValue<T>(string tag, T def)
        {
            if (ContainsKey(tag)) //present in dictionary
            {
                var v = this[tag];
                if (v is T) //type match
                {
                    return (T)this[tag];
                }
            }
            //otherwise set the value so when saved to file the value is present to be edited
            this[tag] = def;
            return def;
        }
    }
}
