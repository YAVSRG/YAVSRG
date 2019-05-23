using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;

namespace Prelude.Utilities
{
    //this is just a dictionary of objects with some methods to easily retrieve a value and give defaults if not present
    //used as dynamic way to store config for anything + ability to save and load it easily
    public class DataGroup : Dictionary<string, object> //todo: find better name
    {
        public DataGroup()
        {

        }

        public T GetValue<T>(string tag, T def)
        {
            if (ContainsKey(tag)) //present in dictionary
            {
                var v = this[tag];
                try
                {
                    dynamic converted = Convert.ChangeType(v, v.GetType());
                    return (T)converted;
                }
                catch (Exception e)
                {
                    //silently fail
                }
            }
            //set the value so when saved to file the value is present to be edited
            this[tag] = def;
            return def;
        }
    }
}
