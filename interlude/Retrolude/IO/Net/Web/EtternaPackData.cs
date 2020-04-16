using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Net.Web
{
    public class EtternaPackData
    {
        public class EtternaPack
        {
            public string type;
            public string id;
            public EtternaPackAttributes attributes;
        }

        public class EtternaPackAttributes
        {
            public string name;
            public double average;
            public string download;
            public long size;
        }

        public List<EtternaPack> data;
    }
}
