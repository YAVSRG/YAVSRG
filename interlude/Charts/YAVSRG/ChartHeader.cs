using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.YAVSRG
{
    public class ChartHeader
    {
        public string Title;
        public string Artist;
        public string Creator;
        public string SourcePath;
        public string SourcePack;
        public string DiffName;
        public string BGFile;
        public string AudioFile;
        public float PreviewTime;
        [JsonIgnore]
        public string File; //should not be saved, only used in memory to track the file name for renaming to .yav and such
    }
}
