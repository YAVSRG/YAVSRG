using Newtonsoft.Json;

namespace Prelude.Gameplay.Charts.YAVSRG
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
