using System.Linq;
using YAVSRG.Gameplay.Charts.YAVSRG;
using System.IO;

namespace YAVSRG.Editor
{
    public class ChartInEditor : Chart
    {
        public EditorData EditorData;

        public ChartInEditor(Chart from) : base(from.Notes.Points, from.Data, from.Keys)
        {
            try
            {
                EditorData = Utils.LoadObject<EditorData>(GetDataPath());
            }
            catch
            {
                EditorData = new EditorData();
            }
        }

        public string GetDataPath()
        {
            return Path.Combine("Data", "Editor", new string(GetFileIdentifier().Where(c => char.IsLetterOrDigit(c) || char.IsWhiteSpace(c) || c == '-').ToArray()) + ".json");
        }
    }
}
