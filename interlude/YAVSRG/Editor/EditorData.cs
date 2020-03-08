using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Interlude.Editor
{
    public class EditorData : ChartHeader
    {
        public List<Bookmark> Bookmarks = new List<Bookmark>();
        public float CursorPosition;
        public int CurrentLayer;

        public static EditorData FromChart(ChartHeader data)
        {
            return new EditorData()
            {
                Artist = data.Artist,
                AudioFile = data.AudioFile,
                BGFile = data.BGFile,
                Creator = data.Creator,
                DiffName = data.DiffName,
                File = Path.ChangeExtension(data.File, ".chart"),
                PreviewTime = data.PreviewTime,
                SourcePack = data.SourcePack,
                SourcePath = data.SourcePath,
                Title = data.Title
            };
        }
    }

    public class Bookmark
    {
        public float From;
        public float To;
        public string Comment;
    }
}
