using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Editor
{
    public class EditorData
    {
        public List<Bookmark> Bookmarks = new List<Bookmark>();
    }

    public class Bookmark
    {
        public float From;
        public float To;
        public string Comment;
    }
}
