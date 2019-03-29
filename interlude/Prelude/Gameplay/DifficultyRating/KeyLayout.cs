using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.DifficultyRating
{
    public class KeyLayout
    {
        public enum Layout
        {
            Spread,
            OneHand,
            LeftOne,
            RightOne,
            LeftTwo,
            RightTwo,
            BMSLeft,
            BMSRight
        }

        public static Dictionary<Layout, KeyLayout>[] LAYOUTS = new Dictionary<Layout, KeyLayout>[] { //store of available key layouts for each mode
            null,null,null,
            new Dictionary<Layout, KeyLayout>
            { //3k
                { Layout.OneHand, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }) } } },
                { Layout.LeftOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1 }), new Hand(new List<int> { 2 }) } } },
                { Layout.RightOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0 }), new Hand(new List<int> { 1, 2 }) } } }
            },
            new Dictionary<Layout, KeyLayout>
            { //4k
                { Layout.Spread, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1 }), new Hand(new List<int> { 2, 3 }) } } },
                { Layout.OneHand, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }) } } },
                { Layout.LeftOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 3 }) } } },
                { Layout.RightOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0 }), new Hand(new List<int> { 1, 2, 3 }) } } }
            },
            new Dictionary<Layout, KeyLayout>
            { //5k
                { Layout.LeftOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 3, 4 }) } } },
                { Layout.RightOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1 }), new Hand(new List<int> { 2, 3, 4 }) } } },
                { Layout.OneHand, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3, 4 }) } } },
                { Layout.LeftTwo, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }), new Hand(new List<int> { 4 }) } } },
                { Layout.RightTwo, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0 }), new Hand(new List<int> { 1, 2, 3, 4 }) } } }
            },
            new Dictionary<Layout, KeyLayout>
            { //6k
                { Layout.Spread, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 3, 4, 5 }) } } },
                { Layout.LeftOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }), new Hand(new List<int> { 4, 5 }) } } },
                { Layout.RightOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1 }), new Hand(new List<int> { 2, 3, 4, 5 }) } } },
                { Layout.LeftTwo, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3, 4 }), new Hand(new List<int> { 5 }) } } },
                { Layout.RightTwo, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0 }), new Hand(new List<int> { 1, 2, 3, 4, 5 }) } } }
            },
            new Dictionary<Layout, KeyLayout>
            { //7k
                { Layout.LeftOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }), new Hand(new List<int> { 4, 5, 6 }) } } },
                { Layout.RightOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 3, 4, 5, 6 }) } } },
                { Layout.BMSLeft, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 3, 2 }), new Hand(new List<int> { 4, 5, 6 }) } } },
                { Layout.BMSRight, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 4, 3, 5, 6 }) } } },
            },
            new Dictionary<Layout, KeyLayout>
            { //8k
                { Layout.Spread, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }), new Hand(new List<int> { 4, 5, 6, 7 }) } } },
                { Layout.LeftOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3, 4 }), new Hand(new List<int> { 5, 6, 7 }) } } },
                { Layout.RightOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 3, 4, 5, 6, 7 }) } } },
            },
            new Dictionary<Layout, KeyLayout>
            { //9k
                { Layout.LeftOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3, 4 }), new Hand(new List<int> { 5, 6, 7, 8 }) } } },
                { Layout.RightOne, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }), new Hand(new List<int> { 4, 5, 6, 7, 8 }) } } },
            },
            new Dictionary<Layout, KeyLayout>
            { //10k
                { Layout.Spread, new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3, 4 }), new Hand(new List<int> { 5, 6, 7, 8, 9 }) } } },
            }
        };

        public class Hand //model of a hand
        {
            List<int> fingers; //ordered list of columns covered by this hand

            public int GetFingerPosition(int column) //used to work out how many fingers apart two columns are (matters for BMS layout)
            {
                return fingers.IndexOf(column);
            }

            public Hand(List<int> f) //constructor
            {
                fingers = f;
            }

            public ushort Mask() //creates a bit mask to use with snaps to isolate notes involving this hand
            {
                ushort r = 0;
                foreach (int f in fingers)
                {
                    r += (ushort)(1 << f);
                }
                return r;
            }

            public int Count
            {
                get { return fingers.Count; }
            }
        }

        public List<Hand> hands; //a key layout is just a list of hands (unordered)

        public static string GetLayoutName(Layout layout, int keys)
        {
            bool even = keys % 2 == 0;
            switch (layout)
            {
                case Layout.Spread:
                    return "Spread";
                case Layout.OneHand:
                    return "One-Handed";
                case Layout.LeftOne:
                    return (keys / 2 + 1).ToString() + "k+" + (keys - keys / 2 - 1).ToString();
                case Layout.RightOne:
                    return (keys / 2 - (even ? 1 : 0)).ToString() + "k+" + (keys - keys / 2 + (even ? 1 : 0)).ToString();
                case Layout.LeftTwo:
                    return (keys / 2 + 2).ToString() + "k+" + (keys - keys / 2 - 2).ToString();
                case Layout.RightTwo:
                    return (keys / 2 - (even ? 2 : 1)).ToString() + "k+" + (keys - keys / 2 + (even ? 2 : 1)).ToString();
                case Layout.BMSLeft:
                    return "IIDX " + GetLayoutName(Layout.LeftOne, keys);
                case Layout.BMSRight:
                    return "IIDX " + GetLayoutName(Layout.RightOne, keys);
                default:
                    return "Unknown Layout";
            }
        }

        public static List<Layout> GetPossibleLayouts(int keys)
        {
            return LAYOUTS[keys].Keys.ToList();
        }

        public static KeyLayout GetLayout(Layout layout, int k) //static retrieval of key layout being used
        {
            if (LAYOUTS[k].ContainsKey(layout))
            {
                return LAYOUTS[k][layout];
            }
            return LAYOUTS[k].First().Value; //get default (most common) playstyle
        }
    }
}
