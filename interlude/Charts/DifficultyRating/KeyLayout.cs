using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.DifficultyRating
{
    public class KeyLayout
    {
        public static Dictionary<string, KeyLayout>[] LAYOUTS = new Dictionary<string, KeyLayout>[] {
            null,null,null,
            new Dictionary<string, KeyLayout>
            { //3k
                { "One Handed", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }) } } },
                { "2k + 1", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1 }), new Hand(new List<int> { 2 }) } } },
                { "1k + 2", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0 }), new Hand(new List<int> { 1, 2 }) } } }
            },
            new Dictionary<string, KeyLayout>
            { //4k
                { "Spread", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1 }), new Hand(new List<int> { 2, 3 }) } } },
                { "One Handed", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }) } } },
                { "3k + 1", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 3 }) } } },
                { "1k + 3", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0 }), new Hand(new List<int> { 1, 2, 3 }) } } }
            },
            new Dictionary<string, KeyLayout>
            { //5k
                { "3k + 2", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 3, 4 }) } } },
                { "2k + 3", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1 }), new Hand(new List<int> { 2, 3, 4 }) } } },
                { "One Handed", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3, 4 }) } } },
                { "4k + 1", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }), new Hand(new List<int> { 4 }) } } },
                { "1k + 4", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0 }), new Hand(new List<int> { 1, 2, 3, 4 }) } } }
            },
            new Dictionary<string, KeyLayout>
            { //6k
                { "Spread", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 3, 4, 5 }) } } },
                { "4k + 2", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }), new Hand(new List<int> { 4, 5 }) } } },
                { "2k + 4", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1 }), new Hand(new List<int> { 2, 3, 4, 5 }) } } },
                { "5k + 1", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3, 4 }), new Hand(new List<int> { 5 }) } } },
                { "1k + 5", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0 }), new Hand(new List<int> { 1, 2, 3, 4, 5 }) } } }
            },
            new Dictionary<string, KeyLayout>
            { //7k
                { "Keyboard/Left thumb", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }), new Hand(new List<int> { 4, 5, 6 }) } } },
                { "Keyboard/Right thumb", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 3, 4, 5, 6 }) } } },
                { "BMS/Left thumb", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 3, 2 }), new Hand(new List<int> { 4, 5, 6 }) } } },
                { "BMS/Right thumb", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 4, 3, 5, 6 }) } } },
            },
            new Dictionary<string, KeyLayout>
            { //8k
                { "Spread", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }), new Hand(new List<int> { 4, 5, 6, 7 }) } } },
                { "5k + 3", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3, 4 }), new Hand(new List<int> { 5, 6, 7 }) } } },
                { "3k + 5", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2 }), new Hand(new List<int> { 3, 4, 5, 6, 7 }) } } },
            },
            new Dictionary<string, KeyLayout>
            { //9k
                { "Left thumb", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3, 4 }), new Hand(new List<int> { 5, 6, 7, 8 }) } } },
                { "Right thumb", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3 }), new Hand(new List<int> { 4, 5, 6, 7, 8 }) } } },
            },
            new Dictionary<string, KeyLayout>
            { //10k
                { "Spread", new KeyLayout() { hands = new List<Hand> { new Hand(new List<int> { 0, 1, 2, 3, 4 }), new Hand(new List<int> { 5, 6, 7, 8, 9 }) } } },
            }
        };

        public class Hand
        {
            List<int> fingers;

            public int GetFingerPosition(int column)
            {
                return fingers.IndexOf(column);
            }

            public Hand(List<int> f)
            {
                fingers = f;
            }

            public ushort Mask()
            {
                ushort r = 0;
                foreach (int f in fingers)
                {
                    r += (ushort)(1 << f);
                }
                return r;
            }
        }

        public List<Hand> hands;

        public static KeyLayout GetLayout(string name, int k)
        {
            if (LAYOUTS[k].ContainsKey(name))
            {
                return LAYOUTS[k][name];
            }
            return LAYOUTS[k].First().Value;
        }
    }
}
