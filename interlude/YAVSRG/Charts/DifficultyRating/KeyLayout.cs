using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.DifficultyRating
{
    public class KeyLayout
    {
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

        public KeyLayout(int k)
        {
            hands = new List<Hand>();
            if (k == 4)
            {
                hands.Add(new Hand(new List<int> { 0, 1 }));
                hands.Add(new Hand(new List<int> { 2, 3 }));
            }
            else if (k == 5)
            {
                hands.Add(new Hand(new List<int> { 0, 1, 2 }));
                hands.Add(new Hand(new List<int> { 3, 4 }));
            }
            else if (k == 6)
            {
                hands.Add(new Hand(new List<int> { 0, 1, 2 }));
                hands.Add(new Hand(new List<int> { 3, 4, 5 }));
            }
            else if (k == 7)
            {
                hands.Add(new Hand(new List<int> { 0, 1, 2, 3 }));
                hands.Add(new Hand(new List<int> { 4, 5, 6 }));
            }
            else if (k == 8)
            {
                hands.Add(new Hand(new List<int> { 0, 1, 2, 3 }));
                hands.Add(new Hand(new List<int> { 4, 5, 6, 7 }));
            }
            else if (k == 9)
            {
                hands.Add(new Hand(new List<int> { 0, 1, 2, 3, 4 }));
                hands.Add(new Hand(new List<int> { 5, 6, 7, 8 }));
            }
        }
    }
}
