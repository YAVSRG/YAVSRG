using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay
{
    //stores how a player hit a row of notes (since notes are stored row by row in interlude chart format)
    //an array of these makes up the hitdata for a score
    public class HitData : OffsetItem //todo: better name
    {
        //array of signed ms delta of notes hit
        public float[] delta;

        //array of bytes representing hit state of notes on this row
        //0 = no note here OR no note required to be hit (e.g. the no long note releases mod sets ln ends to 0s and they are not considered)
        //1 = note here and has not been hit (yet, but also 1 can represent not hit after the fact)
        //2 = hit (updating delta should not be allowed)
        //Todo: make this an enum allowing for special hits like mines and holding long notes
        public byte[] hit;

        //creates the data structure
        public HitData(GameplaySnap s, int keycount) : base(s.Offset)
        {
            hit = new byte[keycount];
            //sets up hit array according to corresponding gameplay snap
            foreach (int k in s.Combine().GetColumns())
            {
                hit[k] = 1;
            }
            delta = new float[keycount];
        }

        //number of notes needing to be hit
        public int Count
        {
            get
            {
                int x = 0;
                for (int i = 0; i < hit.Length; i++)
                {
                    if (hit[i] > 0)
                    {
                        x++;
                    }
                }
                return x;
            }
        }
    }
}
