using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface;
using YAVSRG.Interface.Animations;
using YAVSRG.Gameplay.Mods;
using YAVSRG.Gameplay.Charts.YAVSRG;

namespace YAVSRG.Gameplay
{
    public class NoteRenderer : Widget
    {
        float[] holds;
        BinarySwitcher holdMiddles, bugFix;
        BinarySwitcher holdsInHitpos = new BinarySwitcher(0);
        int[] holdColors, holdColorsHitpos, svindex;
        float[] pos, time, sv;

        ChartWithModifiers Chart;
        protected int HitPos { get { return Game.Options.Profile.HitPosition; } }
        protected int ColumnWidth { get { return Game.Options.Theme.ColumnWidth; } }
        protected float ScrollSpeed { get { return Game.Options.Profile.ScrollSpeed / (float)Game.Options.Profile.Rate; } }
        protected int Height;

        protected AnimationCounter animation;

        public NoteRenderer(ChartWithModifiers chart, IVisualMod mod)
        {
            Game.Options.Theme.LoadTextures(Chart.Keys);

            Animation.Add(animation = new AnimationCounter(25, true));

            //i make all this stuff ahead of time so i'm not creating a ton of new arrays/recalculating the same thing/sending stuff to garbage every frame
            holds = new float[Chart.Keys];
            holdMiddles = new BinarySwitcher(0);
            bugFix = new BinarySwitcher(0);
            holdColors = new int[Chart.Keys];
            holdColorsHitpos = new int[Chart.Keys];
            pos = new float[Chart.Keys];
            time = new float[Chart.Keys];
            sv = new float[Chart.Keys + 1];
            svindex = new int[Chart.Keys + 1];
        }
    }
}
