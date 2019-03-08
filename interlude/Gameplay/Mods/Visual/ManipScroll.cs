using OpenTK;
using System.Collections.Generic;
using YAVSRG.Graphics;
using YAVSRG.Interface;

namespace YAVSRG.Gameplay.Mods.Visual
{
    public class ManipScroll : IVisualMod
    {
        public ManipScroll(Rect bounds, int keys) : base(bounds, keys) { }

        public override Plane PlaceObject(int Column, float Position, ObjectType Type)
        {
            float left = (Column - Keys * 0.5f) * Game.Options.Theme.ColumnWidth;
            float right = left + Game.Options.Theme.ColumnWidth;
            float bottom, top;
            if (Type == ObjectType.Backdrop)
            {
                bottom = Bounds.Bottom;
                top = Bounds.Top;
            }
            else
            {
                bottom = Bounds.Bottom - ((Type == ObjectType.Receptor) ? 0 : round(Position)) - Game.Options.Profile.HitPosition;
                top = bottom - Game.Options.Theme.ColumnWidth;
            }
            return new Plane(new Vector3(left,top,0), new Vector3(right, top, 0), new Vector3(right, bottom, 0), new Vector3(left, bottom, 0));
        }

        public override IEnumerable<Plane> DrawHold(int Column, float Start, float End)
        {
            float left = (Column - Keys * 0.5f) * Game.Options.Theme.ColumnWidth;
            float right = left + Game.Options.Theme.ColumnWidth;
            float bottom = Bounds.Bottom - Start - Game.Options.Profile.HitPosition - Game.Options.Theme.ColumnWidth * 0.5f;
            float top = bottom - (End-Start);
            yield return new Plane(new Vector3(left, top, 0), new Vector3(right, top, 0), new Vector3(right, bottom, 0), new Vector3(left, bottom, 0));
        }

        float round(float x)
        {
            float t = (float)Game.Audio.Now();
            var b = Game.Gameplay.CurrentChart.Timing.BPM.GetPointAt(t, false);
            float f = (t - b.Offset) * Game.Options.Profile.ScrollSpeed;
            return x + f - (x + f) % (Game.Options.Profile.ScrollSpeed * b.MSPerBeat * 0.125f) - f;
        }
    }
}
