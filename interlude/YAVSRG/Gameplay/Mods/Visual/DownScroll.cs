using OpenTK;
using System.Collections.Generic;
using Interlude.Graphics;
using Interlude.Interface;

namespace Interlude.Gameplay.Mods.Visual
{
    public class DownScroll : IVisualMod
    {
        public DownScroll(Rect bounds, int keys) : base(bounds, keys) { }

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
                bottom = Bounds.Bottom - Position - Game.Options.Profile.HitPosition;
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
    }
}
