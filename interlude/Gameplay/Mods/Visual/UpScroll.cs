using OpenTK;
using System.Collections.Generic;
using Interlude.Graphics;
using Interlude.Interface;

namespace Interlude.Gameplay.Mods.Visual
{
    public class UpScroll : IVisualMod
    {
        public UpScroll(Rect bounds, int keys) : base(bounds, keys) { }

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
                top = Bounds.Top + Position + Game.Options.Profile.HitPosition;
                bottom = top + Game.Options.Theme.ColumnWidth;
            }
            return new Plane(new Vector3(left,top,0), new Vector3(right, top, 0), new Vector3(right, bottom, 0), new Vector3(left, bottom, 0)).Rotate(Type== ObjectType.Tail ? 2 : 0);
        }

        public override IEnumerable<Plane> DrawHold(int Column, float Start, float End)
        {
            float left = (Column - Keys * 0.5f) * Game.Options.Theme.ColumnWidth;
            float right = left + Game.Options.Theme.ColumnWidth;
            float top = Bounds.Top + Start + Game.Options.Profile.HitPosition + Game.Options.Theme.ColumnWidth * 0.5f;
            float bottom = top + (End-Start);
            yield return new Plane(new Vector3(left, top, 0), new Vector3(right, top, 0), new Vector3(right, bottom, 0), new Vector3(left, bottom, 0));
        }
    }
}
