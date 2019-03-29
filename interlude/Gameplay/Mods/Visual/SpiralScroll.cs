using System;
using System.Collections.Generic;
using OpenTK;
using Interlude.Graphics;
using Interlude.Interface;

namespace Interlude.Gameplay.Mods.Visual
{
    public class SpiralScroll : IVisualMod
    {
        public SpiralScroll(Rect bounds, int keys) : base(bounds, keys) { }

        public override Plane PlaceObject(int Column, float Position, ObjectType Type)
        {
            float scale = 0.002f;
            float amount = Game.Options.Theme.ColumnWidth * Utils.GetBeat(2);
            if (Type == ObjectType.Backdrop)
            {
                return default(Plane);
            }
            float left = (Column - Keys * 0.5f) * Game.Options.Theme.ColumnWidth + (float)Math.Sin(Position * scale + Column) * amount * Position/Height;
            float right = left + Game.Options.Theme.ColumnWidth;
            float bottom = Bounds.Bottom - Game.Options.Profile.HitPosition - Position;
            float top = bottom - Game.Options.Theme.ColumnWidth;
            return new Plane(new Vector3(left, top, 0), new Vector3(right, top, 0), new Vector3(right, bottom, 0), new Vector3(left, bottom, 0));
        }

        public override IEnumerable<Plane> DrawHold(int Column, float Start, float End)
        {
            float scale = 0.002f;
            float amount = Game.Options.Theme.ColumnWidth * Utils.GetBeat(2);
            int spacing = 40;
            float x = Start;
            while (End-x > spacing)
            {
                float x1 = (Column - Keys * 0.5f) * Game.Options.Theme.ColumnWidth + (float)Math.Sin((x + spacing) * scale + Column) * amount * (x + spacing) / Height;
                float x2 = (Column - Keys * 0.5f) * Game.Options.Theme.ColumnWidth + (float)Math.Sin((x) * scale + Column) * amount * (x) / Height;
                yield return new Plane(new Vector3(x1, func(x + spacing), 0), new Vector3(x1 + Game.Options.Theme.ColumnWidth, func(x + spacing), 0), new Vector3(x2 + Game.Options.Theme.ColumnWidth, func(x), 0), new Vector3(x2, func(x), 0));
                x += spacing;
            }
            float f1 = (Column - Keys * 0.5f) * Game.Options.Theme.ColumnWidth + (float)Math.Sin((x) * scale + Column) * amount * (x) / Height;
            float f2 = (Column - Keys * 0.5f) * Game.Options.Theme.ColumnWidth + (float)Math.Sin(End * scale + Column) * amount * End / Height;
            yield return new Plane(new Vector3(f1, func(x), 0), new Vector3(f1 + Game.Options.Theme.ColumnWidth, func(x), 0), new Vector3(f2 + Game.Options.Theme.ColumnWidth, func(End), 0), new Vector3(f2, func(End), 0));
        }

        private float func(float x)
        {
            return Bounds.Bottom - Game.Options.Profile.HitPosition - x - Game.Options.Theme.ColumnWidth * 0.5f;
        }
    }
}
