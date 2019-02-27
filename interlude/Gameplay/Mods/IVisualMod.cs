using System.Collections.Generic;
using OpenTK;
using YAVSRG.Graphics;
using YAVSRG.Interface;

namespace YAVSRG.Gameplay.Mods
{
    public class IVisualMod
    {
        public enum ObjectType
        {
            Backdrop,
            Receptor,
            Note,
            Head,
            Tail,
            Mine
        }
        //Bounds for the playfield
        protected Rect Bounds;
        protected int Keys;

        public IVisualMod(Rect bounds, int keys)
        {
            Bounds = bounds;
            Keys = keys;
        }

        //Can be used to update any animation
        public virtual void Update() { }

        //Returns the draw location for a given playfield object
        public virtual Plane PlaceObject(int Column, float Position, ObjectType Type) { return new Plane(); }

        //Draws a section of a hold note
        public virtual IEnumerable<Plane> DrawHold(int Column, float Start, float End) { yield return new Plane(); }

        //Gives the height of the playfield in pixels
        //The note renderer will stop drawing notes this distance away from receptors as they will be off screen
        public virtual int Height { get { return (int)Bounds.Height; } }
    }
}
