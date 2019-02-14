using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK;

namespace YAVSRG.Gameplay.Mods
{
    public class IVisualMod
    {
        //Can be used to update any animation
        public virtual void Update() { }

        //Evaluates gameplay coordinate to turn into screen coordinate
        public virtual Vector2 GetCoord(float x, float y) { return new Vector2(x, y); }

        //Gives the height of the playfield in pixels
        //The note renderer will stop drawing notes this distance away from receptors as they will be off screen
        public readonly int Height;

        //Flag to indicate if this visual mod requires dividing things into little strips to curve them along the path notes take
        public readonly bool CurvedRender;
    }
}
