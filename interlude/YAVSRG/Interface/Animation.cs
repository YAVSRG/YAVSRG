using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface
{
    //Handles an animation of a widget
    public class Animation
    {
        //Returns true if the animation has completed its purpose and is no longer needed in memory.
        //Animations that handle groups of child animations use this to remove finished animations from the collection.
        public virtual bool DisposeMe { get { return false; } }

        //Returns true if the animation is doing something and false if it is inactive
        //This is not just the inverse of DisposeMe as animations that handle groups of child animations may want to persist so that new animations can be added later on
        //Todo: maybe revise so it really is just the inverse of DisposeMe
        public virtual bool Running { get { return true; } }

        //Called every frame to do the action that this animation actually does
        public virtual void Update() { }
    }
}
