using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface
{
    //Represents a collection of widgets making up the whole display of the game.
    //The game will switch through several screens during use which divides different functions into different parts e.g. one screen you see when playing, one screen you see to select levels etc.
    public class Screen : Widget
    {
        //Called when a screen is being entered
        //prev is the screen that was previously being displayed
        //prev.OnExit(this) is called BEFORE this.OnEnter(prev)
        public virtual void OnEnter(Screen prev) { }

        //Called when this screen is being exited for another
        //this.OnExit(next) is called BEFORE next.OnEnter(this)
        public virtual void OnExit(Screen next) { Dispose(); }

        //Overrides widget bounds behaviour; The bounds of a screen fill the whole game window
        public override Rect GetBounds()
        {
            return GetBounds(ScreenUtils.Bounds.ExpandY(-Game.Screens.Toolbar.Height));
        }
    }
}
