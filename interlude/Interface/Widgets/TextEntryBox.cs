using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Interface.Widgets
{
    class TextEntryBox : Widget
    {

        bool focus;
        Animations.AnimationColorMixer color;

        Action<string> set;
        Func<string> get;
        Action update;

        public TextEntryBox(Action<string> setter, Func<string> getter, Action updater)
        {
            set = setter;
            get = getter;
            update = updater;
            Animation.Add(color = new Animations.AnimationColorMixer(Game.Screens.HighlightColor));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, top, right, bottom, Game.Screens.DarkColor);
            SpriteBatch.Font1.DrawText(get() != "" ? get() : "Press " + Game.Options.General.Binds.Search.ToString().ToUpper() + " to search...", 20f, left + 20, top + 10, color);
            SpriteBatch.DrawFrame(left, top, right, bottom, 25f, color);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            color.Target(focus ? Color.White : Game.Screens.HighlightColor);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (focus)
            {
                if (Input.KeyTap(Game.Options.General.Binds.Search, true) || !Input.HasIM() || ScreenUtils.CheckButtonClick(left, top, right, bottom))
                {
                    Input.ChangeIM(null);
                    focus = false;
                }
            }
            else
            {
                if (Input.KeyTap(Game.Options.General.Binds.Search) || ScreenUtils.CheckButtonClick(left, top, right, bottom))
                {
                    Input.ChangeIM(new InputMethod(set, get, update));
                    focus = true;
                }
            }
        }
    }
}
