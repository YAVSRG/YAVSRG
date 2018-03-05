using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    class ComboDisplay : Widget
    {
        AnimationSlider size;
        Func<int> get;

        public ComboDisplay(Func<int> getter)
        {
            size = new AnimationSlider(30);
            get = getter;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            int val = get();
            SpriteBatch.DrawCentredText(val.ToString(), size + val * 0.01f, left, top, System.Drawing.Color.White);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            size.Update();
        }
    }
}
