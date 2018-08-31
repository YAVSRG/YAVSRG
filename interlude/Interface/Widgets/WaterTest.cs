using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    class WaterTest : Widget
    {
        float[,] data;
        Animations.AnimationCounter clock;

        public WaterTest()
        {
            data = new float[50, 50];
            Animation.Add(clock = new Animations.AnimationCounter(120, true));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            /*
            float w = (right - left) / 49;
            float h = (bottom - top) / 49;
            for (int x = 0; x < 49; x++)
            {
                for (int y = 0; y < 49; y++)
                {
                    SpriteBatch.Draw("", left + w * x, top + h * y, left + w + w * x, top + h + h * y, colors: new System.Drawing.Color[] {
                        System.Drawing.Color.FromArgb((int)(data[x,y]+127),255,255,255),
                        System.Drawing.Color.FromArgb((int)(data[x+1,y]+127),255,255,255),
                        System.Drawing.Color.FromArgb((int)(data[x+1,y+1]+127),255,255,255),
                        System.Drawing.Color.FromArgb((int)(data[x,y+1]+127),255,255,255),
                    });
                }
            }*/
            float w = (right - left) / 50;
            float h = (bottom - top) / 50;
            for (int x = 0; x < 50; x++)
            {
                for (int y = 0; y < 50; y++)
                {
                    SpriteBatch.Draw("", left + w * x, top + h * y, left + w + w * x, top + h + h * y, color: System.Drawing.Color.FromArgb((int)(127 + data[x, y]), 255, 255, 255));
                }
            }

        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            if (Input.KeyPress(OpenTK.Input.Key.Space, true))
            {
                data[25, 25] = 100;
            }
            //if (clock.value == 50)
            {
                float[,] old = new float[50, 50]; Array.Copy(data, 0, old, 0, data.Length);
                for (int x = 0; x < 50; x++)
                {
                    for (int y = 0; y < 50; y++)
                    {
                        data[x, y] = (
                            (x < 49 ? old[x + 1, y] : 0) +
                            (x > 0 ? old[x - 1, y] : 0) +
                            (y < 49 ? old[x, y + 1] : 0) +
                            (y > 0 ? old[x, y - 1] : 0)
                            ) * 0.5f - data[x, y];
                        if (data[x,y] < 0)
                        {
                            data[x, y] = 0;
                        }
                        data[x, y] *= 0.9f;
                        data[x, y] = Math.Max(-120, Math.Min(120, data[x, y]));
                    }
                }
            }
        }
    }
}
