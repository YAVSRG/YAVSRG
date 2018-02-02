using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class InfoCard : Button
    {
        bool alternateInfo;
        protected string[] info;

        public InfoCard() : base("infocard","", null)
        {
            action = Toggle;
        }

        public void Toggle()
        {
            alternateInfo = !alternateInfo;
        }

        private void DrawRow(string a, string b, float left, float top, float right, float bottom)
        {
            SpriteBatch.DrawCentredTextToFill(a, left, top, (left + right) / 2, bottom, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawCentredTextToFill(b, (left + right) / 2, top, right, bottom, Game.Options.Theme.MenuFont);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            DrawRow(info[0], info[1], left, top, right, top + 100);
            for (int i = 1; i < 6; i++)
            {
                DrawRow(info[i * 2], info[i * 2 + 1], left, top + 40 + i * 60, right, top + 100 + i * 60);
            }
            DrawRow(info[12], info[13], left, top + 400, right, top + 500);
            /* this all needs to be redone any way
            SpriteBatch.DrawTextToFill(info[0], X.Val, Y.Val + 5, X.Val + 250, Y.Val + 95, System.Drawing.Color.White);
            SpriteBatch.DrawTextToFill(info[1], X.Val + 250, Y.Val + 5, X.Val + 500, Y.Val + 95, System.Drawing.Color.White);
            for (int i = 1; i < 7; i++)
            {
                SpriteBatch.DrawTextToFill(info[i * 3-1], X.Val+150, Y.Val + i * 100+30, X.Val+350, Y.Val + i * 100+70, System.Drawing.Color.White);
                SpriteBatch.DrawTextToFill(info[i * 3], X.Val, Y.Val + i * 100+5, X.Val + 150, Y.Val + i * 100 + 95, System.Drawing.Color.White);
                SpriteBatch.DrawTextToFill(info[i * 3+1], X.Val+350, Y.Val + i * 100+5, X.Val + 500, Y.Val + i * 100 + 95, System.Drawing.Color.White);
            }
            SpriteBatch.DrawTextToFill(info[20], X.Val, Y.Val + 705, X.Val + 250, Y.Val + 795, System.Drawing.Color.White);
            SpriteBatch.DrawTextToFill(info[21], X.Val + 250, Y.Val + 705, X.Val + 500, Y.Val + 795, System.Drawing.Color.White);*/
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
        }
    }
}
