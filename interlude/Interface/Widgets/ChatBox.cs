using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using System.Globalization;

namespace YAVSRG.Interface.Widgets
{
    public class ChatBox : Widget
    {
        class ChatChannel
        {
            public List<string> lines;
            public SimpleButton button;

            public ChatChannel(SimpleButton b)
            {
                lines = new List<string>();
                button = b;
            }
        }


        bool collapsed
        {
            get { return TopLeft.AbsoluteTargetY == 80; }
        }

        Dictionary<string, ChatChannel> channels;
        string selectedChannel = "";
        string entryText = "";
        int newMessages = 0;
        ScrollContainer channelSelector;
        Animations.AnimationSlider newMsgFade;

        public ChatBox()
        {
            channels = new Dictionary<string, ChatChannel>();
            channelSelector = new ScrollContainer(10f, 10f, false);
            AddChild(channelSelector.PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(100, 0, AnchorType.MIN, AnchorType.MAX));
            Animation.Add(newMsgFade = new Animations.AnimationSlider(0));
        }

        public override void Draw(Rect bounds)
        {
            SpriteBatch.DrawRect(bounds, Color.FromArgb((int)((Bottom(bounds)-Top(bounds))*0.25f), 0, 0, 0));
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(bounds, Color.FromArgb(180, 0, 0, 0));
            if (newMsgFade.Val > 0.01f)
            {
                int a = (int)(255 * newMsgFade.Val);
                var c = Color.FromArgb(0, 0, 0, 0);
                SpriteBatch.Draw(bounds: new Rect(bounds.Right - 1200, bounds.Top - 300, bounds.Right, bounds.Top), colors: new[] { c, c, Color.FromArgb(a, Color.Black), c });
                SpriteBatch.Font1.DrawJustifiedText("Press " + Game.Options.General.Binds.Chat.ToString() + " to view " + newMessages.ToString() + " new message" + (newMessages == 1 ? "" : "s"), 30f, bounds.Right, bounds.Top - 50, Color.FromArgb(a,Game.Options.Theme.MenuFont));
            }
            if (collapsed)
            {
                
            }
            else
            {
                DrawWidgets(bounds);
                if (selectedChannel != "")
                {
                    var l = channels[selectedChannel].lines;
                    int c = Math.Min(l.Count, (int)(bounds.Height / 25 - 2));
                    for (int i = 0; i < c; i++)
                    {
                        RenderText(l[i], bounds.Left + 120, bounds.Bottom - 70 - 25 * i);
                    }
                }
                SpriteBatch.Font1.DrawText("> " + entryText, 20f, bounds.Left + 120, bounds.Bottom - 40, Game.Options.Theme.MenuFont);
                ScreenUtils.DrawFrame(bounds, 30f, Game.Screens.HighlightColor);
            }
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (collapsed)
            {
                if (Input.KeyTap(Game.Options.General.Binds.Chat) && Game.Screens.Toolbar.Height > 0)
                {
                    Move(new Rect(0, 480, 0, 80));
                    Input.ChangeIM(new InputMethod((s) => { entryText = s; }, () => { return entryText; }, () => { }));
                    newMessages = 0;
                    newMsgFade.Target = 0;
                }
            }
            else
            {
                if (Input.KeyTap(Game.Options.General.Binds.Chat, true))
                {
                    Move(new Rect(0, 80, 0, 80));
                    Input.ChangeIM(null);
                }
                else if (entryText != "" && Input.KeyTap(OpenTK.Input.Key.Enter, true))
                {
                    SendMessage(selectedChannel, entryText);
                    entryText = "";
                }
                if (ScreenUtils.CheckButtonClick(bounds))
                {
                    Input.ChangeIM(new InputMethod((s) => { entryText = s; }, () => { return entryText; }, () => { }));
                }
            }
        }

        public void AddLine(string channel, string text)
        {
            if (!channels.ContainsKey(channel))
            {
                CreateChannel(channel);
            }
            var l = channels[channel];
            l.lines.Insert(0,text); //no limit to chat history
            //if i want one, put it here
            if (collapsed)
            {
                newMessages += 1;
                newMsgFade.Target = 1;
            }
        }

        void RenderText(string text, float x, float y)
        {
            int index = 0;
            Color c = Color.White;
            for (int i = index; i < text.Length; i++)
            {
                if (text[i] == '{')
                {
                    x += SpriteBatch.Font2.DrawText(text.Substring(index, i - index), 20f, x, y, c);
                    index = i;
                }
                else if (text[i] == '}')
                {
                    string[] parse = text.Substring(index + 1, i - index - 1).Split(':');
                    bool valid = false;
                    if (parse.Length > 1)
                    {
                        if (parse[0] == "c")
                        {
                            int argb = -1;
                            int.TryParse(parse[1], NumberStyles.HexNumber, CultureInfo.InvariantCulture, out argb);
                            c = Color.FromArgb(255,Color.FromArgb(argb));
                            valid = true;
                        }
                        else if (parse[0] == "e")
                        {
                            int id = 0;
                            int.TryParse(parse[1], out id);
                            SpriteBatch.Draw("emoji", new Rect(x, y, x + 35, y + 35), c, id, 0, 0);
                            x += 35;
                            valid = true;
                        }
                    }
                    if (!valid)
                    {
                        x += SpriteBatch.Font2.DrawText(text.Substring(index, i - index), 20f, x, y, c);
                    }
                    index = i + (valid ? 1 : 0);
                }
            }
            x += SpriteBatch.Font2.DrawText(text.Substring(index), 20f, x, y, c);
        }

        void SendMessage(string channel, string msg)
        {
            if (channel == "Lobby" && Game.Multiplayer.Connected)
            {
                Game.Multiplayer.SendMessage(msg);
            }
        }

        void CreateChannel(string channel)
        {
            channels.Add(channel, new ChatChannel(new SimpleButton(channel, () => { selectedChannel = channel; }, () => { return selectedChannel == channel; }, 20f)));
            channelSelector.AddChild(channels[channel].button.PositionBottomRight(80, 40, AnchorType.MIN, AnchorType.MIN));
            if (selectedChannel == "") { selectedChannel = channel; }
        }

        void RemoveChannel(string channel)
        {
            channelSelector.Items().Remove(channels[channel].button);
            channels.Remove(channel);
            if (channel == selectedChannel) { selectedChannel = ""; }
            //the data just gets garbage collected
        }
    }
}
