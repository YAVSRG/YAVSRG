using YAVSRG.Interface.Widgets;
using YAVSRG.IO;
using YAVSRG.Graphics;

namespace YAVSRG.Interface.Screens
{
    class ScreenLobby : Screen
    {
        Widget hostButton;
        string lobbyCode = "";

        public ScreenLobby()
        {
            AddChild(new FramedButton("Disconnect", Game.Multiplayer.Disconnect).PositionBottomRight(300, 100, AnchorType.MIN, AnchorType.MIN));
            AddChild(hostButton = new FramedButton("Host a lobby", Game.Multiplayer.HostLobby).PositionBottomRight(300, 100, AnchorType.MIN, AnchorType.MIN));
            AddChild(new TextEntryBox((s) => { lobbyCode = s; }, () => { return lobbyCode; }, () => { }, () => { Game.Multiplayer.JoinLobby(lobbyCode); }, () => { return "Press " + Game.Options.General.Binds.Search.ToString().ToUpper() + " to enter lobby code..."; })
     .PositionTopLeft(-250, 20, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(250, 80, AnchorType.CENTER, AnchorType.MIN));
            AddChild(new BoolPicker("Play charts together", Game.Multiplayer.SyncCharts, (v) => { Game.Multiplayer.SyncCharts = v; }).PositionTopLeft(-50, 300, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(50, 350, AnchorType.CENTER, AnchorType.MIN));
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            if (Game.Multiplayer.Hosting)
            {
                if (Game.Multiplayer.LobbyKey != "")
                {
                    SpriteBatch.Font1.DrawCentredText("Your lobby code is: " + Game.Multiplayer.LobbyKey, 30f, 0, bounds.Top + 100, Game.Options.Theme.MenuFont, true, Game.Screens.DarkColor);
                    SpriteBatch.Font1.DrawCentredText("Press Ctrl+C to put it on the clipboard", 20f, 0, bounds.Top + 150, Game.Options.Theme.MenuFont, true, Game.Screens.DarkColor);
                }
                float y = bounds.Top + 150;
                for (int i = 0; i < Game.Multiplayer.Clients.Length; i++)
                {
                    if (Game.Multiplayer.Clients[i]?.LoggedIn == true)
                    {
                        SpriteBatch.DrawRect(new Rect(bounds.Left + 10, y, bounds.Left + 310, y + 40), i == Game.Multiplayer.Server.ChartPicker ? System.Drawing.Color.FromArgb(127, Game.Screens.BaseColor) : System.Drawing.Color.FromArgb(127, 0, 0, 0));
                        SpriteBatch.Font1.DrawText(Game.Multiplayer.Clients[i].Username, 25f, bounds.Left + 20, y, Game.Options.Theme.MenuFont, true);
                        y += 50;
                    }
                }
                SpriteBatch.Font2.DrawParagraph("Chart picker is highlighted.\nLeft click to give someone chart picker role.\nRight click to kick someone.\nVery much WIP lol", 20f, new Rect(-300, 0, 300, 600), Game.Options.Theme.MenuFont);
            }
            SpriteBatch.Font1.DrawJustifiedText("Multiplayer Lobby BETA", 30f, bounds.Right - 10, bounds.Bottom - 60, Game.Options.Theme.MenuFont);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (!Game.Multiplayer.Connected)
            {
                hostButton.SetState(WidgetState.NORMAL);
            }
            else
            {
                hostButton.SetState(WidgetState.DISABLED);
                if (Game.Multiplayer.Hosting)
                {
                    if (Input.KeyPress(OpenTK.Input.Key.ControlLeft) && Input.KeyTap(OpenTK.Input.Key.C))
                    {
                        System.Windows.Forms.Clipboard.SetText(Game.Multiplayer.LobbyKey);
                    }
                    float y = bounds.Top + 150;
                    for (int i = 0; i < Game.Multiplayer.Clients.Length; i++)
                    {
                        if (Game.Multiplayer.Clients[i]?.LoggedIn == true)
                        {
                            if (ScreenUtils.MouseOver(new Rect(bounds.Left + 10, y, bounds.Left + 310, y + 40)))
                            {
                                if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                                {
                                    Game.Multiplayer.Server.ChartPicker = i;
                                }
                                else if (Input.MouseClick(OpenTK.Input.MouseButton.Right))
                                {
                                    Game.Multiplayer.Server.Kick("Manually kicked by host", i);
                                }
                            }
                            y += 50;
                        }
                    }
                }
            }
        }
    }
}
