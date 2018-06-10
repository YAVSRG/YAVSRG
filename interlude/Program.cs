using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using YAVSRG.Interface;

namespace YAVSRG
{
    class Program
    {
        static void Main(string[] args)
        {
            Mutex m = new Mutex(true, "Interlude");
            if (m.WaitOne(TimeSpan.Zero, true))
            {
                Game g = new Game();
                Utilities.Logging.SetLogAction((s, t) => { });
                try
                {
                    g.Run(120.0); //run the game
                }
                catch (Exception e)
                {
                    g.Exit(); //if it crashes close it and give a neat crash log
                    Application.Run(new CrashWindow(e.ToString()));
                }
                finally
                {
                    g.Exit();
                    g.Dispose(); //clean up resources. i don't know if there's anything left to clean up but it's here i guess
                }
                m.ReleaseMutex();
            }
        }
    }
}
