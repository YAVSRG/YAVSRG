using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Utilities.CommandLine
{
    public class CommandLine
    {
        public static Dictionary<string, Action> Main = new Dictionary<string, Action>
        {
            { "Cache Tools", () => { Console.WriteLine("Not yet implemented"); Console.ReadLine(); } },
            { "Chart Conversion Tools", () => { Console.WriteLine("Not yet implemented"); Console.ReadLine(); } },
            { "Skin Conversion Tools", () => { Console.WriteLine("Not yet implemented"); Console.ReadLine(); } },
        };

        public static void Menu(string title, Dictionary<string,Action> options)
        {
            while (true)
            {
                Console.Clear();
                Console.WriteLine(new string('-', Console.WindowWidth));
                Console.WriteLine(CenteredText(title));
                Console.WriteLine(new string('-', Console.WindowWidth));
                List<string> o = options.Keys.ToList();
                for (int i = 1; i < options.Count; i++)
                {
                    Console.WriteLine(i.ToString() + ". " + o[i - 1]);
                }
                Console.WriteLine("OR Enter B to go back");
                Console.WriteLine(new string('-', Console.WindowWidth));
                while (true)
                {
                    Console.Write("> ");
                    string input = Console.ReadLine().ToUpper();
                    if (input == "B")
                    {
                        return;
                    }
                    else
                    {
                        int selection = -1;
                        int.TryParse(input, out selection);
                        if (selection >= 0 && selection < options.Count)
                        {
                            options.Values.ToList()[selection + 1]();
                            break;
                        }
                    }
                    Console.WriteLine("\nInvalid option.");
                }
            }

        }

        static string CenteredText(string s)
        {
            int l = (int)Math.Ceiling((Console.WindowWidth - s.Length) / 2f);
            int r = (int)Math.Floor((Console.WindowWidth - s.Length) / 2f);
            return new string(' ', l) + s + new string(' ', r);
        }
    }
}
