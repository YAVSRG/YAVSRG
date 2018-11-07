using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Threading;

namespace YAVSRG.Utilities
{
    public class TaskManager
    {
        public class NamedTask
        {
            CancellationTokenSource token = new CancellationTokenSource();
            Task t;
            Action callback;
            public string Name;

            public NamedTask(Action a, string name, Action callback)
            {
                Name = name;
                this.callback = callback; //not used
                t = new Task(a, token.Token);
            }

            public void Start()
            {
                t.Start();
            }

            public void Cancel()
            {
                token.Cancel();
                if (t.Exception != null)
                {
                    Logging.Log("Exception in task " + Name + ": " + t.Exception.ToString());
                }
            }

            public TaskStatus Status
            {
                get { return t.Status; }
            }
        }

        public List<NamedTask> Tasks;

        public TaskManager()
        {
            Tasks = new List<NamedTask>();
        }

        public void AddTask(NamedTask t)
        {
            Tasks.Add(t);
            if (t.Name != "")
            {
                Game.Screens.Toolbar.Chat.AddLine("Tasks", "Added task: " + t.Name, true);
            }
            t.Start();
        }

        public void AddTask(Action a, string Name)
        {
            AddTask(new NamedTask(a, "", () => { }));
        }

        public void AddAnonymousTask(Action a)
        {
            new NamedTask(a, "", () => { }).Start();
        }

        public void StopAll()
        {
            foreach (NamedTask t in Tasks)
            {
                t.Cancel();
            }
        }

        public bool HasTasksRunning()
        {
            foreach (NamedTask t in Tasks)
            {
                if (t.Status == TaskStatus.Running)
                {
                    return true;
                }
            }
            return false;
        }
    }
}
