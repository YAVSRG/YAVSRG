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
            public string name;
            public Action callback;

            public NamedTask(Action a, string name, Action callback)
            {
                this.name = name;
                t = new Task(a, token.Token);
            }

            public void Start()
            {
                t.Start();
            }

            public void Cancel()
            {
                token.Cancel();
            }

            public TaskStatus Status
            {
                get { return t.Status; }
            }
        }

        public List<NamedTask> tasks;

        public TaskManager()
        {
            tasks = new List<NamedTask>();
        }

        public void AddTask(NamedTask t)
        {
            tasks.Add(t);
            t.Start();
        }

        public void Stop()
        {
            foreach (NamedTask t in tasks)
            {
                t.Cancel();
            }
        }
    }
}
