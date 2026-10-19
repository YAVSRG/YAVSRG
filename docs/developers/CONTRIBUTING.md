# Contributing to Interlude

Thank you for taking the time to contribute to Interlude!

There are lots of ways to contribute, if you like the project but don't have time to contribute then you can show your appreciation by 'Starring' the repo on GitHub and telling your friends :)

## I have a question

If you have looked through the wiki and can't find an answer to your question, the best place to ask is in [the Discord](https://yavsrg.net/discord)  

## I have a bug report

The best place to report bugs is the #bug-reports channel in [the Discord](https://yavsrg.net/discord)  
If you don't or prefer not to use Discord, you can open an Issue directly on GitHub

## How to make a pull request

This guide assumes little pre-existing knowledge other than that you have written some code in a programming language before  
If you are experienced with a workflow that works for you instead, feel free to disregard or improvise upon these instructions (at your own risk)  
If these steps don't help or you are stuck, ask in [the Discord](https://yavsrg.net/discord)  

### 1. Follow the [building instructions](BUILDING.md)

### 2. Create a fork of YAVSRG on GitHub
You will need to [sign up for a GitHub account](https://github.com/login) if you don't have one

Once you are signed into your GitHub account, [create a fork of YAVSRG](https://github.com/YAVSRG/YAVSRG/fork)

This is where you can commit whatever changes you like to your own copy of the repo

### 3. Set up your own branch to work in

Once the fork is created, connect the repository on your machine to a branch on the fork:
```bash
git remote add personal https://github.com/<*YOUR GITHUB USERNAME*>/YAVSRG.git
# if unsure of a branch name, use "<*YOUR GITHUB USERNAME*>/develop"
git checkout -b <*BRANCH NAME*>
git push --set-upstream personal <*BRANCH NAME*>
```
If you are adding a particular feature you could name your branch something descriptive like `help-mode` or `scoring-bug-fix`

I **very strongly recommend** that you don't add commits to your `main` branch locally  
The branch `main` should stay connected to the original repo (`YAVSRG/YAVSRG`)  
Your branch should be connected to your fork (`<username>/YAVSRG`)  
This will make your life **much** easier when keeping your changes in sync with the main repo

In general you should only **pull** on the `main` branch to fetch updates, and never add your own commits

### 4. Make and test changes

You can now make any changes you like - make git commits as you go and you can push them to your fork on GitHub

Any commits you push will appear at `https://github/com/<*YOUR USERNAME*>/YAVSRG` and not on the main repo

When you are ready to submit your changes to be added to the main repo, you can submit a PR (next step))

### 5. Submit a PR (Pull Request)

Once you have a fix or feature ready:
- Commit all your changes to your branch
- Ensure everything builds and meets formatting checks by running `./scripts/pr.sh`
- Push your changes to GitHub
- Submit a PR by going to https://github.com/YAVSRG/YAVSRG/compare, choose `base: main` <- `compare: <*YOUR BRANCH*>` and click 'Open Pull Request'

Give your PR a title and description and submit it, it will now be open on the main repo to add/merge the changes into the main version

PRs should be focused on **one particular fix or feature**, if you have two things to change, do them on two different branches and open two PRs

Currently all PRs are reviewed by Percyqaz, I will
- Suggest any fixes or changes I think are needed and advise on next steps
- Merge the PR if nothing else is needed!

I review on weekends, so apologies if it takes me a bit of time to get to your request

### Shortened overview of the process

Start a branch for your change, new branches should be based on `YAVSRG/YAVSRG/main`
```bash
git checkout main
git pull
git checkout -b <*BRANCH NAME*>
```

Change files, write code, etc - Add these changes as commits to the branch

```bash
# See step 3 if you haven't created 'personal' as an origin
git push --set-upstream personal <*BRANCH NAME*>
```

Go to https://github.com/YAVSRG/YAVSRG/compare to open a PR