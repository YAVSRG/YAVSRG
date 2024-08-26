# How to contribute to this repository

This guide assumes no pre-existing knowledge other than that you have written some code in a programming language before

If you are familiar with Git, GitHub or a workflow that works for you instead, feel free to disregard or improvise upon these instructions

If these steps don't help or you are stuck, ask in [the Discord](https://yavsrg.net/discord)

### 1. Get the code on your machine
See the [guide to building Interlude as a developer in README.md](https://github.com/YAVSRG/YAVSRG?tab=readme-ov-file#-building-interlude-for-developers-only)

Now you can successfully build and run Interlude with any changes you've made to the codebase!

### 2. Create a fork of YAVSRG on GitHub

You will need to [sign up for a GitHub account](https://github.com/login) if you don't have one

Once you are signed into your GitHub account, [create a fork of YAVSRG](https://github.com/YAVSRG/YAVSRG/fork)

This is where you can commit whatever changes you like to your own copy of the repo

Once the fork is created, you can connect the repository you already have on your machine to a branch on the fork with these terminal commands:

```bash
# terminal needs to be in the YAVSRG folder
git remote add personal https://github.com/<*YOUR GITHUB USERNAME*>/YAVSRG.git
git checkout -b <*BRANCH NAME*>
git push --set-upstream personal <*BRANCH NAME*>
# now you can add commits and push them as you please
```

Not sure what to name your branch? You can just call it `develop` or if you are adding a particular feature you could name it something descriptive like `help-mode` or `scoring-bug-fix`

The `main` branch is still connected to the original repo, not your fork which can come in handy for fetching the latest official changes

### 3. Make changes

You can now make any changes you like and commit your work as you go using git/any tools built into your IDE that integrate with git

Your commits will appear at `https://github/com/<*YOUR USERNAME*>/YAVSRG` and not on the main repo

When you are ready to submit your changes to be added to the main repo, you can submit a PR (step 4)

### 4. Submit a PR

Submit a PR by going to https://github.com/YAVSRG/YAVSRG/compare, choose `base: main` <- `compare: <*YOUR BRANCH*>` and click 'Open Pull Request'

Give your PR a title and description and submit it, it is now basically an open thread on the main repo to add/merge the changes into the current version

Percyqaz will take a look at your changes, suggest any fixes or modifications as needed and then accept or reject the changes