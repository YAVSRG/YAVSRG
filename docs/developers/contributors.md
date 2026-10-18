# How to contribute to this repository

This guide assumes little pre-existing knowledge other than that you have written some code in a programming language before

If you are experienced with a workflow that works for you instead, feel free to disregard or improvise upon these instructions at your own risk

If these steps don't help or you are stuck, ask in [the Discord](https://yavsrg.net/discord)

### 1. Get the code on your machine
See the [guide to building Interlude as a developer in README.md](https://github.com/YAVSRG/YAVSRG?tab=readme-ov-file#-building-interlude-for-developers-only)

Now you can successfully build and run Interlude with any changes you've made to the codebase!

### 2. Create a fork of YAVSRG on GitHub

You will need to [sign up for a GitHub account](https://github.com/login) if you don't have one

Once you are signed into your GitHub account, [create a fork of YAVSRG](https://github.com/YAVSRG/YAVSRG/fork)

This is where you can commit whatever changes you like to your own copy of the repo

Once the fork is created, connect the repository on your machine to a branch on the fork with these commands:

```bash
# terminal needs to be in the YAVSRG folder
git remote add personal https://github.com/<*YOUR GITHUB USERNAME*>/YAVSRG.git
git checkout -b <*BRANCH NAME*>
git push --set-upstream personal <*BRANCH NAME*>
# now you can add commits and push them as you please
# `git commit` and `git push` commands, etc or your IDE integration will work normally from now on
```

If you are adding a particular feature you could name your branch something descriptive like `help-mode` or `scoring-bug-fix`, otherwise just naming it `develop` is fine  

I **very strongly recommend** that you don't add commits to your `main` branch locally  
This will make your life much easier and less confusing since if you followed the instructions above, since `main` is still connected to the original repo, not your fork  



The `main` on your local repo should only be used for fetching updates from the main repo, then you can merge or rebase these changes into your working branch as needed  

### 3. Make changes

You can now make any changes you like and commit/push your work as you go using git or IDE integrations with git

Any commits you push will appear at `https://github/com/<*YOUR USERNAME*>/YAVSRG` and not on the main repo

When you are ready to submit your changes to be added to the main repo, you can submit a PR (step 4)

### 4. Submit a PR

Submit a PR by going to https://github.com/YAVSRG/YAVSRG/compare, choose `base: main` <- `compare: <*YOUR BRANCH*>` and click 'Open Pull Request'

Give your PR a title and description and submit it, it is now basically an open thread on the main repo to add/merge the changes into the current version

Percyqaz will take a look at your changes, suggest any fixes or modifications as needed and then accept or reject the changes