0.7.27.4
====

Yet another hotfix update

# Bug fixes
- Hotfix for a change to the osu!stable client's database format today, score importing should work again after this fix
- Reverted the anti-aliasing changes to framebuffers, it caused hard-to-investigate issues (in addition to what was fixed in 0.7.27.3) on other people's hardware

The reverted change for framebuffers only affects the score screen graph and gameplay previews, the MSAA setting and its performance gains remain the same

# Improvements
There is now only a "Log in with Discord" button instead of another one to register  
If you don't have an account it will prompt you to register automatically, otherwise it will log you in  
Multiple people (not naming names) were clicking "Log in" without an account and reporting it as a bug

