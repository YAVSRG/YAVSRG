The web infrastructure for [[Interlude]]

todo: [[Backbeat]] should become part of the main server
as should the [[Noteskins]] repo

Can use B2 to store objects instead of having them in GitHub
#### todo: Find a way to securely take database backups
- ChatGPT kindly told me about `scp` which sounds like the right plan as I already just have an SSH port open
- Scheduled GitHub action to fetch a copy of database ** does copying it while it's being read cause any issues?
- Where do I upload it such that only I can see it <- Set up a private repo with just this script, take build artifacts from there

#### todo: New deployment pipeline for server
- Run unit tests
- Build docker image
- Run docker image ** todo: Need some default secrets set up for this OR do some command line argument magic
- Run integration tests against image
- Deploy image to prod server

#### todo: Migrate to SQLite for persistence on server backend
- DigitalOcean droplet has 25gb of disk space
- SQLite files can scale to the 20gb mark easily
- I cannot forsee my databases needing more than a couple GB ever unless this project blows up
- Its all on one machine so that's a whole layer of fallibility gone

I reckon these 3 things could be written up as GitHub issues