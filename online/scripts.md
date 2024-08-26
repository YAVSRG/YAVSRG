# Set up dev cert for local testing

```
cd secrets
dotnet dev-certs https -ep localhost.pfx -p DEVELOPMENT --trust
```

# Build & Push

For manual use, also see the github action which does the same

Build (from top level repo folder)
```
docker build -t interlude-web-server -f Interlude.Web/server/dockerfile .
```

gh actions uses doctl for this
```
docker login -u <TOKEN> -p <TOKEN> registry.digitalocean.com
```

Push
```
docker tag interlude-web-server registry.digitalocean.com/yavsrg/interlude-web-server:latest
docker push registry.digitalocean.com/yavsrg/interlude-web-server:latest
```

# Droplet setup

Run on Ubuntu 22.10 x64

Got setup info from https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-22-04

### ./setup.sh
```
sudo apt update

sudo apt install apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

sudo apt update
apt-cache policy docker-ce
sudo apt install docker-ce

sudo systemctl status docker
sysctl "vm.overcommit_memory=1"

snap install core; snap refresh core
snap install --classic certbot
certbot certonly --standalone
```

Then login to docker repo
`docker login registry.digitalocean.com/yavsrg/interlude-web-server`
Enter DO API key for both username and password

### ./update.sh
```
docker pull registry.digitalocean.com/yavsrg/interlude-web-server
wget https://raw.githubusercontent.com/YAVSRG/YAVSRG/main/online/docker-compose-production.yml -O ./docker-compose.yml
docker compose up --detach
```

### ./cert.sh
```
mkdir ./secrets
openssl pkcs12 -export -out ./secrets/api.pfx -inkey /etc/letsencrypt/live/api.yavsrg.net/privkey.pem -in /etc/letsencrypt/live/api.yavsrg.net/fullchain.pem
```

### ./logs.sh
```
docker logs server
```