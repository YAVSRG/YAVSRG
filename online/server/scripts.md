# Build & Push

### Build (from top level repo folder)
```
docker build -t interlude-web-server -f Interlude.Web/server/dockerfile .
```

### gh actions uses doctl for this
```
docker login -u <TOKEN> -p <TOKEN> registry.digitalocean.com
```

### Push
```
docker tag interlude-web-server registry.digitalocean.com/yavsrg/interlude-web-server:latest
docker push registry.digitalocean.com/yavsrg/interlude-web-server:latest
```

# Droplet setup

### Docker setup
```
sudo apt update

sudo apt install apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

sudo apt update
apt-cache policy docker-ce
sudo apt install docker-ce

sudo systemctl status docker
```

### Run container
```
docker login -u <TOKEN> -p <TOKEN> registry.digitalocean.com
docker pull registry.digitalocean.com/yavsrg/interlude-web-server
docker rm server
docker run -d -p 32767:32767 --restart unless-stopped --name server registry.digitalocean.com/yavsrg/interlude-web-server
```
