cd ..
docker build -t interlude-web-server -f Interlude.Web/server/dockerfile .
docker stop server
docker rm server
docker run -d -p 32767:32767 -p 443:443 --restart unless-stopped --name server --mount type=bind,source="$(pwd)"/Interlude.Web/secrets,target=/docker_root/secrets interlude-web-server