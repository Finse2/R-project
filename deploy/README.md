# Putting MusicBrainz Explorer online

Server: `finse@192.168.1.35` (Linux, on the home network, no GPU needed).
Commands marked **PC** run in PowerShell on the Windows PC; **Server** commands run after `ssh finse@192.168.1.35`.

## 1. Install Docker (Server)

```bash
curl -fsSL https://get.docker.com | sudo sh
sudo usermod -aG docker finse
exit
```

Log in again (`ssh finse@192.168.1.35`) so the group change applies, then check: `docker run --rm hello-world`.

## 2. Make the app folder (Server)

```bash
sudo mkdir -p /opt/musicbrainz-explorer
sudo chown finse: /opt/musicbrainz-explorer
exit
```

## 3. Copy the project (PC)

Close the app on the PC first. This copies about 11.5 GB over the home network:

```powershell
cd "$env:USERPROFILE\Desktop\R project"
scp -r app.R .dockerignore deploy data output music-recommender finse@192.168.1.35:/opt/musicbrainz-explorer/
```

Then make the files readable by the container (Server):

```bash
chmod -R a+rX /opt/musicbrainz-explorer
```

## 4. Start it and test on the home network (Server)

```bash
cd /opt/musicbrainz-explorer
docker compose -f deploy/docker-compose.yml up -d --build
docker compose -f deploy/docker-compose.yml exec ollama ollama pull qwen3:4b
```

The first build takes 5-15 minutes. Then open `http://192.168.1.35:3838` on the PC.

## 5. Put it on your domain (choose A or B)

### A. Cloudflare Tunnel (recommended: no router changes)

Needs the domain's DNS to be on Cloudflare (free plan).

1. Cloudflare dashboard -> Zero Trust -> Networks -> Tunnels -> **Create a tunnel** -> Cloudflared -> name it `musicbrainz`.
2. Copy the token (the long text after `--token` in the install command). Do not install anything from that page.
3. **Public hostname**: subdomain `music`, your domain, service type `HTTP`, URL `app:3838`. Save.
4. Server:

```bash
cd /opt/musicbrainz-explorer
echo "TUNNEL_TOKEN=paste-the-token-here" > deploy/.env
chmod 600 deploy/.env
docker compose -f deploy/docker-compose.yml --profile tunnel up -d
```

### B. Router port forwarding

1. In the router, forward TCP ports **80** and **443** to `192.168.1.35`.
2. At your DNS provider, add an **A record** `music` -> your home's public IP (shown at https://ifconfig.me).
3. Server:

```bash
cd /opt/musicbrainz-explorer
echo "DOMAIN=music.yourdomain.com" > deploy/.env
docker compose -f deploy/docker-compose.yml --profile caddy up -d
```

## 6. Check

Open `https://music.yourdomain.com`, then try a chart page and a recommendation search.

## Day-to-day

| Task | Command (Server, in /opt/musicbrainz-explorer) |
|---|---|
| See logs | `docker compose -f deploy/docker-compose.yml logs -f app` |
| After changing app.R (copy it again with scp first) | `docker compose -f deploy/docker-compose.yml up -d --build app` |
| Stop everything | `docker compose -f deploy/docker-compose.yml --profile tunnel --profile caddy down` |

Everything restarts by itself after a server reboot.
