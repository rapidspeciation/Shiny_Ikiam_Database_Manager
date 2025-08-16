# DEPLOYMENT.md (for `rapidspeciation/Shiny_Ikiam_Database_Manager`)

> These steps **build on** the EC2/Docker/Nginx/HTTPS setup from **Shiny Ikiam Wings Gallery**.
> No new FreeDNS entries are needed. This app will be served at **`https://wings.gallery.info.gf/db/`** via Nginx path routing on the **same domain and certificate**.

## 1) Get the code onto the same EC2 instance

```bash
cd ~
git clone https://github.com/rapidspeciation/Shiny_Ikiam_Database_Manager
cd Shiny_Ikiam_Database_Manager
```

## 2) Add the required JSON files (next to `Ikiam_DB_app.R`)

> The app reads `users.json` and `shiny-ikiam-db.json` **from the same directory** as `Ikiam_DB_app.R`.

```bash
# Inside the repo
cd /home/ec2-user/Shiny_Ikiam_Database_Manager

# Create the files (paste real contents later with nano)
touch users.json
touch shiny-ikiam-db.json

# Quick check
ls -1 Ikiam_DB_app.R users.json shiny-ikiam-db.json
```

* **users.json** → consumed by `jsonlite::fromJSON("users.json")`
* **shiny-ikiam-db.json** → used by `googlesheets4::gs4_auth(path = "shiny-ikiam-db.json")`

## 3) Build the Docker image

```bash
cd /home/ec2-user/Shiny_Ikiam_Database_Manager
docker build -t sikwings_db_build .
```

## 4) Run the Database Manager container

Map **host 8081 → container 8080** and live-mount the repo so app code + JSON files are visible inside the container:

```bash
docker run -d -p 8081:8080 \
  --name sikwings_db_instance \
  --rm \
  -v /home/ec2-user/Shiny_Ikiam_Database_Manager:/srv/shiny-server \
  sikwings_db_build
```

> Do **not** open port 8081 in the security group; Nginx will proxy internally from 443→8081.

## 5) Nginx: add path routing **without overwriting Certbot's config**

> You **must not** replace `/etc/nginx/conf.d/shiny.conf` (Certbot manages it).
> Instead, create a **snippet** with your proxy locations and **include** it only in the TLS (443) server.

### 5.1 Create the shared locations snippet

```bash
sudo mkdir -p /etc/nginx/snippets
sudo tee /etc/nginx/snippets/ikiam-locations.conf > /dev/null <<'EOF'
# Wings app at "/"
location / {
    proxy_pass http://localhost:8080/;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_set_header Host $host;
    proxy_read_timeout 600s;
    proxy_send_timeout 600s;
}

# Database Manager app at "/db"
location /db/ {
    # Strip the "/db/" prefix so the Shiny app thinks it's at "/"
    rewrite ^/db/(.*)$ /$1 break;
    proxy_pass http://localhost:8081/;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_set_header Host $host;
    proxy_read_timeout 600s;
    proxy_send_timeout 600s;
}

# Normalize "/db" -> "/db/"
location = /db { return 301 /db/; }
EOF
```

### 5.2 Include the snippet in the **443 ssl** server

Find the TLS vhost file (created by Certbot) and edit it:

```bash
# Locate the 443 block for wings.gallery.info.gf
sudo grep -R "listen 443" -n /etc/nginx/conf.d/ /etc/nginx/nginx.conf
sudo grep -R "server_name wings.gallery.info.gf" -n /etc/nginx/conf.d/ /etc/nginx/nginx.conf

# Edit the file that has:  listen 443 ssl;  server_name wings.gallery.info.gf;
sudo nano /etc/nginx/conf.d/shiny.conf
```

Inside the **server { … }** block that has `listen 443 ssl;` and `server_name wings.gallery.info.gf;`, add this line **once** (not inside another `location`):

```
include /etc/nginx/snippets/ikiam-locations.conf;
```

> Do **not** duplicate the same `location` blocks inline and in the snippet. Keep them **only** in the snippet.

Reload:

```bash
sudo nginx -t && sudo systemctl reload nginx
```

## 6) HTTPS

You already enabled HTTPS via Certbot for `wings.gallery.info.gf`. That single certificate secures **all paths**, including `/db/`. No DNS changes needed.

## 7) Verify

```bash
# Containers
docker ps --format "table {{.Names}}\t{{.Ports}}\t{{.Status}}"

# Local HTTP proxy (optional)
curl -I http://127.0.0.1/
curl -I http://127.0.0.1/db/

# Public HTTPS
curl -I https://wings.gallery.info.gf/
curl -I https://wings.gallery.info.gf/db/
```

## 8) Future updates (hot-update flow)

```bash
cd /home/ec2-user/Shiny_Ikiam_Database_Manager

# Stop the running container (auto-removed due to --rm)
docker stop sikwings_db_instance

# Pull latest app code
git pull

# (If you changed users.json or shiny-ikiam-db.json, edit them now)
# nano users.json
# nano shiny-ikiam-db.json

# Restart with live mount
docker run -d -p 8081:8080 \
  --name sikwings_db_instance \
  --rm \
  -v $(pwd):/srv/shiny-server \
  sikwings_db_build
```

> Rebuild only if the **Dockerfile** changed (e.g., new packages):
>
> ```bash
> docker stop sikwings_db_instance
> docker build -t sikwings_db_build .
> docker run -d -p 8081:8080 \
>   --name sikwings_db_instance \
>   --rm \
>   -v $(pwd):/srv/shiny-server \
>   sikwings_db_build
> ```

## 9) Troubleshooting quick hits

* **Don't overwrite Certbot's file** with `tee > /etc/nginx/conf.d/shiny.conf`. Always edit and **include the snippet**.
* `https://…` fails while `http://127.0.0.1/db/` works → the 443 server likely **doesn't include** the snippet.
* `nginx -T | sed -n '/server_name wings\.gallery\.info\.gf/,/}/p'` → confirm the include line is inside the **443** server.
* Keep EC2 security group open for **80/tcp** and **443/tcp** only; never expose **8080/8081** publicly.