# DEPLOYMENT.md (for `rapidspeciation/Shiny_Ikiam_Database_Manager`)

> These steps **build on** the EC2/Docker/Nginx/HTTPS setup from **Shiny Ikiam Wings Gallery**. Follow that repo for instance provisioning, Docker/Nginx install, and HTTPS.
> No new FreeDNS entries needed. This app will be served at **`https://wings.gallery.info.gf/db`** via Nginx path routing.

## 1) Get the code onto the same EC2 instance

```bash
cd ~
git clone https://github.com/rapidspeciation/Shiny_Ikiam_Database_Manager
cd Shiny_Ikiam_Database_Manager
```

## 2) Add the required JSON files (next to `Ikiam_DB_app.R`)

> The app expects `users.json` and `shiny-ikiam-db.json` **in the same directory** as `Ikiam_DB_app.R`. Create the files now (contents added later by you).

```bash
# Inside the repo folder
cd /home/ec2-user/Shiny_Ikiam_Database_Manager

# Create empty files (you will paste real contents later)
touch users.json
touch shiny-ikiam-db.json

# (Optional) quick check
ls -1 Ikiam_DB_app.R users.json shiny-ikiam-db.json
```

* **users.json**: holds the user list + passwords your app reads with `jsonlite::fromJSON("users.json")`.
* **shiny-ikiam-db.json**: Google Service Account credentials used by `googlesheets4::gs4_auth(path = "shiny-ikiam-db.json")`.

> You'll paste your real JSON content into those two files yourself (e.g., with `nano users.json` and `nano shiny-ikiam-db.json`) before running the container.

## 3) Build the Docker image

```bash
cd /home/ec2-user/Shiny_Ikiam_Database_Manager
docker build -t sikwings_db_build .
```

## 4) Run the Database Manager container

Map host **8081 → container 8080**; bind-mount the repo so code + the two JSON files are visible inside the container at `/srv/shiny-server`.

```bash
docker run -d -p 8081:8080 \
  --name sikwings_db_instance \
  --rm \
  -v /home/ec2-user/Shiny_Ikiam_Database_Manager:/srv/shiny-server \
  sikwings_db_build
```

> Because the JSON files live in the repo directory on the host, they're automatically present inside the container (same path your code uses). No extra mounts needed.

## 5) Nginx: route `/db` to this container (reuse the same domain)

Append/update your existing config (the Wings app stays at `/` → `localhost:8080`; the DB app lives at `/db` → `localhost:8081`).

```bash
sudo tee /etc/nginx/conf.d/shiny.conf > /dev/null <<'EOF'
# Nginx reverse proxy for Shiny apps at wings.gallery.info.gf

server {
    listen 80;
    server_name wings.gallery.info.gf;

    # ---- Root app (Wings Gallery) at "/" -> :8080 ----
    location / {
        proxy_pass http://localhost:8080/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_read_timeout 600s;
        proxy_send_timeout 600s;
    }

    # ---- Database Manager at "/db" -> :8081 ----
    # Keep trailing slashes; rewrite strips the prefix so the app sees "/"
    location /db/ {
        rewrite ^/db/(.*)$ /$1 break;
        proxy_pass http://localhost:8081/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_read_timeout 600s;
        proxy_send_timeout 600s;
    }

    # Redirect "/db" -> "/db/"
    location = /db { return 301 /db/; }
}
EOF

sudo nginx -t && sudo systemctl reload nginx
```

## 6) HTTPS

You already enabled HTTPS with Certbot for `wings.gallery.info.gf` in the Wings repo. That single certificate covers **all paths**, including `/db`. No DNS changes required.
If HTTPS isn't set up yet, complete the Certbot step from the Wings repo, then ensure the TLS server block mirrors both locations (`/` and `/db/`).

## 7) Future updates (hot-update flow)

```bash
cd /home/ec2-user/Shiny_Ikiam_Database_Manager

# Stop current container (auto-removed due to --rm)
docker stop sikwings_db_instance

# Pull latest code
git pull

# (If you changed users.json or shiny-ikiam-db.json, edit them now with nano)
# nano users.json
# nano shiny-ikiam-db.json

# Restart with live mount
docker run -d -p 8081:8080 \
  --name sikwings_db_instance \
  --rm \
  -v $(pwd):/srv/shiny-server \
  sikwings_db_build
```

> **Rebuild needed only if the Dockerfile changed** (e.g., added packages):
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