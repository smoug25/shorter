# Shorter
Shorter is backend server for link shorting wreaten in haskell

## Build and configuration

For configuration you have to add app.conf file in directory with app.
File must consist next fields. You can see defalt config below.

```conf
server {
  domen = "http://0.0.0.0:8080"
  port = 8080
}
pg {
  dsn = "postgresql://postgres:example@localhost:25432/shorter"
}
redis {
  host = "0.0.0.0"
  port = "16379"
  password = "example"
  db = 0
  maxConn = 64
}
smtp {
  host = "localhost"
  login = ""
  password = ""
  from = "auth@shorter.ru"
}
```
For building you have several options

* Build with cabal command

```shell
cabal build
```

* Build with make commands

```shell
make build
```

## Run application

App can be running in severalmods with next mods:

* upDb   - run db migrations
* cli    - run app like cli app
* server - run app like web server

Local run app with make command:

* First of all you have to start local environment in docker

```shell
make up
```
* Command for run app in diferent mods

```shell
make upDb_local_run
make cli_local_run
make server_local_run
```

* For shot down local environment

```shell
make down
```

## API description

### Authorization

#### POST /auth

Request:

```json
{
    "email": "user@example.com"
}
```

In response you will get cookie with auth token and confirmation code on sended emain, after response you have 5 minuts for confirm your email.

#### POST /confirm

Request:

```json
{
    "code": "5658"
}
```

In succsessful response you will get cookie with session token. Session ttl is 1 month.

### Url shorting

#### POST /shorten

Request:

```json
{
    "url" : "http://google.com",
    "allowEmails": [{ "email":"some@email.com"}],
    "private" : true
}
```
You can create pablic or private urls. For private urls you have to be authorized.

Response:

```json
{
    "shortUrl": "<server.domen>/262d9ae846"
}
```
There <server.domen> is value from config.

### GET /<url_short_hash>

There <url_short_hash> is hash like "262d9ae846". If url is private and you are not authorized you get 401 http status another way you will be redirected to original url.


