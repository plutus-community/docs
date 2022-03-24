date:2022-03-10
change-date: 2022-03-21
author:   Dieter Buricke <ghburdie@web.de>

# MkDocs Docker image and Docker container

If you like to run mkdocs server in Docker container, then this description can be usefull for you.

After starting Docker container, mkdocs server is reached at http://localhost:8083 from host system (compare configuration section "ports:" in docker-compose-dev.yml).

## Building image and starting container

Building Docker image and starting container with docker-compose needs only to be done once.
Then you can reuse that container several times (but you can rebuild it whenever you want).

Run following command (as user root!? - depending on your Docker installation) in directory, where files Dockerfile-dev and docker-compose-dev.yml are located:

```
docker-compose -p plutus-community-docs -f docker-compose-dev.yml up
```
Above command builds Docker image "plutus-community-docs_docs" (notice _underscore before docs!) and starts Docker container "plutus-community-docs".

## Docker container needs mkdocs serving at 0.0.0.0!

To be able to connect from host system to mkdocs server running inside Docker container, it needs to bind to all interfaces within container (not only the default: localhost)!

### In mkdocs.yml:
This is enabled through following line in mkdocs.yml:
```
dev_addr: '0.0.0.0:8000'
```
### Or when starting mkdocs:

If you want to leave mkdocs.yml untouched, mkdocs serve has to be started with option --dev-addr (the option has to be placed after mkdocs serve and must not put before serve!) in Dockerfile Dockerfile-dev:
```
CMD ["mkdocs", "serve", "--dev-addr=0.0.0.0:8000"]
```

## List availabe containers

List of available (--all: not only running) containers and filtering output with grep:
```
docker container ls --all | grep plutus-community-docs
```
You get a list with following columns:
CONTAINER ID | IMAGE | COMMAND | CREATED | STATUS | PORTS | NAMES

## Stopping container

Stopping container:
```
docker stop <CONTAINER ID>
```

## Starting container

### Starting existing container (in detached mode):
```
docker start <CONTAINER ID>
```
### Starting in attached mode
For watching log messages of mkdocs server it can be helpfull to start in attached mode:
```
docker start -a <CONTAINER ID>
```
Container can be stopped with keys: STRG+c

## Starting interactive shell to container
For administrative work you can start a shell to container:
```
docker exec -it <CONTAINER ID> /bin/sh
```
