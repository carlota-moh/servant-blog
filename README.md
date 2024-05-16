# servant-blog

This is a minimal servent written in Servant.

# Database

The database system is composed of two services:

- `postgres`: a PostgreSQL instance hosted in Docker and mapped to port 5435. 
- `liquibase`: service for running migrations

1. Set up the container:

```bash
docker compose up -d
```

**NOTE**: Data will persist when running docker compose down. To empty the DB comment out the `volumes` lines on 
the dockerfile before running `docker compose up`

2. Access it:

```bash
docker exec -it servant-blog-postgres-1 psql -U postgres -d warehouse
```

3. Query the database

a. Open GHCi: `stack ghci`
b. Load module with example queries: 

```bash
ghci> :l src/Query.hs
ghci> runDb insertData
ghci> runDb querydata
```

# Using the project

1. Set up the database:

```bash
docker compose up --build
```

2. Initialize server

```bash
stack build && stack run
```

3. Query the APi

TBD