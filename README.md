# servant-blog

This is a minimal servent written in Servant.

# Database

The database used is a PostgreSQL instance hosted in Docker and mapped to port 5435. To access it:

1. Set up the container:

```bash
docker compose up -d
```

2. Access it:

```bash
docker exec -it servant-blog-postgres-1 psql -U postgres -d warehouse
```

# Using the project

1. Set up the database:

```bash
docker compose up --build -
```

2. Initialize up server

```bash
stack build && stack run
```

3. Query the server

a. Open GHCi: `stack ghci`
b. Load module with example queries: 

```bash
ghci> :l src/Query.hs
ghci> run
```