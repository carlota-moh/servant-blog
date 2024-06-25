# servant-blog

This is a minimal servent written in Servant.

# Database

The database system is composed of two services:

- `postgres`: a PostgreSQL instance hosted in Docker and mapped to port 5435. 
- `liquibase`: service for running migrations

## 1. Set up the container:

```bash
docker compose up -d
```

**NOTE**: Data will persist when running docker compose down. To empty the DB comment out the `volumes` lines on 
the dockerfile before running `docker compose up`

## 2. Access it:

```bash
docker exec -it servant-blog-postgres-1 psql -U postgres -d warehouse
```

## 3. Query the database

### 3.a. Open GHCi: `stack ghci`
### 3.b. Load module with example queries: 

```bash
ghci> :l src/Db.hs
ghci> runDb insertData
ghci> runDb queryData
```

# Using the project

## 1. Set up the database:

```bash
docker compose up --build -d
```

**NOTE**: Data will persist when running docker compose down. To empty the DB comment out the `volumes` lines on 
the dockerfile before running `docker compose up`

## 2. Initialize server

```bash
stack build && stack run
```

## 3. Query the API

You can either query the database directly using `curl` or by using some of the example queries provided in `src/Query.hs`

### Using curl

```bash
curl --request GET http://localhost:8080/users/list-all 

curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"userId":1,"userName":"Charly"}' \
  http://localhost:8080/createUser
```

### Using query functions

1. Open GHCi: `stack ghci`
2. Load module with example queries: 

```bash
ghci> :l src/Query.hs
ghci> runQuery queryAllUsers
ghci> runQuery queryOneUser 1
ghci> runQuery queryUpsertUser (User 5 "TMA1" $ Just 9999)
ghci> runQuery queryDeleteUser 1
ghci> runQuery queryUpsertUser (User 5 "TMA2" $ Just 9998)
```