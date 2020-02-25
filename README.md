rfp-service
===

### Installation

This service runs on Haskell which requires you to install the Haskell platform as well as Cabal, Haskell's package manager. The easiest way to achieve this is through ghcup.
Check out installation instructions [here](https://github.com/haskell/ghcup#installation)

Once cabal is installed run

    cabal configure && cabal build

### Commands

**run**

Run API server. Optional port number
    
    run [-p|--port] 

Example

    -- Run server on port 8080
    cabal exec rfp-service run -- -p 8080
    
**createuser**

Create a user. If the -a --admin flag is present
a API key is created and can be used for API
POST / DELETE methods.

    createuser (-u|--username ARG) (-p|--password ARG)
               [-e|--email ARG] [-a|--admin]

Example

    -- Create admin user with username=server password=servant
    cabal exec rfp-service createuser -- -u server -p servant -a

### RFP Worker 

A worker runs separately to handle email jobs etc. 
Make sure you have a redis is running on port 6379 
and then start the worker with

    cabal exec rfp-worker

### Building with Nix

    nix-build release.nix 

Once nix-build is finished you can run the API
with the binary located in ./result

    ./result/bin/rfp-service

### THINGS TO NOTE

Two folders are excluded from git since we don't want to share their content. After cloning this repo run the following command at the root of the project. 

    mkdir -p static/uploads logs

### Env variables

**ENV**

Current environment. The options are

- Development
- Production

Switching these environments mostly changes how logging is managed

**PORTNR**

The port number for the service to run on. 

**PGHOST**

Postgres host (e.g. localhost)

**PGPORT**

Postgres port

**PGUSER**

Postgres username

**PGPASS**

Postgres password

**PGDATABASE**

Postgres database

**SENDGRID_API_KEY**

Sendgrid API Key. This key needs to be present when running the RFPWorker to send out emails. 
