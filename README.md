rfp-service
===

### Installation

This service runs on Haskell which requires you to install the Haskell platform as well as Cabal, Haskell's package manager. The easiest way to achieve this is through ghcup.
Check out installation instructions [here](https://github.com/haskell/ghcup#installation)

Once cabal is installed run

    cabal configure && cabal build

### Running the API 

    cabal exec rfp-service

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
