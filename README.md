rfp-service
===

### Installation

This service runs on Haskell which requires you to install the Haskell platform as well as Cabal, Haskell's package manager. The easiest way to achieve this is through ghcup.
Check out installation instructions [here](https://github.com/haskell/ghcup#installation)

This project uses Cabal version > 3.0. Once Cabal is installed verify the version with the following command

    cabal --version

If your version is less than 3.0 then you need to update Cabal with the following command

    cabal install cabal-install-3.0.0.0

### Running the API 

To run the api start by building it to get the mms-service executable

    cabal new-build --reorder-goals

Then run it with the following command

    cabal new-exec mms-service --reorder-goals


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
