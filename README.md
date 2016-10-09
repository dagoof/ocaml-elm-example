# Ocaml Elm example

## Setup
Prerequisites to run this project are a working install of `OCaml 4.02.3` through opam, `opam` itself, and a globally installed `npm`, `nodejs`, and `Elm`.

Then run `make setup` from the root directory to install the necessary Elm packages, OCaml packages, and the single-page-app server used to serve the frontend.

## Build

Run `make build` to compile the OCaml backend and build the Elm html output.

## Running

Run `make run` from one shell and `make run-spa` from another to host the backend on port 3000 and frontend on port 8085 respectively.

These addresses are unfortunately necessary for now because the frontend is hardcoded to make requests to the 3000 backend, and the backend is hardcoded to accept CORS requests from the 8185 frontend.

## Tests

Run `make test` to run the backend test suite. There are no Elm tests yet.