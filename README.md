# Ocaml Elm example

## Overview

This project aims to be an example of a simple, varied, and useful Elm application backed by OCaml and the Opium webserver.

All of the frontend code lives in the `frontend/src` directory, compiles into the `frontend/build` directory. The resulting page is served served by the `single-page-server` binary, which can be installed through `npm` or through `make setup`. 

All of the backend code is placed in the `backend/src` directory and compiles through a fairly standard OCaml Oasis setup. The Makefile, configure, and setup.ml files are all auto-generated and driven by [Oasis](https://ocaml.org/learn/tutorials/setting_up_with_oasis.html). Tests are in the `backend/tests` directory and use the `Alcotest` package to provide output.

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
