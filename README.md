# Cloud Haskell Implementation

## Proposal 1

Machines do not have a shared database for packages. When a project is asked to be built, that machine becomes the _master_ for that build and handles all communication between various other worker nodes.


# Proposal 2

Machines have a shared database for packages. This database is managed by a dedicated _master_ who handles all builds. When a project is asked to be built, the machine that asks sends a messages to the _master_ asking for the project to be built.


## How to Achieve this

### Replace all concurrency code with message passing
  `singleBuild` makes use of STM and MVar's.

  _Functions called by singleBuild that are affected_

    * `getConfigCache`
    * `ensureConfig`
    * `withSingleContext` (logs)
