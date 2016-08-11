Puppet playground
=================

- `make build` creates a Docker container called `plasma/puppet-testing`, which
  has master-less Puppet installed.

- `make run` starts the above container in a root shell and mounts the
  `manifests` directory to '/root/manifests' in the container. To apply a
  manifest, run:

      puppet apply FILENAME --debug

   The container is removedon exit, so there stale state for the user to
   cleanup.

Known Issues
------------

The Docker image begins by running `apt-get update`, which has no effect if its
result is locally cached. If a package to be installed (or their dependencies)
have been updated, then creating the image may fail.  Run `make no-cache` to
resolve this issue.