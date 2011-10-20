# Remote Plugin Loader

Fetches rebar plugins from github on demand. Very experimental. YMMV.

## Demo

    $ cd examples
    $ rebar check-deps install-plugins -v
    $ rebar compile -v      # notice all plugins are now available
    $ rebar clean-plugins clean -v

## Why on earth?

Because `rebar get-deps compile -v` has to be run first-off, and plugins that
participate in build-chain control operations such as `(pre|post)process/2` do
not run until you subsequently run the build a second time, which sucks.

## License

BSD-like.
