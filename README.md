# Remote Plugin Loader

Fetches rebar plugins from github on demand. Very experimental. YMMV.

## Demo

    $ cd examples
    $ rebar check-deps plugins:install -v
    $ rebar compile -v      # notice all plugins are now available
    $ rebar plugins:clean clean -v

## License

BSD-like.
