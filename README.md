# Remote Plugin Loader

Fetches rebar plugins from github on demand. Very experimental. YMMV.

## Demo

    $ cd examples
    $ rebar check-deps install-plugins -v
    $ rebar compile -v      # notice all plugins are now available
    $ rebar clean-plugins clean -v

## Why on earth?

Because *normally* `rebar get-deps compile -v` has to be run first, then the 
plugins become available so that plugins which participate in build-chain 
control operations (such as `(pre|post)process/2`) do not run properly until you
 subsequently run the build a second time, which sucks.

## Running the integration tests

Try this:

    $ rebar get-deps compile -C test.config retest

And stick the `-v` switch on the end if you wish to see what's going on under 
the covers.

## License

BSD-like.
