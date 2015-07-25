# jalapeno

An excapade into the realm of FRP - trying to design my own FRP network designed
around video game development. It is the spiritual successor to my library
[Spice](http://github.com/crockeo/spice).

### Installation

Until it's in any sort of place to be uploaded to Hackage, right now you'll have
to install it directly:

```bash
$ git clone http://github.com/crockeo/jalapeno.git
$ cd jalapeno
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal sdist
```

And then use `cabal` to install directly from the source distribution.

### License

Refer to the `LICENSE` file for licensing information.
