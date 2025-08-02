# Developer info

## Contributions

We welcome contributions, bug fixes, features requests (or implementations), documentation, issues, discussions or feedback. If you're not sure what exactly you want, or how to implement it, feel free to [create an issue](https://github.com/reazen/relude/issues).

### Documentation

One way you can help is by improving the quality of documentation. We aim for consistency within our docs. For the best output, doc comments in code should look like:

```reason
/**
[Module.functionName] does something. This applies the provided function to the
value of type [t] and returns [something] if...

Note: there are certain special cases to be aware of. See {!val:otherFn} for an
alternative way to approach this problem.

Running time: O(n{^2})

{[
  Module.functionName(x => x, Module.someValue) == 4;
]}
*/
```

Specifically:

- Comments should use [odoc syntax](http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html)
- The first sentence should state the name of the function and succinctly state what it does
- Use complete sentences
- Add line breaks at column 80, to match `refmt`'s width used for code
- Any gotchas should be listed in a separate paragraph
- Use odoc cross-references to link to other functions and modules
- If time complexity is of interest, list that last before the examples
- Try to include multiple examples
  - Write examples as if `open Relude.Globals` is present
  - Use the same formatting `refmt` would produce
  - Show returned values with `== value`
  - Try to fit each example onto a single line, when possible

## Development scripts

In order to build `Relude`, after cloning, run:

```sh
# Create opam switch and install dependencies
> make init

# Install development tools
> make dev-tools
```

From there, use the following scripts to build/clean/test the project:

```sh
# Clean

> make clean

# Build

> make build

# Watch

> make watch

# Test

> make test

# Test with watch mode

> make test-watch

# Coverage
# The coverage files are .gitignore'd, so you must run this
# command locally to view the detailed coverage reports.

> make test-coverage

# Docs (build documentation):

> make docs

# Docs (serve documentation locally):

> make docs-serve

```

## Publishing to opam

We publish to the opam repository by hand:

```sh
# Run tests and build to ensure everything works

> make test
> make build

# Create a release tag

> git tag v2.x.x
> git push upstream --follow-tags
> git push origin --follow-tags

# Submit to opam-repository (follow opam packaging guidelines)

# Go to GitHub and create a new release
```

## NixOS

If you are using NixOS, there is an extremely basic `default.nix` file that you can
use to build the project in a `nix-shell`, to avoid the `bs-platform` binary issues
in `node_modules`.

e.g.

```sh
# Launch nix shell with default.nix

> nix-shell

# (Re-)install dependencies

nix> rm -rf node_modules
nix> npm install
nix> exit

# Now you can run commands normally

> npm run watch
```
