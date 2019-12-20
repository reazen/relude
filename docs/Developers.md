# Developer info

## Contributions

We welcome contributions, bug fixes, features requests (or implementations), documentation, issues, discussions or feedback.  If you're not sure what exactly you want, or how to implement it, feel free to [create an issue](https://github.com/reazen/relude/issues).

## Development scripts

```sh
# Clean

> npm run clean

# Build

> npm run build

# Watch

> npm run watch

# Test

> npm run test

# Coverage
# The coverage files are .gitignore'd, so you must run this
# command locally to view the detailed coverage reports.

> npm run coverage && open docs/coverage/lcov-report/index.html

# Docs (run local server):

> npm run docs

# Clean & build
> npm run cleanbuild

# Clean & watch
> npm run cleanwatch

# Clean & test
> npm run cleantest

# Clean & coverage
> npm run cleancoverage && open docs/coverage/lcov-report/index.html

# Release build
# This does not publish the release, it just makes sure various build artifacts (.js, .rei, etc.) are built or re-generated from scratch
# This should be used before we go to publish a release to make sure the release is clean

> npm run releasebuild
```

## Publishing to npm

We currently do this by hand:

```sh
# Run the release build to generate all release artifacts

> npm run releasebuild

# Bump the version, update package.json, and create the git tag

> npm version major|minor|patch

# Push git commit and tags to upstream/origin

> git push upstream --follow-tags
> git push origin --follow-tags

# Publish the new version to npm

> npm publish

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