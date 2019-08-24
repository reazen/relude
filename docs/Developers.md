# Developer info

## Contributions

We welcome all contributions: bug fixes, features requests (or implementations), documentation, issues, discussions or feedback.

## Development scripts

* Clean: `npm run clean`
* Build: `npm run build`
* Watch: `npm run watch`
* Test: `npm run test`
* Test coverage: `npm run coverage`
* Docs: `npm run docs` (run the docs server)
* Clean & build: `npm run cleanbuild`
* Clean & watch: `npm run cleanwatch`
* Clean & test: `npm run cleantest`
* Clean & coverage: `npm run cleancoverage`
* Release build: `npm run releasebuild`
    * This does not publish the release, it just makes sure various build artifacts (.js, .rei, etc.) are built or re-generated from scratch
    * This should be used before we go to publish a release to make sure the release is clean

## Publishing to npm

We currently do this by hand:

```
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

```
> nix-shell
%nix% > npm install
%nix% > npm run start
```