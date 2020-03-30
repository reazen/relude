# Installation

The library is published on [npm](https://www.npmjs.com/package/relude). We try to respect semantic versioning, but while Relude is at version `0.x`, there will likely be breaking changes without corresponding major version bumps.  We will try to document migration paths in release notes when possible.

Note: we currently depend on [bs-bastet](https://github.com/Risto-Stevcev/bs-bastet) as a `peerDependency` (and a `devDependency`).  We use peer dependencies rather than hard dependencies to avoid
the duplicate package issues that surface when using a statically typed language in the npm duplicated package installation methodology.


1. Install `bs-bastet` and `relude` via `npm` or `yarn`

```sh
> npm install --save bs-bastet relude
```

2. **IMPORTANT**: add `relude` **and** `bs-bastet` to your `bsconfig.json`:

```json
{
  "bs-dependencies": [
    "bs-bastet",
    "relude"
  ]
}
```

Without `bs-bastet` in your `bs-dependencies`, some things may fail to compile - see the [FAQ](FAQ.md) for more information.
