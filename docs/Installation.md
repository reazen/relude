# Installation

The library is published on [npm](https://www.npmjs.com/package/relude). We try to respect semantic versioning, but while Relude is at version `0.x`, there will likely be breaking changes without corresponding major version bumps.  We will try to document migration paths in release notes when possible.

Note: we currently depend on [mel-bastet](https://github.com/johnhaley81/mel-bastet).


1. Install `mel-bastet` and `relude` via `npm` or `yarn`

```sh
> yarn add mel-bastet relude
```

2. **IMPORTANT**: add `relude` **and** `mel-bastet` to your `bsconfig.json`:

```json
{
  "bs-dependencies": [
    "mel-bastet",
    "relude"
  ]
}
```

Without `mel-bastet` in your `bs-dependencies`, some things may fail to compile - see the [FAQ](FAQ.md) for more information.
