# Publishing Relude to Opam

This document describes how Relude is published to the [opam repository](https://opam.ocaml.org/).

## Automated Publishing

Relude uses GitHub Actions to automatically publish to opam when a new release is created.

### Prerequisites

The repository must have an `OPAM_PUBLISH_TOKEN` secret configured with:
- A GitHub Personal Access Token with `public_repo` scope
- Permission to create pull requests in external repositories (opam-repository)

### Automated Workflow

1. **Create a GitHub Release**: When a new release is published on GitHub, the CI workflow automatically:
   - Validates the `relude.opam` file using `opam lint`
   - Installs the `opam-publish` plugin
   - Configures git credentials for GitHub operations
   - Publishes to opam using `opam publish --no-confirmation --tag=<version> .`

2. **Validation & Dry Run**: On every CI run (including PRs), the workflow:
   - Validates the opam file using `opam lint`
   - Runs a dry run of the publishing process to ensure it would work
   - Creates a temporary tag and tests `opam publish --dry-run`

## Manual Publishing

If automated publishing fails or is not configured, you can publish manually:

```bash
# Clone the repository and checkout the release tag
git clone https://github.com/reazen/relude.git
cd relude
git checkout v<version>

# Publish to opam
opam publish --tag=v<version> .
```

## Setting up OPAM_PUBLISH_TOKEN

Repository maintainers need to:

1. Create a GitHub Personal Access Token with `public_repo` scope
2. Add it as a repository secret named `OPAM_PUBLISH_TOKEN`
3. Ensure the token has permission to create pull requests

## Opam File Validation & Dry Run Testing

The `relude.opam` file and publishing process are automatically tested on every CI run:

### Validation
```bash
opam lint relude.opam
```

### Dry Run Testing
On every non-release CI run, the workflow performs a comprehensive validation of the publishing process:
- Installs the `opam-publish` plugin
- Verifies package metadata can be read with `opam show --raw .`
- Validates required opam file fields (homepage, bug-reports, synopsis)
- Checks git repository state
- Confirms publishing prerequisites without actually submitting

To test locally:
```bash
# Install opam-publish plugin
opam install opam-publish

# Test package metadata
opam show --raw .

# Validate required fields
grep -q "^homepage:" relude.opam && 
grep -q "^bug-reports:" relude.opam && 
grep -q "^synopsis:" relude.opam && 
echo "✅ Required fields present"

# Check git state
git rev-parse --verify HEAD
```

## Release Process

1. Ensure all changes are merged and CI is passing
2. Update version in `dune-project` and `relude.opam` if needed
3. Create and publish a new GitHub release with proper version tag (e.g., `v2.1.0`)
4. The CI workflow will automatically publish to opam
5. Monitor the workflow and check for any publishing errors

## Troubleshooting

- **Permission errors**: Ensure `OPAM_PUBLISH_TOKEN` has the correct scopes
- **Validation errors**: Run `opam lint relude.opam` locally to identify issues
- **Publishing failures**: Check the GitHub Actions logs for detailed error messages

For more information about opam packaging, see the [official documentation](https://opam.ocaml.org/doc/Packaging.html).