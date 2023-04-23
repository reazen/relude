const fs = require("fs");
const path = require("path");
const pkg = require("../package.json");
const bsConfig = require("../bsconfig.json");

const bsConfigWriter = json => {
  const bsConfigPath = path.join(__dirname, "..", "bsconfig.json");
  fs.writeFileSync(bsConfigPath, JSON.stringify(json, null, 2));
};

const packageWriter = json => {
  const packagePath = path.join(__dirname, "..", "package.json");
  fs.writeFileSync(packagePath, JSON.stringify(json, null, 2));
}

const dependenciesNotNeededForPkg = ["bisect_ppx"];
const hasToBeRemoved = (dep) => !dependenciesNotNeededForPkg.includes(dep)

const filteredDependencies = Object.entries(pkg.dependencies).filter(
  ([dep]) => hasToBeRemoved(dep)
);

const newPkg = {
  ...pkg,
  dependencies: Object.fromEntries(filteredDependencies)
};

packageWriter(newPkg)

const bsDependencies = bsConfig["bs-dependencies"].filter(hasToBeRemoved);

const { ["ppx-flags"]: _, ...rest } = bsConfig;
const newBsConfig = {
  ...rest,
  "bs-dependencies": bsDependencies
};

bsConfigWriter(newBsConfig)
