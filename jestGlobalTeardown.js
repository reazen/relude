const bisectRuntime = require("bisect_ppx/lib/js/src/runtime/bucklescript/runtime.js");

module.exports = function globalTeardown(_globalConfig) {
  bisectRuntime.write_coverage_data();
};
