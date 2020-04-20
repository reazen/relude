const bisectRuntime = require("bisect_ppx/lib/js/src/runtime/bucklescript/runtime.js");

afterAll(() => bisectRuntime.write_coverage_data());
