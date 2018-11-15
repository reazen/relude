open Belt.Result;

/**
Fs.FFI wraps a few functions from the node.js fs module with little to no modification to the fs API.
*/
module Native = {
  let dirname: option(string) = [%bs.node __dirname];
  let dirnameOrDot = Js.Option.getWithDefault(".", dirname);

  [@bs.val] [@bs.module "fs"]
  external readFileSync:
    (string, [@bs.string] [ | `hex | `utf8 | `ascii]) => string =
    "readFileSync";

  [@bs.val] [@bs.module "fs"]
  external writeFileSync:
    (string, string, [@bs.string] [ | `hex | `utf8 | `ascii]) => unit =
    "writeFileSync";

  [@bs.val] [@bs.module "fs"]
  external readFile:
    (
      string,
      [@bs.string] [ | `hex | `utf8 | `ascii],
      (Js.null(Js.Exn.t), string) => unit
    ) =>
    unit =
    "readFile";

  [@bs.val] [@bs.module "fs"]
  external writeFile:
    (
      string,
      string,
      [@bs.string] [ | `hex | `utf8 | `ascii],
      Js.null(Js.Exn.t) => unit
    ) =>
    unit =
    "writeFile";
};

/**
Fs.Eff wraps the synchronous Native functions in the Eff monad.
Note: these fs functions can actually fail with exceptions with are not handled by Eff.
*/
module Eff = {
  /* Read a file with no accomodation for errors */
  let readFileSync: string => Relude_Eff.t(string) =
    (path, ()) => {
      /*
      Js.Console.log("Fs.Eff.readFileSync: reading file: " ++ path ++ "...");
      */
      Native.readFileSync(path, `utf8);
    }

  /* Write a file with no accomodation for errors */
  let writeFileSync: (string, string) => Relude_Eff.t(unit) =
    (path, content, ()) => {
      /*
      Js.Console.log("Fs.Eff.writeFileSync: writing file: " ++ path ++ "...");
      */
      Native.writeFileSync(path, content, `utf8);
    }
};

/**
Fs.Aff wraps the asynchronous Native functions in the Aff monad.
These functions can fail with async callback errors, which are captured by the Aff error type (the raw Js.Exn.t in this case).
*/
module Aff = {
  let readFile: string => Relude_Aff.t(string, Js.Exn.t) =
    (path, onDone, ()) => {
      /*
      Js.Console.log("Fs.Aff.readFile: reading " ++ path ++ "...");
      */
      Native.readFile(path, `utf8, (err, content) =>
        switch (Js.Null.toOption(err), content) {
        | (Some(err'), _) =>
          Js.Console.error(
            "Read failed: "
            ++ (Js.Exn.message(err') |> Relude_Option.getOrElse(_ => "No error")),
          );
          onDone(Error(err'), ());
        | (_, content) =>
          /*
          Js.Console.log("Read success!");
          */
          onDone(Ok(content), ());
        }
      );
    };

  let writeFile: (string, string) => Relude_Aff.t(unit, Js.Exn.t) =
    (path, content, onDone, ()) => {
      /*
      Js.Console.log("Fs.Aff.writeFile: writing file: " ++ path ++ "...");
      */
      Native.writeFile(path, content, `utf8, err =>
        switch (Js.Null.toOption(err)) {
        | Some(err') =>
          Js.Console.error(
            "Write failed: "
            ++ (Js.Exn.message(err') |> Relude_Option.getOrElse(_ => "No error")),
          );
          onDone(Error(err'), ());
        | None =>
          /*
          Js.Console.log("Write success!");
          */
          onDone(Ok(), ());
        }
      );
    };
};

let testFilePath: string => string = (fileName) => Native.dirnameOrDot ++ "/" ++ fileName;
