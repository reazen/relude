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
module IO = {
  /* Read a file with no accomodation for errors */
  let readFileSync: string => Relude_IO.t(string, Js.Exn.t) =
    path => Relude_IO.triesJS(() => Native.readFileSync(path, `utf8));

  let writeFileSync: (string, string) => Relude_IO.t(unit, Js.Exn.t) =
    (path, content) =>
      Relude_IO.triesJS(() => Native.writeFileSync(path, content, `utf8));

  let readFile: string => Relude_IO.t(string, Js.Exn.t) =
    path =>
      Relude_IO.async(onDone =>
        Native.readFile(path, `utf8, (err, content) =>
          switch (Js.Null.toOption(err), content) {
          | (Some(err'), _) =>
            Js.Console.error(
              "Read failed: "
              ++ (
                Js.Exn.message(err')
                |> Relude_Option.getOrElseLazy(_ => "No error")
              ),
            );
            onDone(Error(err'));
          | (_, content) => onDone(Ok(content))
          }
        )
      );

  let writeFile: (string, string) => Relude_IO.t(unit, Js.Exn.t) =
    (path, content) =>
      Relude_IO.async(onDone =>
        Native.writeFile(path, content, `utf8, err =>
          switch (Js.Null.toOption(err)) {
          | Some(err') =>
            Js.Console.error(
              "Write failed: "
              ++ (
                Js.Exn.message(err')
                |> Relude_Option.getOrElseLazy(_ => "No error")
              ),
            );
            onDone(Error(err'));
          | None => onDone(Ok())
          }
        )
      );
};

let testFilePath: string => string =
  fileName => Native.dirnameOrDot ++ "/" ++ fileName;
