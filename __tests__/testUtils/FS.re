/**
Fs.Native wraps a few functions from the node.js fs module with little to no
modification to the fs API.
*/
module Native = {
  let dirname: option(string) = [%mel.node __dirname];
  let dirnameOrDot = Belt.Option.getWithDefault(dirname, ".");

  [@mel.module "fs"] [@warning "-103"]
  external readFileSync: (string, [ | `hex | `utf8 | `ascii]) => string =
    "readFileSync";

  [@mel.module "fs"]
  external writeFileSync: (string, string, [ | `hex | `utf8 | `ascii]) => unit =
    "writeFileSync";

  [@mel.module "fs"]
  external readFile:
    (
      string,
      [ | `hex | `utf8 | `ascii],
      (Js.null(Js.Exn.t), string) => unit
    ) =>
    unit =
    "readFile";

  [@mel.module "fs"]
  external writeFile:
    (string, string, [ | `hex | `utf8 | `ascii], Js.null(Js.Exn.t) => unit) =>
    unit =
    "writeFile";
};

/**
Fs.IO wraps the synchronous Native functions in the IO monad.

Note: these fs functions can actually fail with exceptions with are not handled
by IO.
*/
module IO = {
  // Read a file with no accomodation for errors
  let readFileSync: string => Relude.IO.t(string, Js.Exn.t) =
    path => Relude.IO.triesJS(() => Native.readFileSync(path, `utf8));

  let writeFileSync: (string, string) => Relude.IO.t(unit, Js.Exn.t) =
    (path, content) =>
      Relude.IO.triesJS(() => Native.writeFileSync(path, content, `utf8));

  let readFile: string => Relude.IO.t(string, Js.Exn.t) =
    path =>
      Relude.IO.async(onDone =>
        Native.readFile(path, `utf8, (err, content) =>
          switch (Js.Null.toOption(err), content) {
          | (Some(err'), _) =>
            Js.Console.error(
              "Read failed: "
              ++ (
                Js.Exn.message(err')
                |> Relude.Option.getOrElseLazy(_ => "No error")
              ),
            );
            onDone(Error(err'));
          | (_, content) => onDone(Ok(content))
          }
        )
      );

  let writeFile: (string, string) => Relude.IO.t(unit, Js.Exn.t) =
    (path, content) =>
      Relude.IO.async(onDone =>
        Native.writeFile(path, content, `utf8, err =>
          switch (Js.Null.toOption(err)) {
          | Some(err') =>
            Js.Console.error(
              "Write failed: "
              ++ (
                Js.Exn.message(err')
                |> Relude.Option.getOrElseLazy(_ => "No error")
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
