open Jest;
open Expect;

module List = Relude.List;
module WriterT = Relude.WriterT;
module Writer = WriterT.Writer;

module WriterLog =
  WriterT.WriterLog.List.WithEntry({
    type t = string;
  });
module WriterList = Writer.WithLog(WriterLog);

let ((<$>), ($>), (<$$>), (>>=)) =
  WriterList.Infix.((<$>), ($>), (<$$>), (>>=));

describe("WriterT", () =>
  test("pure, >>=, tell, $>, runWriterT", () => {
    // This gets a little ugly without having a do notation equivalent
    let writer =
      WriterList.pure(42)
      >>= (
        a =>
          Writer.tell(["a = " ++ string_of_int(a)])
          $> a
          * 2
          >>= (
            a =>
              Writer.tell(["a = " ++ string_of_int(a)])
              $> a
              + 5
              >>= (a => Writer.tell(["a = " ++ string_of_int(a)]) $> a)
          )
      );
    expect(writer |> Writer.runWriterT)
    |> toEqual((89, ["a = 42", "a = 84", "a = 89"]));
  })
);
