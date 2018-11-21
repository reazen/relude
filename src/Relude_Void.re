/**
Void.t is a type for which it is impossible to construct a value.
*/
type t =
  | Void(t);

/**
A function that can be used when you need to provide a function from Void.t => 'a.
The function will never actually be called, because it's impossible to construct a valud of type Void.t.
 */
let absurd: t => 'a =
  void => {
    let rec go: t => 'b =
      fun
      | Void(nextVoid) => go(nextVoid);

    switch (void) {
    | Void(b) => go(b)
    };
  };

/**
Show for Void.t is absurd because there is no value of type Void.t that can be shown.
 */
let show: t => string = absurd;
