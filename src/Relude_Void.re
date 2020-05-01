/**
[Void.t] is a type for which it is impossible to construct a value. This may
sound like a strange concept, but it can be very valuable for indicating at
compile time that certain case is unraeachable.

For example, the type [result('a, Void.t)] is still a [result], and you can
still do all of the normal result things to it, but you can know for a fact that
it can't be an [Error], because there would be no way to construct a valid error
payload to satisfy the [Void.t] type.
*/
type t =
  | Void(t);

/**
[Void.absurd] can be used when you need to provide a function from [Void.t] to
any other type. Because no value of void can be constructed, this function will
never actually be called.
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
[Void.show] converts a value of type [Void.t] to a [string], however, since you
can never construct a value of type [Void.t], this function is unreachable, and
is in fact just an alias for {!val:absurd}.
*/
let show: t => string = absurd;
