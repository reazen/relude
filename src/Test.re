type t('a, 'e) =
  | Pure('a): t('a, 'e)
  | Throw('e): t('a, 'e)
  | Map('r => 'a, t('r, 'e)): t('a, 'e);

let pure: 'a => t('a, 'e) = a => Pure(a);

let throw: 'e => t('a, 'e) = e => Throw(e);

let map: ('a => 'b, t('a, 'e)) => t('b, 'e) =
  (aToB, ioA) => Map(aToB, ioA);

let rec mapError: 'e1 'e2 'a. (('e1 => 'e2, t('a, 'e1)) => t('a, 'e2)) =
  (e1ToE2, ioA) =>
    switch (ioA) {
    | Pure(a) => Pure(a)
    | Throw(e) => Throw(e1ToE2(e))
    | Map(rToA, ioR) => Map(rToA, ioR |> mapError(e1ToE2))
    };
