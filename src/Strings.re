/* TODO: use StringLabels to avoid name clash with just "String".  We might want to consider not using the namespace=true setup, and have fully-qualified module names. */

let length: string => int = s => StringLabels.length(s)

let trim: string => string = StringLabels.trim

let isEmpty: string => bool = s => length(s) == 0

let isNotEmpty: string => bool = s => !isEmpty(s)

/* Wraps the given string in Some if it is not empty */
let toNonEmpty: string => option(string) = s => if (s |> isEmpty) None else Some(s)

let isWhitespace: string => bool = s => s |> trim |> isEmpty

/* Wraps the given string in Some if it is not whitespace.  Note: this doesn't trim off leading or trailing whitespace. */
let toNonWhitespace: string => option(string) = s => if (s |> isWhitespace) None else Some(s)

