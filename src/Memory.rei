type t;

let build: Rom.t => t;
let copy: t => t;
let get_byte: (t, int) => int;
let get_word: (t, int) => int;
let get_indirect: (t, int) => int;
let set_byte: (t, int, int) => unit;