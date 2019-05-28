type addressing_mode =
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | Accumulator
  | Immediate
  | Indirect
  | IndirectX
  | IndirectY
  | Relative
  | ZeroPage
  | ZeroPageX
  | ZeroPageY
  | None;

exception UnrecognizedAddressingMode(string);

let decode_addressing_mode = (json: Js.Json.t): addressing_mode => {
  switch (Js.Json.decodeString(json)) {
  | None => None
  | Some(value) =>
    switch (value) {
    | "absolute" => Absolute
    | "absoluteX" => AbsoluteY
    | "absoluteY" => AbsoluteY
    | "accumulator" => Accumulator
    | "immediate" => Immediate
    | "indirect" => Indirect
    | "indirectX" => IndirectX
    | "indirectY" => IndirectY
    | "relative" => Relative
    | "zeroPage" => ZeroPage
    | "zeroPageX" => ZeroPageX
    | "zeroPageY" => ZeroPageY
    | _ => raise(UnrecognizedAddressingMode(value))
    }
  };
};

type t = {
  code: char,
  length: int,
  timing: int,
  addressing_mode: option(addressing_mode),
};

let decode = (json: Js.Json.t): t => {
  open! Json.Decode;

  {
    code: json |> field("code", int) |> Char.chr,
    length: json |> field("length", int),
    timing: json |> field("timing", int),
    addressing_mode:
      json |> optional(field("addressing_mode", decode_addressing_mode)),
  };
};