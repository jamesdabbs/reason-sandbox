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
  | Implicit;

type code = int;

type t = {
  code: int,
  length: int,
  timing: int,
  addressing_mode,
};

exception UnrecognizedAddressingMode(string);

let inspect_addressing_mode = (mode: addressing_mode) => {
  switch (mode) {
  | Absolute => "absolute"
  | AbsoluteX => "absoluteX"
  | AbsoluteY => "absoluteY"
  | Accumulator => "accumulator"
  | Immediate => "immediate"
  | Indirect => "indirect"
  | IndirectX => "indirectX"
  | IndirectY => "indirectY"
  | Relative => "relative"
  | ZeroPage => "zeroPage"
  | ZeroPageX => "zeroPageX"
  | ZeroPageY => "zeroPageY"
  | Implicit => "implicit"
  };
};

let decode_addressing_mode = (json: Js.Json.t): addressing_mode => {
  switch (Js.Json.decodeString(json)) {
  | None => Implicit
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

let decode = (json: Js.Json.t): t => {
  open! Json.Decode;

  {
    code: json |> field("code", int),
    length: json |> field("length", int),
    timing: json |> field("timing", int),
    addressing_mode:
      json
      |> optional(field("addressing_mode", decode_addressing_mode))
      |> Util.default(Implicit),
  };
};