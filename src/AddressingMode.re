type t =
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

exception Unrecognized(string);

let inspect = (mode: t) => {
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

let decode = (json: Js.Json.t): t => {
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
    | _ => raise(Unrecognized(value))
    }
  };
};