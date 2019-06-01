type code = int;

type t = {
  code: int,
  length: int,
  timing: int,
  addressing_mode: AddressingMode.t,
};

let decode = (json: Js.Json.t): t => {
  open! Json.Decode;

  {
    code: json |> field("code", int),
    length: json |> field("length", int),
    timing: json |> field("timing", int),
    addressing_mode:
      json
      |> optional(field("addressing_mode", AddressingMode.decode))
      |> Util.default(AddressingMode.Implicit),
  };
};