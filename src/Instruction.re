type pattern =
  | Static
  | Jump
  | Read
  | Write
  | ReadModifyWrite;

type t = {
  label: string,
  description: string,
  opcodes: array(Opcode.t),
  access_pattern: pattern,
};

exception InvalidAccessPattern(string);

let decode_access_pattern = (pattern: Js.Json.t) => {
  switch (Js.Json.decodeString(pattern)) {
  | None => Static
  | Some("jump") => Jump
  | Some("read") => Read
  | Some("write") => Write
  | Some("readModifyWrite") => ReadModifyWrite
  | Some(value) => raise(InvalidAccessPattern(value))
  };
};

let decode = (json: Js.Json.t): t => {
  open! Json.Decode;

  {
    label: json |> field("label", string),
    description: json |> field("description", string),
    opcodes: json |> field("opcodes", array(Opcode.decode)),
    access_pattern:
      json
      |> optional(field("access_pattern", decode_access_pattern))
      |> Util.default(Static),
  };
};

[@bs.module] external raw: Js.Json.t = "./instructions.json";

let all: array(t) = Json.Decode.array(decode, raw);