type t = {
  label: string,
  description: string,
  opcodes: array(Opcode.t),
};

let decode = (json: Js.Json.t): t => {
  open! Json.Decode;

  {
    label: json |> field("label", string),
    description: json |> field("description", string),
    opcodes: json |> field("opcodes", array(Opcode.decode)),
  };
};

let load = (path: string): array(t) => {
  Node.Fs.readFileSync(path, `binary)
  |> Json.parseOrRaise
  |> Json.Decode.array(decode);
};