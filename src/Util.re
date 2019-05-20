let show_hex = (string: string) => {
  Array.init(String.length(string), index =>
    Printf.sprintf("%X", Char.code(string.[index]))
  )
  |> Js.Array.joinWith(" ");
};