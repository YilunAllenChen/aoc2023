open Stdio

let load_data day = In_channel.read_lines ("data/" ^ day)
