open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)


let simulator (programme : program) number_steps = Format.printf "IN :\n";
  let env = List.fold_left (
    fun env inp -> 
      Format.printf "%s = " inp;
      let value = read_line () in
      Env.add inp (VBit (value="1")) env
  ) Env.empty programme.p_inputs in
  Netlist_printer.print_env 
    (fun ff (key, value) ->
      Format.fprintf ff "%s = %a" key Netlist_printer.print_value value
    ) "[" ", " "]" Format.std_formatter env;

  Format.printf "@.";
  Format.printf "OUT :\n"


let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
