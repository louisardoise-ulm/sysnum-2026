let print_only = ref false
let number_steps = ref (-1)

(*let simulator program number_steps = 
  (* First initate all inputs *)
  Format.printf "IN :\n";
  let read_inputs p =
    List.fold_left
      (fun env input ->
        Printf.printf "%s = " input;
        let value = read_line () in
        Env.add input (Netlist_ast.VBit (value = "1")) env
      ) Env.empty p.p_inputs in
  let inp = read_inputs program in
  (* Print outputs *)
  Format.printf "OUT :\n";*)

let simulator program number_steps = failwith "hmm"
  (*Format.printf "IN :\n";
  let env = List.fold_left (
    fun env inp -> 
      Format.printf "%s = " inp;
      let value = read_line () in
      Netlist_ast.Env.add inp (if (value="1") then 1 else 0) env
  ) Netlist_ast.Env.empty program.p_inputs in
  Format.printf "PROCESSING :\n";
  Format.printf "OUT :\n"*)


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
