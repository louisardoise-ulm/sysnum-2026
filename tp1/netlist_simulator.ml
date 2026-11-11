open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)
let number_steps = ref 2
let rom_size = 16
let ram_size = 16

let value_to_dec value = match value with (* MSD *)
  | VBit b -> if b then 1 else 0
  | VBitArray a -> Array.fold_right (fun elm acc -> (if elm then 1 else 0)*(int_of_float (2.**(float_of_int acc)))) a 0

let fetch_bool x env_ref = match x with
  | Aconst (VBit b) -> b
  | Aconst (VBitArray _) -> failwith "On avait dit un Bit, pourquoi y'a une array la..." 
  | Avar ident -> (
    match Env.find ident !env_ref with
    | VBit b -> b
    | VBitArray _ -> failwith "On avait dit un Bit, pourquoi y'a une array la..." )

let fetch_arg x env_ref = match x with
  | Aconst b -> b
  | Avar ident -> Env.find ident !env_ref 

let compute exp env_ref = match exp with
  | Earg x -> VBit(fetch_bool x env_ref) (* DONE *)

  | Ereg x ->  Env.find x !env_ref  (* DONE *) (* A priori pas de pb, le scheduler va ts les fetch au debut de la loop*)
  | Enot x -> VBit (not (fetch_bool x env_ref))(* DONE *)
  | Ebinop(op, x, y) -> (* DONE *)
    let a = fetch_bool x env_ref in
    let b = fetch_bool y env_ref in
    (match op with 
    | Or -> VBit (a || b)
    | And -> VBit (a && b)
    | Xor -> VBit (a <> b)
    | Nand -> VBit (not (a && b)))
  
  | Emux (s,x,y) -> if fetch_bool s env_ref then fetch_arg x env_ref else fetch_arg y env_ref (* DONE *) (* should handle all types *)
  | Erom (addr_s, word_s, r_addr) -> (match fetch_arg (Avar "ROM") env_ref with(* DONE *)
    | VBitArray x ->
      VBitArray (Array.sub x 
      (word_s * (value_to_dec (fetch_arg r_addr env_ref))) (* Get location = addr_size*addr but r_addr is a Bit or an array of Bit*)
      word_s) 
    | _ -> failwith "ROM should be array")
  
  | Eram (addr_s, word_s, r_addr, w_enable, w_addr, data) -> (match fetch_arg (Avar "RAM") env_ref with
    | VBitArray x -> (match fetch_arg w_enable env_ref with
      | VBit w -> if not w then (
          VBitArray (Array.sub x 
          (word_s * (value_to_dec (fetch_arg r_addr env_ref))) (* Get location = addr_size*addr but r_addr is a Bit or an array of Bit*)
          word_s) 
        ) else (
          let addr = (word_s * (value_to_dec (fetch_arg w_addr env_ref))) in
          match fetch_arg data env_ref with
          | VBit b ->
              x.(addr) <- b;
              VBit b
          | VBitArray a ->
              Array.blit a 0 x addr (Array.length a);
              VBitArray a
        )
      | _ -> failwith "Expected a Bit" )
    | _ -> failwith "RAM should be array")
  
  | Econcat (a1,a2) -> (* DONE *)
    let a = fetch_arg a1 env_ref in
    let b = fetch_arg a2 env_ref in
    (match (a,b) with 
    | (VBitArray a1, VBitArray a2) -> VBitArray(Array.append a1 a2)
    | (VBit a1, VBit a2) -> VBitArray([|a1; a2|])
    | (VBitArray a1, VBit a2) -> VBitArray(Array.append a1 [|a2|])
    | (VBit a1, VBitArray a2) -> VBitArray(Array.append [|a1|] a2))
  
  | Eslice (s,e,a) -> (* DONE *)
    (match fetch_arg a env_ref with 
    | VBit b -> if e=s then VBit b else failwith "Wrong start/end for slicing"
    | VBitArray x -> 
        if e=s then VBit x.(s)
        else VBitArray (Array.sub x s (e-s+1))
    )
  
  | Eselect (i,a) -> (* DONE *)
    (match fetch_arg a env_ref with
    | VBit x -> 
        if i=0 then VBit x 
        else failwith "couldnt select from a single Bit"
    | VBitArray x -> VBit x.(i))
  
let parse_input value =
  if String.length value = 0 then
    failwith "no input given"
  else if String.length value = 1 then
    VBit (value.[0] = '1')
  else
    VBitArray (
      Array.init (String.length value)
        (fun i -> value.[i] = '1')
    )
let simulator (programme : program) number_steps = 
  Format.printf "IN :@.";
  let env = ref Env.empty in

  (* allocate memory *)
  env := Env.add "ROM" (VBitArray (Array.make rom_size false)) !env;
  env := Env.add "RAM" (VBitArray (Array.make ram_size false)) !env;

  (* set all variables to 0*size *)
  Env.iter (fun ident ty -> match ty with
    | TBit -> env := Env.add ident (VBit false) !env
    | TBitArray n -> env := Env.add ident (VBitArray (Array.make n false)) !env
  ) programme.p_vars;

  (* loop *)
  for tour = number_steps downto 1 do
    (* get all hyper-parameters *)
    List.iter (
      fun inp ->
        let l = match Env.find inp programme.p_vars with 
          | TBit -> 1
          | TBitArray n -> n in
        Format.printf "%s %i bit(s) = " inp l;
        Format.pp_print_flush Format.std_formatter (); (*Pas de moi, j'ai demandé au chat pourquoi ca fonctionnait pas...*)
        let value = read_line () in
        env := Env.add inp (parse_input value) !env
    ) programme.p_inputs;
    List.iter (
      fun (ident, exp) ->
        env := Env.add ident (compute exp env) !env;
    ) programme.p_eqs;
    List.iter (
      fun (ident, exp) ->
        match exp with
        | Eram (addr_s, word_s, r_addr, w_enable, w_addr, data) -> (match fetch_arg (Avar "RAM") env with
            | VBitArray x -> (match fetch_arg w_enable env with
              | VBit w -> if w then (
                  let addr = (word_s * (value_to_dec (fetch_arg w_addr env))) in
                  match fetch_arg data env with
                  | VBit b ->
                      x.(addr) <- b;
                  | VBitArray a ->
                      Array.blit a 0 x addr (Array.length a);
                ) else ()
              | _ -> failwith "Expected a Bit" )
            | _ -> failwith "RAM should be array")
        | _ -> ()
        (*Format.printf "@."*)
    ) programme.p_eqs;
    Netlist_printer.print_env
      (fun ff (key, value) -> Format.fprintf ff "%s=%a" key Netlist_printer.print_value value)
      "[" ", " "]"
      Format.std_formatter
      !env;
    Format.printf "@.";
  done;

  (* print result *)
  Format.printf "@.@.@.";
  Format.printf "OUT :@.";
  List.iter
  (fun output ->
    let value = Env.find output !env in
    Format.printf "%s = %a@." output Netlist_printer.print_value value
  ) programme.p_outputs


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
