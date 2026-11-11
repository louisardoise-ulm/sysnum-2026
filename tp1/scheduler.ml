open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp (id, exp) = 
  let ret = function 
    | Avar x -> [x] 
    | Aconst _ -> [] in
  match exp with
    | Earg x -> ret x
    | Ereg x -> [] (* REG is updated at clock change *)
    | Erom (_,_,x) -> ret x
    | Eram (_,_,x,_,_,_) -> ret x
    | Enot x -> ret x
    | Ebinop (_, x, y) -> ret x @ ret y
    | Emux (s, x, y) -> ret s @ ret x @ ret y
    | Econcat (x,y) -> ret x @ ret y
    | Eslice (_,_,x) -> ret x
    | Eselect (_, x) -> ret x

let schedule p = 
  let g = Graph.mk_graph () in 
    List.iter (fun (i,e) -> Graph.add_node g i) p.p_eqs;
  let defined = List.map fst p.p_eqs in (*list of 'real' nodes (ie not input variables)*)
    List.iter (
      fun (i,e) -> List.iter (
        fun x -> if List.mem x defined then Graph.add_edge g x i (* Only add if its not an input variable*)
      ) (read_exp (i,e))
    ) p.p_eqs;
  if Graph.has_cycle g then raise Combinational_cycle;
  let sorted = topological g in
  let eqs = List.map (
    fun x -> 
      List.find (fun (i,e) -> i = x) p.p_eqs
  ) sorted in
  { p with p_eqs = eqs}