let checksyntax (s  : string) : bool = 
	let rec reccheck (s : char list) (p : int) (lamon : int) : bool =
		begin match s with
		| [] -> p = 0 && lamon = 0
		| x :: xs -> begin match x with
			| '\\' -> if lamon = 3 || lamon = 2 then (let () = print_string "1" in false) else reccheck xs p 3
			| '.'  -> if lamon != 2 then false else reccheck xs p 1
			| '('  -> if lamon > 1 then  false else reccheck xs (p+1) 0
			| ')'  -> if lamon > 1 then  false else if p = 0 then false else reccheck xs (p-1) 0
			| _    -> if lamon = 2 then (let () = print_string "5" in false) else reccheck xs p (if lamon = 0 then 0 else pred lamon)	 
			end 
		end
	in reccheck (explode s) 0 0 
  
  let main () = let s = read_line in checksyntax s ;;
