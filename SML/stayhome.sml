structure Cmap = BinaryMapFn(struct
                type ord_key = int * int
                fun compare ((x1,y1),(x2,y2)) =
                if x1<x2 then LESS else if x1>x2 then GREATER else if y1<y2 then LESS else if y1>y2 then GREATER else EQUAL
            end)


fun stayhome file =
    let
            (* A function to read an integer from specified input. *)
            (* Open input file. *)
        val inStream = TextIO.openIn file

        fun max(a,b) =
            if a<b then b
            else a

        fun pf(a,i,j,t) =
        let
            val y = Queue.enqueue(a,(i,j,t))
        in
            a
        end
        fun readGrid (y,world,history,sotiris,airports,corona,i:int,j:int,n,m,Tx,Ty) =
            case y of NONE => (world,history,sotiris,airports,corona,1,n,m,0,Tx,Ty)
            | SOME(#" ") => readGrid(TextIO.input1 inStream,world,history,sotiris,airports,corona,i,j,n,m,Tx,Ty)
            | SOME(#"\n") => readGrid(TextIO.input1 inStream,world,history,sotiris,airports,corona,i+1,0,n+1,max(m,j),Tx,Ty)
            | SOME(#"S") => readGrid(TextIO.input1 inStream,Cmap.insert(world,(i,j),#"S"),Cmap.insert(history,(i,j),0),pf(sotiris,i,j,0),airports,corona,i,j+1,n,m,Tx,Ty)
            | SOME(#"T") => readGrid(TextIO.input1 inStream,Cmap.insert(world,(i,j),#"T"),Cmap.insert(history,(i,j),0),sotiris,airports,corona,i,j+1,n,m,i,j)
            | SOME(#"A") => readGrid(TextIO.input1 inStream,Cmap.insert(world,(i,j),#"A"),Cmap.insert(history,(i,j),0),sotiris,pf(airports,i,j,0),corona,i,j+1,n,m,Tx,Ty)
            | SOME(#"W") => readGrid(TextIO.input1 inStream,Cmap.insert(world,(i,j),#"W"),Cmap.insert(history,(i,j),0),sotiris,airports,pf(corona,i,j,0),i,j+1,n,m,Tx,Ty)
            | SOME(#".") =>	readGrid(TextIO.input1 inStream,Cmap.insert(world,(i,j),#"."),Cmap.insert(history,(i,j),0),sotiris,airports,corona,i,j+1,n,m,Tx,Ty)
            | SOME(#"X") =>	readGrid(TextIO.input1 inStream,Cmap.insert(world,(i,j),#"X"),Cmap.insert(history,(i,j),0),sotiris,airports,corona,i,j+1,n,m,Tx,Ty)
            | _ => (world,history,sotiris,airports,corona,1,n,m,0,Tx,Ty)

        fun choose(x,y) =
            if x > y then x
            else y

        fun is_sotiris (x) =
            case x of #"S" => true
            | #"L" => true
            | #"U" => true
            | #"D" => true
            | #"R" => true
            | #"C" => true
            | #"E" => true
            | #"Q" => true
            | #"V" => true
            | _ => false

        fun original_sotiris(x) =
            case x of #"S" => true
            | #"L" => true
            | #"U" => true
            | #"R" => true
            | #"D" => true
            | _ => false

        fun airport_sotiris(x) =
            case x of #"C" => true
            | #"E" => true
            | #"Q" => true
            | #"V" => true
            | _ => false

        fun transform(x) =
            case x of #"D" => #"C"
            | #"L" => #"E"
            | #"R" => #"Q"
            | #"U" => #"V"
            | _ => x

        fun lower (x) = Char.chr(Char.ord x + Char.ord#"a" - Char.ord#"A")

        fun update_s(world,history,sotiris, airports, corona,i:int,j:int,c:char,t,n,m,t_airports,Tx,Ty) =
            if j<m andalso j>=0 andalso i<n andalso i>=0 then
                    let
                        val	x = valOf(Cmap.find(world,(i,j)))
                        val	y = valOf(Cmap.find(history, (i,j)))
                    in
                        case c of #"R" =>
                            if x = #"." orelse x = #"T"  then update_s(Cmap.insert(world,(i,j),c),Cmap.insert(history, (i,j),t),pf(sotiris,i,j,t),airports,corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                            else if x = #"A" then update_s(Cmap.insert(world,(i,j),#"Q"),Cmap.insert(history, (i,j),t),pf(sotiris,i,j,t),airports,corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                            else if  original_sotiris(x) andalso y = t then update_s(Cmap.insert(world,(i,j),choose(c,x)),history,sotiris,airports,corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                            else if  airport_sotiris(x) andalso y = t then update_s(Cmap.insert(world,(i,j),choose(transform(c),x)),history,sotiris,airports,corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                            else update_s(world,history,sotiris,airports,corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                        | #"L" =>
                            if  x = #"." orelse x = #"T" then update_s(Cmap.insert(world,(i,j),c),Cmap.insert(history, (i,j),t),pf(sotiris,i,j,t), airports, corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                            else if x = #"A" then update_s(Cmap.insert(world,(i,j),#"E"),Cmap.insert(history, (i,j),t),pf(sotiris,i,j,t),airports,corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                            else if  original_sotiris(x) andalso y = t then update_s(Cmap.insert(world,(i,j),choose(c,x)),history,sotiris,airports,corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                            else if airport_sotiris(x) andalso y = t then update_s(Cmap.insert(world,(i,j),choose(transform(c),x)),history,sotiris,airports,corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                            else update_s(world,history,sotiris,airports,corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                        | #"D" =>
                            if  x = #"." orelse x = #"T" then update_s(Cmap.insert(world,(i,j),c),Cmap.insert(history, (i,j),t),pf(sotiris,i,j,t),airports, corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                            else if x = #"A" then update_s(Cmap.insert(world,(i,j),#"C"),Cmap.insert(history, (i,j),t),pf(sotiris,i,j,t),airports,corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                            else if  original_sotiris(x) andalso y = t then update_s(Cmap.insert(world,(i,j),choose(c,x)),history,sotiris,airports,corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                            else if  airport_sotiris(x) andalso y = t then update_s(Cmap.insert(world,(i,j),choose(transform(c),x)),history,sotiris,airports,corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                            else update_s(world,history,sotiris,airports,corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                        | #"U" =>
                            if  x = #"."orelse x = #"T" then update_sotiris(Cmap.insert(world,(i,j),c),Cmap.insert(history, (i,j),t),pf(sotiris,i,j,t),airports, corona,t,n,m,t_airports,Tx,Ty)
                            else if x = #"A" then update_sotiris(Cmap.insert(world,(i,j),#"V"),Cmap.insert(history, (i,j),t),pf(sotiris,i,j,t),airports,corona,t,n,m,t_airports,Tx,Ty)
                            else if  original_sotiris(x) andalso y = t then update_sotiris(Cmap.insert(world,(i,j),choose(c,x)),history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                            else if  airport_sotiris(x) andalso y = t then update_sotiris(Cmap.insert(world,(i,j),choose(transform(c),x)),history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                            else update_sotiris(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                        | _ => update_sotiris(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                    end
            else
                    case c of #"R" =>
                        update_s(world,history,sotiris,airports,corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                    | #"L" =>
                        update_s(world,history,sotiris,airports,corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                    | #"D" =>
                        update_s(world,history,sotiris,airports,corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                    | #"U" =>
                        update_sotiris(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                    | _ => update_sotiris(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)

        and update_sotiris (world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty) =
            if (is_sotiris(valOf(Cmap.find(world,(Tx,Ty))))) then answer(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
            else if Queue.isEmpty(sotiris) then answer(world,history,sotiris,airports,corona,t,n,m,t_airports, Tx,Ty)
            else
            let
                    val (i,j,time) = Queue.dequeue(sotiris)
                in
                    if (is_sotiris(valOf(Cmap.find(world,(i,j))))) then
                        case time-t of 0 =>  update_corona(world,history,pf(sotiris,i,j,time),airports,corona,t,n,m,t_airports,Tx,Ty)
                        | _ =>  update_s(world,history,sotiris,airports,corona,i,j+1,#"R",t,n,m,t_airports,Tx,Ty)
                    else update_sotiris(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                end

        and update_c(world,history,sotiris,airports,corona,i:int,j:int,c:char,t,n,m,t_airports,Tx,Ty) =
            if j<m andalso j>=0 andalso i<n andalso i>=0 then
                    let
                        val x = valOf(Cmap.find(world,(i,j)))
                        val y = valOf(Cmap.find(history,(i,j)))
                    in
                        case c of #"R" =>
                            if x = #"." orelse x = #"T" then update_c(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                            else if (x = #"A" orelse (airport_sotiris(x))) andalso t_airports = 0 then update_c(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),i,j-2,#"L",t,n,m,t+5,Tx,Ty)
                            else if (x = #"A" orelse (airport_sotiris(x))) andalso t_airports <> 0 then update_c(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                            else if is_sotiris(x) then update_c(Cmap.insert(world,(i,j),lower(x)),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                            else update_c(world,history,sotiris,airports,corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                        | #"L" =>
                            if x = #"." orelse x = #"T" then update_c(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris, airports,pf(corona,i,j,t),i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                            else if (x = #"A" orelse (airport_sotiris(x))) andalso t_airports = 0 then update_c(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),i+1,j+1,#"D",t,n,m,t+5,Tx,Ty)
                            else if (x = #"A" orelse (airport_sotiris(x))) andalso t_airports <> 0 then update_c(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                            else if is_sotiris(x) then update_c(Cmap.insert(world,(i,j),lower(x)),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                            else update_c(world,history,sotiris,airports,corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                        | #"D" =>
                            if x = #"." orelse x = #"T" then update_c(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris, airports,pf(corona,i,j,t),i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                            else if (x = #"A" orelse (airport_sotiris(x))) andalso t_airports = 0 then update_c(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),i-2,j,#"U",t,n,m,t+5,Tx,Ty)
                            else if (x = #"A" orelse (airport_sotiris(x))) andalso t_airports <> 0 then update_c(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                            else if is_sotiris(x) then update_c(Cmap.insert(world,(i,j),lower(x)),Cmap.insert(history, (i,j),t),sotiris, airports,pf(corona,i,j,t),i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                            else update_c(world,history,sotiris,airports,corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                        | #"U" =>
                            if x = #"." orelse x = #"T" then update_corona(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),t,n,m,t_airports,Tx,Ty)
                            else if (x = #"A" orelse (airport_sotiris(x))) andalso t_airports = 0 then update_corona(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),t,n,m,t+5,Tx,Ty)
                            else if(x = #"A" orelse (airport_sotiris(x))) andalso t_airports <> 0 then update_corona(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),t,n,m,t_airports,Tx,Ty)
                            else if is_sotiris(x) then update_corona(Cmap.insert(world,(i,j),lower(x)),Cmap.insert(history, (i,j),t),sotiris,airports,pf(corona,i,j,t),t,n,m,t_airports,Tx,Ty)
                            else update_corona(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                        | _ =>  update_corona(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                    end
            else
                    case c of #"R" =>
                        update_c(world,history,sotiris,airports,corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                    | #"L" =>
                        update_c(world,history,sotiris,airports,corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                    | #"D" =>
                        update_c(world,history,sotiris,airports,corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                    | #"U" =>
                        update_corona(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                    | _ => update_corona(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)

            and update_a(world,history,sotiris,airports,corona,i:int,j:int,c:char,t,n,m,t_airports,Tx,Ty) =
                if j<m andalso j>=0 andalso i<n andalso i>=0 then
                        let
                            val x = valOf(Cmap.find(world,(i,j)))
                            val y = valOf(Cmap.find(history,(i,j)))
                        in
                            case c of #"R" =>
                                if x = #"." orelse x = #"T" then update_a(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,pf(airports,i,j,t),corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                                else if is_sotiris(x) then update_a(Cmap.insert(world,(i,j),lower(x)),Cmap.insert(history, (i,j),t),sotiris,pf(airports,i,j,t),corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                                else update_a(world,history,sotiris,airports,corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                            | #"L" =>
                                if x = #"." orelse x = #"T" then update_a(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris, pf(airports,i,j,t),corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                                else if is_sotiris(x) then update_a(Cmap.insert(world,(i,j),lower(x)),Cmap.insert(history, (i,j),t),sotiris,pf(airports,i,j,t),corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                                else update_a(world,history,sotiris,airports,corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                            | #"D" =>
                                if x = #"." orelse x = #"T" then update_a(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris, pf(airports,i,j,t),corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                                else if is_sotiris(x) then update_a(Cmap.insert(world,(i,j),lower(x)),Cmap.insert(history, (i,j),t),sotiris, pf(airports,i,j,t),corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                                else update_a(world,history,sotiris,airports,corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                            | #"U" =>
                                if x = #"." orelse x = #"T" then update_corona(Cmap.insert(world,(i,j),#"W"),Cmap.insert(history, (i,j),t),sotiris,pf(airports,i,j,t),corona,t,n,m,t_airports,Tx,Ty)
                                else if is_sotiris(x) then update_corona(Cmap.insert(world,(i,j),lower(x)),Cmap.insert(history, (i,j),t),sotiris,pf(airports,i,j,t),corona,t,n,m,t_airports,Tx,Ty)
                                else update_corona(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                            | _ =>  update_corona(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                        end
                else
                        case c of #"R" =>
                            update_a(world,history,sotiris,airports,corona,i,j-2,#"L",t,n,m,t_airports,Tx,Ty)
                        | #"L" =>
                            update_a(world,history,sotiris,airports,corona,i+1,j+1,#"D",t,n,m,t_airports,Tx,Ty)
                        | #"D" =>
                            update_a(world,history,sotiris,airports,corona,i-2,j,#"U",t,n,m,t_airports,Tx,Ty)
                        | #"U" =>
                            update_corona(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                        | _ => update_corona(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)

        and update_airports(world,i,j) =
            if is_sotiris(valOf(Cmap.find(world,(i,j)))) then Cmap.insert(world,(i,j),lower(valOf(Cmap.find(world,(i,j)))))
            else Cmap.insert(world,(i,j),#"W")

        and update_corona(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty) =
            if (t_airports<>0) andalso (t mod 2 = 1) andalso (t>=t_airports) then
                if Queue.isEmpty(airports) then answer(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                else if t = t_airports then
                    let
                        val (i,j,time) = Queue.dequeue(airports)
                    in
                        case time-t of 0 => answer(world,history,sotiris,pf(airports,i,j,time),corona,t,n,m,t_airports,Tx,Ty)
                        | _ => update_corona(update_airports(world,i,j),history,sotiris,pf(airports,i,j,t),corona,t,n,m,t_airports,Tx,Ty)
                    end
                else
                    let
                        val (i,j,time) = Queue.dequeue(airports)
                    in
                        case time-t of 0 => answer(world,history,sotiris,pf(airports,i,j,time),corona,t,n,m,t_airports,Tx,Ty)
                        | _ => update_a(world,history,sotiris,airports,corona,i,j+1,#"R",t,n,m,t_airports,Tx,Ty)
                    end
            else if (t mod 2 = 0) then
                if Queue.isEmpty(corona) then answer(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)
                else
                    let
                        val (i,j,time) = Queue.dequeue(corona)
                        (* val _ = print(Int.toString(time)^" "^Int.toString(t)) *)
                    in
                        case time-t of 0 => answer(world,history,sotiris,airports,pf(corona,i,j,time),t,n,m,t_airports,Tx,Ty)
                        | _ => update_c(world,history,sotiris,airports,corona,i,j+1,#"R",t,n,m,t_airports,Tx,Ty)
                    end
            else answer(world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty)

        and str_answer(world,i,j,str) =
            let
                val c = Char.toString(valOf(Cmap.find(world,(i,j))))
            in
                case c of "R" => str_answer(world,i,j-1,str) ^ "R"
                | "r" =>  str_answer(world,i,j-1,str) ^ "R"
                | "Q" => str_answer(world,i,j-1,str) ^ "R"
                | "q" => str_answer(world,i,j-1,str) ^ "R"
                | "L" => str_answer(world,i,j+1,str) ^ "L"
                | "l" => str_answer(world,i,j+1,str) ^ "L"
                | "E" => str_answer(world,i,j+1,str) ^ "L"
                | "e" => str_answer(world,i,j+1,str) ^ "L"
                | "U" => str_answer(world,i+1,j,str) ^ "U"
                | "u" => str_answer(world,i+1,j,str) ^ "U"
                | "V" => str_answer(world,i+1,j,str) ^ "U"
                | "v" => str_answer(world,i+1,j,str) ^ "U"
                | "D" => str_answer(world,i-1,j,str) ^ "D"
                | "d" => str_answer(world,i-1,j,str) ^ "D"
                | "C" => str_answer(world,i-1,j,str) ^ "D"
                | "c" => str_answer(world,i-1,j,str) ^ "D"
                | "S" => str
                | "s" => str
                | _ => str
        end

        and answer (world,history,sotiris,airports,corona,t,n,m,t_airports,Tx,Ty) =
            let
            in
                if (is_sotiris(valOf(Cmap.find(world,(Tx,Ty))))) then print(Int.toString(t)^"\n"^str_answer(world,Tx,Ty,"")^"\n")
                else if (Queue.isEmpty(sotiris)) then print("IMPOSSIBLE\n")
                else if valOf(Cmap.find(world,(Tx,Ty))) = #"W" then print("IMPOSSIBLE\n")
                else
                      update_sotiris(world, history,sotiris,airports,corona,t+1, n, m,t_airports,Tx,Ty)
            end
        in
                update_sotiris(readGrid(TextIO.input1 inStream,Cmap.empty,Cmap.empty,Queue.mkQueue(),Queue.mkQueue(),Queue.mkQueue(),0,0,0,0,0,0))
end
