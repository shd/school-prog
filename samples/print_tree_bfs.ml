type tree = Node of tree*tree*int | Leaf;;

(* Функции, предназначенные для печати строки в конкретной позиции,
   сделаны для упрощения вычислений *)

(* текущая позиция, на которой остановилась предыдущая печать *)
let print_pos = ref 0;; 

(* перевод строки *)
let print_at_newline () = print_pos := 0; print_newline ();;

(* печать строки s' в позиции pos'. Если начальная позиция левее текущей -
   обрезаем строку, чтобы оставшиеся печатаемые символы оказались бы на
   своих местах. *)
let print_at pos' s' = 
    (* корректируем начальную позицию и строку *)
    let (pos,s) = if pos' < !print_pos then (
        let start = (!print_pos - pos') in
        if start > String.length s' then failwith "pos is too to the left"
        else (!print_pos, String.sub s' start (String.length s' - start))
    ) else (pos',s') in
    Printf.printf "%.*s%s" (pos - !print_pos) "" s; 
    print_pos := pos + String.length s;;

let num_width n = String.length (string_of_int n);;

let rec tree_width p =
    match p with
        Node (l,r,n) -> num_width n + tree_width l + tree_width r
      | Leaf -> 0;;

(* print_tree: (int * tree * int) list -> unit
   Первый элемент тройки: левая граница полосы
   Второй элемент тройки: текущий узел
   Третий элемент тройки: координата родителя 
*)
let rec print_tree p =
    (* Печать линий *)
    List.iter (fun (offset,t,offset_parent) -> match t with
        Node(l,r,w) -> 
            (* вычисляем позицию текущего узла (узла t) *)
            let offset_son = offset + tree_width l + num_width w / 2 in

            (* Проверим, с какой стороны родитель относительно текущего узла *)
            if offset_parent > offset then 
                print_at (offset_son) ("/" ^ String.make (offset_parent - offset_son - 1) '-' ^ "+")
            else if offset_parent < offset then
                print_at (offset_parent) ("+" ^ String.make (offset_son - offset_parent - 1) '-' ^ "\\")
      | _ -> ()
    ) p;
    print_at_newline ();

    (* Печать значений *)
    List.iter (fun (offset, t, _) -> match t with 
        Node(l,r,w) -> print_at (offset + tree_width l) (string_of_int w);
      | Leaf -> ()
    ) p;
    print_at_newline ();

    (* Вычисляем следующую строку *)
    let next_line = List.flatten (List.map (fun (offset, x, _) -> match x with
        Node (l,r,n) -> 
            let offset_parent = offset + tree_width l + num_width n / 2 in
            (if l <> Leaf then [(offset, l, offset_parent)] else []) @ 
                (if r <> Leaf then [(offset + tree_width l + num_width n, r, offset_parent)] else [])
      | Leaf -> []
    ) p) in

    (* Проверка в рекурсивном вызове: чтобы избежать вечной рекурсии *)
    if next_line <> [] then print_tree next_line;;

print_tree [0, Node (Node (Node(Leaf,Leaf,11111),Node(Leaf,Leaf,7),12), 
                     Node (Node(Leaf,Node(Leaf,Leaf,-3),823423),Node(Node(Leaf,Leaf,799),Leaf,0),29999), 
                     99), 
            0];;
