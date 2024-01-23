open Syntax

let universe_label = ref 0

let fresh_name (n : string) : string =
  let res = Printf.sprintf "%s%i" n !universe_label in
  universe_label := !universe_label + 1;
  res

let int_bound = 100

type pool = { normal : process list; waiting : process list }
type res = (pool, string) result

let rec step_send_message (ch1, received1, proc1) (waiting : process list) =
  match waiting with
  | [] -> None
  | Out { ch; msg; proc } :: waiting -> (
      if eq_message ch ch1 then
        let proc1 = subst_process received1 msg proc1 in
        Some { normal = [ proc; proc1 ]; waiting }
      else
        match step_send_message (ch1, received1, proc1) waiting with
        | None -> None
        | Some { normal; waiting } ->
            Some { normal; waiting = Out { ch; msg; proc } :: waiting })
  | p :: waiting -> (
      match step_send_message (ch1, received1, proc1) waiting with
      | None -> None
      | Some { normal; waiting } -> Some { normal; waiting = p :: waiting })

let handler_waiting (waiting : process list) =
  (* let num = *)
  (*   List.fold_left (fun n p -> match p with In _ -> n + 1 | _ -> n) 0 waiting *)
  (* in *)
  let rec aux num waiting =
    if num < 0 then None
    else
      match waiting with
      | [] -> None
      | In { ch; received; proc } :: waiting -> (
          match step_send_message (ch, received, proc) waiting with
          | None -> (
              match aux (num - 1) waiting with
              | None -> None
              | Some { normal; waiting } ->
                  Some
                    { normal; waiting = In { ch; received; proc } :: waiting })
          | Some { normal; waiting } -> Some { normal; waiting })
      | p :: waiting -> aux (num - 1) (waiting @ [ p ])
  in
  aux (List.length waiting) waiting

let rec step ({ normal; waiting } : pool) : res =
  match normal with
  | [] -> Ok { normal; waiting }
  | Zero :: normal -> Ok { normal; waiting }
  | In { ch; received; proc } :: normal -> (
      let waiting = In { ch; received; proc } :: waiting in
      match handler_waiting waiting with
      | None -> step { normal; waiting }
      | Some { normal = normal'; waiting } ->
          Ok { normal = normal' @ normal; waiting })
  | Out { ch; msg; proc } :: normal -> (
      let waiting = Out { ch; msg; proc } :: waiting in
      match handler_waiting waiting with
      | None -> step { normal; waiting }
      | Some { normal = normal'; waiting } ->
          Ok { normal = normal' @ normal; waiting })
  | Parallel (p1, p2) :: normal -> Ok { normal = p1 :: p2 :: normal; waiting }
  | New (n, proc) :: normal ->
      let n' = fresh_name n in
      Ok { normal = subst_process n (Name n') proc :: normal; waiting }
  | NewInt (n, proc) :: normal ->
      let n' = Random.int int_bound in
      Ok { normal = subst_process n (CInt n') proc :: normal; waiting }
  | CheckEq { m1; m2; proc } :: normal ->
      if eq_message m1 m2 then Ok { normal = proc :: normal; waiting }
      else Ok { normal; waiting }
  | CheckNeq { m1; m2; proc } :: normal ->
      if eq_message m1 m2 then Ok { normal; waiting }
      else Ok { normal = proc :: normal; waiting }
  | Dec { encoded; decoded; key; proc } :: normal -> (
      match encoded with
      | Enc { key = k; content } when eq_message k key ->
          Ok { normal = subst_process decoded content proc :: normal; waiting }
      | _ -> Ok { normal; waiting })
  | Split { pair; fst; snd; proc } :: normal -> (
      match pair with
      | Pair (m1, m2) ->
          Ok
            {
              normal =
                subst_process fst m1 (subst_process snd m2 proc) :: normal;
              waiting;
            }
      | _ -> Ok { normal; waiting })
  | Assert { phi; proc } :: normal ->
      if check_qualifier phi then Ok { normal = proc :: normal; waiting }
      else Error (Printf.sprintf "Assert fail: %s" (layout_qualifier phi))

let layout_multi_process ps =
  List.fold_left
    (fun res p -> Printf.sprintf "%s| %s |\n" res (layout_process p))
    "" ps

let layout_pool { normal; waiting } =
  Printf.sprintf "Normal:\n%sWaitting:\n%s"
    (layout_multi_process normal)
    (layout_multi_process waiting)

let run normal =
  let pool = { normal; waiting = [] } in
  let counter = ref 0 in
  let rec aux pool =
    let _ = counter := !counter + 1 in
    let _ = Printf.printf "[Step %i]\n" !counter in
    let _ = Printf.printf "%s\n" (layout_pool pool) in
    match pool.normal with
    | [] ->
        let _ = Printf.printf "End\n" in
        ()
    | _ -> (
        match step pool with
        | Ok pool -> aux pool
        | Error msg ->
            let _ = Printf.printf "[Error:] %s\n" msg in
            ())
  in
  aux pool
