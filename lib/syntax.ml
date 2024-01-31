type message =
  | CInt of int
  | Name of string
  | Pair of message * message
  | Inl of message
  | Inr of message
  | Enc of { key : message; content : message }

type qualifier =
  | Top
  | Bot
  | Eq of message * message
  | And of qualifier * qualifier
  | Not of qualifier

type process =
  | Zero
  | In of { ch : message; received : string; proc : process }
  | Out of { ch : message; msg : message; proc : process }
  | Parallel of process * process
  | New of string * process
  | NewInt of string * process
  | CheckEq of { m1 : message; m2 : message; proc : process }
  | CheckNeq of { m1 : message; m2 : message; proc : process }
  | Dec of {
      encoded : message;
      decoded : string;
      key : message;
      proc : process;
    }
  | Split of { pair : message; fst : string; snd : string; proc : process }
  | Assert of { phi : qualifier; proc : process }

let rec subst_message (x : string) (msg : message) (body : message) =
  match body with
  | CInt _ -> body
  | Name name -> if String.equal name x then msg else Name name
  | Pair (m1, m2) -> Pair (subst_message x msg m1, subst_message x msg m2)
  | Inl m -> Inl (subst_message x msg m)
  | Inr m -> Inr (subst_message x msg m)
  | Enc { key; content } ->
      Enc
        { key = subst_message x msg key; content = subst_message x msg content }

let rec eq_message (m1 : message) (m2 : message) =
  match (m1, m2) with
  | CInt n1, CInt n2 -> Int.equal n1 n2
  | Name n1, Name n2 -> String.equal n1 n2
  | Pair (m11, m12), Pair (m21, m22) -> eq_message m11 m21 && eq_message m12 m22
  | Inl m1, Inl m2 -> eq_message m1 m2
  | Inr m1, Inr m2 -> eq_message m1 m2
  | Enc { key = k1; content = c1 }, Enc { key = k2; content = c2 } ->
      eq_message k1 k2 && eq_message c1 c2
  | _, _ -> false

let rec check_qualifier phi : bool =
  match phi with
  | Top -> true
  | Bot -> false
  | Eq (m1, m2) -> eq_message m1 m2
  | And (phi1, phi2) -> check_qualifier phi1 && check_qualifier phi2
  | Not phi -> not (check_qualifier phi)

let rec subst_qualifier (x : string) (msg : message) (phi : qualifier) =
  match phi with
  | Top | Bot -> phi
  | Eq (m1, m2) -> Eq (subst_message x msg m1, subst_message x msg m2)
  | And (phi1, phi2) ->
      And (subst_qualifier x msg phi1, subst_qualifier x msg phi2)
  | Not phi -> Not (subst_qualifier x msg phi)

let rec subst_process (x : string) (m : message) (proc : process) =
  match proc with
  | Zero -> Zero
  | In { ch; received; proc } ->
      let ch = subst_message x m ch in
      if String.equal received x then In { ch; received; proc }
      else In { ch; received; proc = subst_process x m proc }
  | Out { ch; msg; proc } ->
      Out
        {
          ch = subst_message x m ch;
          msg = subst_message x m msg;
          proc = subst_process x m proc;
        }
  | Parallel (p1, p2) -> Parallel (subst_process x m p1, subst_process x m p2)
  | New (n, proc) ->
      if String.equal n x then New (n, proc) else New (n, subst_process x m proc)
  | NewInt (n, proc) ->
      if String.equal n x then NewInt (n, proc)
      else NewInt (n, subst_process x m proc)
  | CheckEq { m1; m2; proc } ->
      CheckEq
        {
          m1 = subst_message x m m1;
          m2 = subst_message x m m2;
          proc = subst_process x m proc;
        }
  | CheckNeq { m1; m2; proc } ->
      CheckNeq
        {
          m1 = subst_message x m m1;
          m2 = subst_message x m m2;
          proc = subst_process x m proc;
        }
  | Dec { encoded; decoded; key; proc } ->
      let encoded = subst_message x m encoded in
      let key = subst_message x m key in
      if String.equal x decoded then Dec { encoded; decoded; key; proc }
      else Dec { encoded; decoded; key; proc = subst_process x m proc }
  | Split { pair; fst; snd; proc } ->
      let pair = subst_message x m pair in
      if String.equal x fst || String.equal x snd then
        Split { pair; fst; snd; proc }
      else Split { pair; fst; snd; proc = subst_process x m proc }
  | Assert { phi; proc } ->
      Assert { phi = subst_qualifier x m phi; proc = subst_process x m proc }

open Printf

let rec layout_message (msg : message) =
  match msg with
  | CInt n -> string_of_int n
  | Name name -> name
  | Pair (m1, m2) -> sprintf "(%s, %s)" (layout_message m1) (layout_message m2)
  | Inl m -> sprintf "Inl(%s)" (layout_message m)
  | Inr m -> sprintf "Inr(%s)" (layout_message m)
  | Enc { content; key } ->
      sprintf "{%s}_{%s}" (layout_message content) (layout_message key)

let rec layout_qualifier (phi : qualifier) =
  match phi with
  | Top -> "⊤"
  | Bot -> "⊥"
  | Eq (phi1, phi2) ->
      sprintf "%s = %s" (layout_message phi1) (layout_message phi2)
  | And (phi1, phi2) ->
      sprintf "%s ∧ %s" (layout_qualifier phi1) (layout_qualifier phi2)
  | Not phi -> sprintf "¬ %s" (layout_qualifier phi)

let rec layout_process (proc : process) =
  match proc with
  | Zero -> "Zero"
  | In { ch; received; proc } ->
      sprintf "%s?%s.%s" (layout_message ch) received (layout_process proc)
  | Out { ch; msg; proc } ->
      sprintf "%s!%s.%s" (layout_message ch) (layout_message msg)
        (layout_process proc)
  | Parallel (p1, p2) ->
      sprintf "%s | %s" (layout_process p1) (layout_process p2)
  | New (n, proc) -> sprintf "(ν%s)%s" n (layout_process proc)
  | NewInt (n, proc) -> sprintf "(iν%s)%s" n (layout_process proc)
  | CheckEq { m1; m2; proc } ->
      sprintf "check %s is %s.%s" (layout_message m1) (layout_message m2)
        (layout_process proc)
  | CheckNeq { m1; m2; proc } ->
      sprintf "check %s is not %s.%s" (layout_message m1) (layout_message m2)
        (layout_process proc)
  | Dec { encoded; decoded; key; proc } ->
      sprintf "decrypt %s is %s.%s" (layout_message encoded)
        (layout_message (Enc { key; content = Name decoded }))
        (layout_process proc)
  | Split { pair; fst; snd; proc } ->
      sprintf "split %s is %s.%s" (layout_message pair)
        (layout_message (Pair (Name fst, Name snd)))
        (layout_process proc)
  | Assert { phi; proc } ->
      sprintf "assert %s.%s" (layout_qualifier phi) (layout_process proc)
