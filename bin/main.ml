open Phi_calculus
open Syntax
open Eval

let key = Name "k"
let ch = Name "ch"
(* let costPrice = CInt 50 *)

let buyer =
  In
    {
      ch;
      received = "ch_quote";
      proc =
        NewInt
          ( "price",
            New
              ( "ch_result",
                Out
                  {
                    ch = Name "ch_quote";
                    msg =
                      Enc
                        { key; content = Pair (Name "price", Name "ch_result") };
                    proc =
                      In
                        { ch = Name "ch_result"; received = "msg"; proc = Zero };
                  } ) );
    }

let seller =
  NewInt
    ( "expectedPrice",
      New
        ( "ch_quote",
          Out
            {
              ch;
              msg = Name "ch_quote";
              proc =
                In
                  {
                    ch = Name "ch_quote";
                    received = "c";
                    proc =
                      Dec
                        {
                          encoded = Name "c";
                          decoded = "pair";
                          key;
                          proc =
                            Split
                              {
                                pair = Name "pair";
                                fst = "fst";
                                snd = "snd";
                                proc =
                                  Out
                                    {
                                      ch = Name "snd";
                                      msg = Name "fst";
                                      proc = Zero;
                                    };
                              };
                        };
                  };
            } ) )

(* let proc = Zero *)
let () = run [ buyer; seller ]
