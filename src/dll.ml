type ('elt, 'mk) node = {
    data: 'elt;
    mutable mark: 'mk option;
    mutable succ: ('elt, 'mk) node;
    mutable pred: ('elt, 'mk) node;
}

type ('elt, 'mk) t = {
    mutable head: ('elt, 'mk) node option;
}

let make () =
    { head=None; }

let insert lst e =
    match lst.head with
        | None -> (
            let rec node = {
                data=e;
                mark=None;
                succ=node;
                pred=node;
            } in
            lst.head <- Some node
        )
        | Some node -> (
            let before = node in
            let after = node.succ in
            let node = {
                data=e;
                mark=None;
                succ=after;
                pred=before;
            } in
            before.succ <- node;
            after.pred <- node;
            lst.head <- Some node
        )

let rotate lst =
    lst.head <- lst.head
        |> Option.map (fun node -> node.succ)

let set_mark lst mk =
    match lst.head with
        | None -> ()
        | Some node -> node.mark <- mk

let get_mark lst =
    lst.head
    |> Option.map (fun node -> node.mark)
    |> Option.join

let peek lst =
    lst.head
    |> Option.map (fun node -> node.data)

let take lst =
    match lst.head with
        | None -> None
        | Some node when node.succ == node -> ( (* this is not a mistake, == should be used not = *)
            lst.head <- None;
            Some node.data
        )
        | Some node -> (
            let before = node.pred in
            let after = node.succ in
            before.succ <- after;
            after.pred <- before;
            lst.head <- Some after;
            Some node.data
        )

(* --- starting here nothing is public, unit tests only --- *)

module Test = struct  
    let nb_failed = ref 0
    let nb_passed = ref 0
    let failures = ref []

    let init name =
        Format.printf "\n<%s{ START }%s>\n\n" (String.make 29 '=') (String.make 29 '=');
        Format.printf "test::\x1b[33m%s\x1b[0m\n" name;
        nb_failed := 0;
        nb_passed := 0;
        failures := []

    let test name (tt:unit->unit) =
        Format.printf "  * %s   " name;
        let res =
            try (tt (); true)
            with f -> (
                failures := (name, f) :: !failures;
                false
            )
        in
        if res then (
            incr nb_passed;
            Format.printf
                "%s\x1b[32mOK\x1b[0m\n"
                (String.make (60 - (String.length name)) '.')
        ) else (
            incr nb_failed;
            Format.printf
                "%s\x1b[31mFAILURE\x1b[0m\n"
                (String.make (60 - (String.length name)) '.')
        )

    let report () =
        Printf.printf
            "\n\t\x1b[%dm*-- Summary: %d tests, %d successes, %d failures --*\x1b[0m\n"
            (if !nb_failed = 0 then 32 else 31)
            (!nb_passed + !nb_failed)
            !nb_passed
            !nb_failed;
        List.iter (fun (t,e) ->
            Printf.printf "In test <\x1b[31m%s\x1b[0m>\n" t;
            (
                try raise e
                with
                    | Assert_failure (s, i, j) ->
                        Printf.printf
                            "  Assert failed: \x1b[33m%s at (%d,%d)\x1b[0m\n"
                            s i j
                    | Failure s ->
                        Printf.printf
                            "  Failure raised: \x1b[33m%s\x1b[0m\n"
                            s
                    | exc -> Printf.printf
                            "  Other exception: \x1b[33m%s\x1b[0m\n"
                            (Printexc.to_string exc)
            )
        ) !failures;
        Format.printf "\n<%s{ END }%s>\n\n" (String.make 30 '=') (String.make 30 '=')
end
