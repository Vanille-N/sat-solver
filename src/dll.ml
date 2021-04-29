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

