(*
   Accumulator of float values and counts.
*)

open Printf
open Lwt
open Lat_t

let flush_every = 60. (* 1 minute *)
let cleanup_older_than = 14. *. 86400. (* 2 weeks *)

type stat = {
  mutable count : int;
  mutable sum : float;
}

type acc = {
  mutable next_flush : float;
  tbl : (string, stat) Hashtbl.t
}

let create () =
  { next_flush = Unix.time () +. flush_every;
    tbl = Hashtbl.create 100 }

let metric_of_stat name stat =
  let count = stat.count in
  assert (count > 0);
  let mean = stat.sum /. float count in
  { metric_name = name;
    metric_count = count;
    metric_mean = mean }

let metric_list_of_acc acc =
  let items =
    Hashtbl.fold (fun name stat l ->
      metric_of_stat name stat :: l
    ) acc.tbl []
  in
  { items }

let is_acceptable x =
  match classify_float x with
  | FP_normal
  | FP_subnormal
  | FP_zero -> true
  | FP_nan
  | FP_infinite -> false

let flush_acc acc =
  let k = Uuid.make () in
  let value = metric_list_of_acc acc in
  Lat_access.Latency_stat.unprotected_put k value (Util_time.now ())
  >>= fun () ->
  Hashtbl.clear acc.tbl;
  return ()

let maybe_flush acc =
  let t = Unix.time () in
  if t >= acc.next_flush then (
    acc.next_flush <- max t (acc.next_flush +. flush_every);
    flush_acc acc
  )
  else
    return ()

(*
   Add a datapoint.
*)
let add acc name v =
  if is_acceptable v then (
    let tbl = acc.tbl in
    let stat =
      try Hashtbl.find tbl name
      with Not_found ->
        let stat = { count = 0; sum = 0. } in
        Hashtbl.add tbl name stat;
        stat
    in
    stat.count <- stat.count + 1;
    stat.sum <- stat.sum +. v
  );
  maybe_flush acc

(*
   Eliminate old data
*)
let cleanup () =
  let some_time_ago =
    Util_time.of_float (Unix.time () -. cleanup_older_than)
  in
  let stream = Lat_access.Latency_stat.to_stream ~max_ord:some_time_ago () in
  Lwt_stream.iter_s (fun (k, v, ord) ->
    Lat_access.Latency_stat.unprotected_delete k
  ) stream

let add_to_agg_tbl tbl metric =
  let { metric_name; metric_count; metric_mean } = metric in
  let sum, count =
    try Hashtbl.find tbl metric_name
    with Not_found ->
      let x = ref 0., ref 0 in
      Hashtbl.add tbl metric_name x;
      x
  in
  sum := !sum +. float metric_count *. metric_mean;
  count := !count + metric_count

(*
   Get metric stats out of the table and sort them
   by decreasing mean.
*)
let get_stats_from_agg_tbl tbl =
  let l =
    Hashtbl.fold (fun name (sum, count) l ->
      { metric_name = name;
        metric_count = !count;
        metric_mean = !sum /. float !count } :: l
    ) tbl []
  in
  List.sort (fun a b -> compare b.metric_mean a.metric_mean) l

(*
   Aggregate stats collected over the last max_age seconds
   and sort them by decreasing mean.
*)
let get_recent max_age =
  let some_time_ago =
    Util_time.of_float (Unix.time () -. max_age)
  in
  let stream = Lat_access.Latency_stat.to_stream ~min_ord:some_time_ago () in
  let agg_tbl = Hashtbl.create 100 in
  Lwt_stream.iter (fun (k, {items}, ord) ->
    List.iter (add_to_agg_tbl agg_tbl) items
  ) stream >>= fun () ->
  return (get_stats_from_agg_tbl agg_tbl)
