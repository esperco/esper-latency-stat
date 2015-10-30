(*
   Produce daily performance report and send it to the developers.
*)

open Printf
open Lwt
open Lat_t

let make_html_table buf title stats =
  bprintf buf "\
<h1>%s</h1>
<table>
<tr>
  <td><b>Operation</b></td>
  <td><b>Latency (mean, seconds)</b></td>
  <td><b>Count</b></td>
  <td><b>Total time (seconds)</b></td>
</tr>
"
    (Util_html.encode title);
  List.iter (fun x ->
    bprintf buf "
<tr>
  <td>%s</td>
  <td>%.3f</td>
  <td>%i</td>
  <td>%.3f</td>
</tr>
"
      (Util_html.encode x.metric_name)
      x.metric_mean
      x.metric_count
      (float x.metric_count *. x.metric_mean)
  ) stats;
  bprintf buf "
</tr>
</table>
"

let make_html_report stats_sorted_by_latency stats_sorted_by_total_time =
  let subject = "API latency over the last 24 hours" in
  let buf = Buffer.create 1000 in
  make_html_table buf "Sorted by latency" stats_sorted_by_latency;
  make_html_table buf "Sorted by total time" stats_sorted_by_total_time;
  let body = Buffer.contents buf in
  subject, body

let make_text_table buf title stats =
  bprintf buf "--- %s ---\n" title;
  bprintf buf "%-45s%-14s%-7s%s\n"
    "Operation"
    "Mean latency"
    "Count"
    "Total time";
  List.iter (fun x ->
    bprintf buf "%-45s%-14.3f%-7i%.3f\n"
      x.metric_name
      x.metric_mean
      x.metric_count
      (float x.metric_count *. x.metric_mean)
  ) stats

let make_text_report stats_sorted_by_latency stats_sorted_by_total_time =
  let buf = Buffer.create 1000 in
  make_text_table buf "Sorted by latency" stats_sorted_by_latency;
  make_text_table buf "Sorted by total time" stats_sorted_by_total_time;
  Buffer.contents buf

let get_24h_stats () =
  Lat_acc.get_recent 86400. >>= fun by_latency ->
  let by_total_time = Lat_acc.sort_by_total_time by_latency in
  return (by_latency, by_total_time)

let get_html_report () =
  get_24h_stats () >>= fun (stats, stats2) ->
  return (make_html_report stats stats2)

let get_text_report () =
  get_24h_stats () >>= fun (stats, stats2) ->
  return (make_text_report stats stats2)

let print_text_report () =
  get_text_report () >>= fun report ->
  print_string report;
  return ()

let send_daily_report () =
  Lat_acc.cleanup () >>= fun () ->
  get_html_report () >>= fun (subject, html_body) ->
  let alerts_addr =
    let conf = Conf.get () in
    Email.of_string conf.Conf_t.developer_email
  in
  Email_esper.send_from_esper
    ~from: (`External (None, alerts_addr))
    [alerts_addr]
    subject
    html_body

let main ~offset =
  Cmdline.parse_options ~offset [];
  if Esper_config.is_prod () then
    Util_lwt_main.run (send_daily_report ())
