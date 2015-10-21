(*
   Produce daily performance report and send it to the developers.
*)

open Printf
open Lwt
open Lat_t

let make_html_report stats =
  let subject = "API latency over the last 24 hours" in
  let buf = Buffer.create 1000 in
  bprintf buf "\
<table>
<tr>
  <td><b>Operation</b></td>
  <td><b>Latency (mean, seconds)</b></td>
  <td><b>Count</b></td></tr>
";
  List.iter (fun x ->
    bprintf buf "
<tr>
  <td>%s</td>
  <td>%.3f</td>
  <td>%i</td>
</tr>
"
      (Util_html.encode x.metric_name)
      x.metric_mean
      x.metric_count
  ) stats;
  bprintf buf "
</tr>
</table>
";
  let body = Buffer.contents buf in
  subject, body

let make_text_report stats =
  let buf = Buffer.create 1000 in
  bprintf buf "%-45s%-25s%s\n"
    "Operation"
    "Latency (mean, seconds)"
    "Count";
  List.iter (fun x ->
    bprintf buf "%-45s%-25.3f%i\n"
      x.metric_name
      x.metric_mean
      x.metric_count
  ) stats;
  Buffer.contents buf

let get_html_report () =
  Lat_acc.get_recent 86400. >>= fun stats ->
  return (make_html_report stats)

let get_text_report () =
  Lat_acc.get_recent 86400. >>= fun stats ->
  return (make_text_report stats)

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
