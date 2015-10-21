(*
   Produce daily performance report and send it to the developers.
*)

open Printf
open Lwt
open Lat_t

let make_html_report stats =
  let subject = "Backend performance over the last 24 hours" in
  let buf = Buffer.create 1000 in
  bprintf buf "\
<table>
<tr><th>Operation</th><th>Mean latency (s)</th><th>Count</th></tr>
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

let send_daily_report () =
  Lat_acc.cleanup () >>= fun () ->
  Lat_acc.get_recent 86400. >>= fun stats ->
  let subject, html_body = make_html_report stats in
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
