module Latency_stat = Mysql_access_kv.Make (struct
  let tblname = "latency_stat"
  module Key = Uuid
  module Value = struct
    type t = Lat_t.metric_list
    let of_string x = Lat_j.metric_list_of_string x
    let to_string x = Lat_j.string_of_metric_list x
  end
  module Ord = Util_time
  let create_ord = Mysql_types.KV.created
  let update_ord = None
end)
