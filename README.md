erl_mon
=======

Integration between erlang and zabbix.

Require netcat for work.

1. Setup your monitoring socket path in priv/check.sh
2. Setup your monitoring socket path in application vaiable *monitoring_socket*

Note: default path is /tmp/monitoring.sock


Default setup contains only one item:

  # ./priv/check.sh running
  # 1

All error / unvailable items will return 0:

  # ./priv/check.sh error_req
  # 0


API
---

Currently usable only erl_mon:inc/1 and erl_mon:inc/2