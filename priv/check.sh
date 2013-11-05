MONITORING_SOCKET=/tmp/monitoring.sock

test -S $MONITORING_SOCKET && echo -n $@ | nc -U $MONITORING_SOCKET || echo 0
