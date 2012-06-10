#!/bin/sh

EXPECTED_ARGS=1
E_BADARGS=65

if [ $# -ne $EXPECTED_ARGS ]
then
  echo "Usage: `basename $0` {node_name}"
  exit $E_BADARGS
fi

cd `dirname $0`
exec erl -pa $PWD/ebin -boot start_sasl -s dist_gen_server -name $1 -kernel inet_dist_listen_min 21500 inet_dist_listen_max 21999
