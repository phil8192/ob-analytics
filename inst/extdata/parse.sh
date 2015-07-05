#!/bin/bash
# parse bitstamp event data into expected format.
# example input:
#  1430524794466 order_changed {"price": "231.00", "amount": "306.59220434", "datetime": "1430524794", "id": 65714203, "order_type": 0}
#  1430524794467 order_deleted {"price": "231.00", "amount": "0.00000000", "datetime": "1430524009", "id": 65712957, "order_type": 1}
#  1430524794518 order_created {"price": "233.00", "amount": "6.84493851", "datetime": "1430524794", "id": 65714204, "order_type": 1}
#  1430524797386 order_created {"price": "231.02", "amount": "0.95228327", "datetime": "1430524797", "id": 65714205, "order_type": 0}
#  1430524799111 order_deleted {"price": "230.37", "amount": "5.18983490", "datetime": "1430524780", "id": 65714193, "order_type": 0}
#  1430524799389 order_created {"price": "230.36", "amount": "5.19006019", "datetime": "1430524799", "id": 65714206, "order_type": 0}
# example output:
#  1430524794466,0,23100,30659220434,1430524794000,65714203,0
#  1430524794467,0,23100,0,1430524009000,65712957,1
#  1430524794518,0,23300,684493851,1430524794000,65714204,1
#  1430524797386,0,23102,95228327,1430524797000,65714205,0
#  1430524799111,0,23037,518983490,1430524780000,65714193,0
#  1430524799389,0,23036,519006018,1430524799000,65714206,0
# and then, rows ending with 0 == go into bids.csv, 1 == asks.csv.
# the format for bids (asks) .csv is:
#
# local_timestamp, event_type, price, volume, exchange_timestamp, order_id
#     where local_timestamp    = time (in milliseconds) when event first received (locally)
#           event_type         = 0: created, 1: modified, 2: deleted
#           price              = price level of order event (in lowest denomination) e.g., $321.55 = 32155 cents.
#           volume             = remaining volume of order (in lowest denomination) e.g., 0.05 Bitcoin = 5000000 Satoshi.
#           exchange_timestamp = time (in milliseconds) when order first received at exchange.
#           order_id           = limit order unique identifier.
bzcat 2015-05-01.log.bz2 \
  |grep -v order_book \
  |grep -v trade \
  |sed 's/,//g; s/\"//g; s/{//g; s/}//g' \
  |awk '{printf "%d,%d,%d,%d,%d000,%d,%d\n", $1, $2, ($4*10^2), ($6*10^8), $8, $10, $12}' \
  |sed 's/ //g' |sed 's/order_created/0/; s/order_changed/1/; s/order_deleted/2/' >orders.csv
cat orders.csv |grep ",0$" |sed 's/,0$//' >bids.csv
cat orders.csv |grep ",1$" |sed 's/,1$//' >asks.csv
rm -f orders.csv

