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
#  65714203,1430524794466,1430524794000,231.00,30659220434,0,0
#  65712957,1430524794467,1430524009000,231.00,0,0,1
#  65714204,1430524794518,1430524794000,233.00,684493851,0,1
#  65714205,1430524797386,1430524797000,231.02,95228327,0,0
#  65714193,1430524799111,1430524780000,230.37,518983490,0,0
#  65714206,1430524799389,1430524799000,230.36,519006018,0,0
#
# the format for bids (asks) .csv is:
#
# id, local.ts, exchange.ts, price, volume, type, side
#     where id                 = limit order unique identifier.
#           local.ts           = time (in milliseconds) when event first received (locally).
#           exchange.ts        = time (in milliseconds) when order first received at exchange.
#           price              = price level of order event.
#           volume             = remaining volume of order (in lowest denomination) e.g., 0.05 Bitcoin = 5000000 Satoshi.
#           type               = 0: created, 1: modified, 2: deleted.
#           side               = 0: bid, 1: ask.
echo "id,local.ts,exchange.ts,price,volume,type,side" >orders.csv
bzcat 2015-05-01.log.bz2 \
  |grep -v order_book \
  |grep -v trade \
  |sed 's/,//g; s/\"//g; s/{//g; s/}//g' \
  |awk '{printf "%d,%d,%d000,%s,%d,%s,%d\n", $10, $1, $8, $4, ($6*10^8), $2, $12}' \
  |sed 's/ //g; s/order_created/0/; s/order_changed/1/; s/order_deleted/2/' >>orders.csv

