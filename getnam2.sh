#!/bin/ksh
                                                                                      
set -x

mkdir /ptmp/wx20mg/temp2
cd /ptmp/wx20mg/temp2

ymdh=2012110918
ymd=`echo $ymdh | cut -c1-8`
YEAR=`echo $ymdh | cut -c1-4`
MONTH=`echo $ymdh | cut -c1-6`
DAY=`echo $ymdh | cut -c1-8`

hpsstar get /NCEPPROD/hpssprod/runhistory/rh${YEAR}/$MONTH/$DAY/com_nam_prod_nam.${ymdh}.awip32.tar $(hpsstar inx /hpssprod/runhistory/rh${YEAR}/$MONTH/$DAY/com_nam_prod_nam.${ymdh}.awip32.tar|grep 'awip32')

exit
