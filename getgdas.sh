#!/bin/ksh
                                                                                      
set -x

mkdir /ptmp/wx20mg/temp2
cd /ptmp/wx20mg/temp2

ymd=20121106
YEAR=`echo $ymd | cut -c1-4`
MONTH=`echo $ymd | cut -c1-6`
DAY=`echo $ymd | cut -c1-8`

cycles="00 06 12 18"
for cyc in $cycles;do

hpsstar get /NCEPPROD/hpssprod/runhistory/rh${YEAR}/$MONTH/$DAY/com_gfs_prod_gdas.${ymd}${cyc}.tar $(hpsstar inx /hpssprod/runhistory/rh${YEAR}/$MONTH/$DAY/com_gfs_prod_gdas.${ymd}${cyc}.tar|grep 'pgrb')
done

mkdir gdas.$ymd
mv *pgrb* gdas.$ymd/.
exit
