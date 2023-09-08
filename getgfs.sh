#!/bin/ksh
                                                                                      
set -x

mkdir /ptmp/wx20mg/temp
cd /ptmp/wx20mg/temp

ymd=20121218
YEAR=`echo $ymd | cut -c1-4`
MONTH=`echo $ymd | cut -c1-6`
DAY=`echo $ymd | cut -c1-8`

cycles="00 06 12 18"
for cyc in $cycles; do
hpsstar get /NCEPPROD/hpssprod/runhistory/1year/rh${YEAR}/$MONTH/$ymd/com_gfs_prod_gfs.${ymd}${cyc}.pgrb.tar $(hpsstar inx /hpssprod/runhistory/1year/rh${YEAR}/$MONTH/$ymd/com_gfs_prod_gfs.${ymd}${cyc}.pgrb.tar|grep 'pgrb')

done
mv gfs* ../gfs.$ymd
exit
