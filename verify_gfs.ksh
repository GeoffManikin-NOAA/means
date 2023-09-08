#!/bin/ksh --login
#
#BSUB -oo /meso/save/Geoffrey.Manikin/monthly_mean/verify_gfs8.out
#BSUB -eo /meso/save/Geoffrey.Manikin/monthly_mean/verify_gfs8.err
#BSUB -J verify_gfs
#BSUB -n 1
#BSUB -W 00:50
#BSUB -q "dev"
#BSUB -P GFS-T2O
#BSUB -x

set -x

mkdir /stmpp1/Geoffrey.Manikin/gfsdaily8
rm /stmpp1/Geoffrey.Manikin/gfsdaily8/*
cd /stmpp1/Geoffrey.Manikin/gfsdaily8

# Set up some constants
export tmmark=tm00
export MP_SHARED_MEMORY=yes

cycles="00 06 12 18"
for cyc in $cycles; do
 
cp /com/date/t18z VERDATE
vdate=`cut -c 7-14 VERDATE`
#vdate=20190123
validtime=$vdate$cyc

ymdv=`echo $validtime | cut -c1-8`
cycv=`echo $validtime | cut -c9-10`
COMIN2=/gpfs/hps/nco/ops/com/gfs/prod/gfs.${ymdv}
EXECutil=/nwprod/util/exec
cp ${COMIN2}/gfs.t${cycv}z.pgrb2.0p25.f000 .
$EXECutil/cnvgrib -g21 gfs.t${cycv}z.pgrb2.0p25.f000 GFSANL1
$EXECutil/copygb -g 221 -x GFSANL1 GFSANL
/nwprod/util/exec/grbindex GFSANL GFSANLI

hours="00 06 12 18 24 30 36 42 48 54 60 66 72 78 84"
hours="00 06 12 18 24 30 36 42 48 60 72 84 96 120 144 168 192 216 240"
#hours="00 06 12 18 24 30 36 42 48 54 60 66 72 78 84 90"
set -x
for fhr in $hours;do

typeset -Z3 fhr
fhr2=$fhr
typeset -Z2 fhr2
cycle="`/meso/save/Geoffrey.Manikin/monthly_mean/advtime ${validtime} -$fhr -1`"
ymd=`echo $cycle | cut -c1-8`
cyc=`echo $cycle | cut -c9-10`
COMOUT=/meso/noscrub/Geoffrey.Manikin/monthly/gfs
COMIN=/gpfs/hps/nco/ops/com/gfs/prod/gfs.${ymd}
EXEC=/meso/save/Geoffrey.Manikin/monthly_mean/exec
mkdir $COMOUT

cp ${COMIN}/gfs.t${cyc}z.pgrb2.0p25.f${fhr} . 
$EXECutil/cnvgrib -g21 gfs.t${cyc}z.pgrb2.0p25.f${fhr} GFSFCST1
$EXECutil/copygb -g 221 -x GFSFCST1 GFSFCST${fhr}
/nwprod/util/exec/grbindex GFSFCST${fhr} GFSFCST${fhr}I

    export XLFUNIT_11="GFSFCST${fhr}"
    export XLFUNIT_12="GFSFCST${fhr}I"
    export XLFUNIT_13="GFSANL"
    export XLFUNIT_14="GFSANLI"
    export XLFUNIT_68="STATSFIL${fhr}"
    $EXEC/gfs_daily_stats <<EOF >> gfsmean.out${fhr}
$fhr
EOF

if [ $fhr -lt 100 ];
then
cp STATSFIL${fhr} $COMOUT/gfsmean_${validtime}V${fhr2}
else
cp STATSFIL${fhr} $COMOUT/gfsmean_${validtime}V${fhr}
fi
done
done
exit
