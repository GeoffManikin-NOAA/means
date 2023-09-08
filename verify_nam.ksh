#!/bin/ksh --login
#
#BSUB -oo /meso/save/Geoffrey.Manikin/monthly_mean/verify_nam.out2
#BSUB -eo /meso/save/Geoffrey.Manikin/monthly_mean/verify_nam.err2
#BSUB -J verify_nam 
#BSUB -n 1
#BSUB -W 00:50
#BSUB -q "dev"
#BSUB -P NAM-T2O
#BSUB -x
#

set -x

mkdir /stmpp1/Geoffrey.Manikin/namdaily2
rm /stmpp1/Geoffrey.Manikin/namdaily2/*
cd /stmpp1/Geoffrey.Manikin/namdaily2

cycles="00 06 12 18"
for cyc in $cycles; do
 
cp /com/date/t18z VERDATE
vdate=`cut -c 7-14 VERDATE`
#vdate=20190123
validtime=$vdate$cyc
hours="00 06 12 18 24 30 36 42 48 54 60 66 72 78 84"
set -x

ymdv=`echo $validtime | cut -c1-8`
cycv=`echo $validtime | cut -c9-10`
COMIN2=/gpfs/hps/nco/ops/com/gfs/prod/gfs.${ymdv}
EXECutil=/nwprod/util/exec
cp ${COMIN2}/gfs.t${cycv}z.pgrb2.0p25.f000 . 
$EXECutil/cnvgrib -g21 gfs.t${cycv}z.pgrb2.0p25.f000 NAMANL1
$EXECutil/copygb -g 221 -x NAMANL1 NAMANL
/nwprod/util/exec/grbindex NAMANL NAMANLI

for fhr in $hours;do

cycle="`/meso/save/Geoffrey.Manikin/monthly_mean/advtime ${validtime} -$fhr -1`"
ymd=`echo $cycle | cut -c1-8`
cyc=`echo $cycle | cut -c9-10`
COMOUT=/meso/noscrub/Geoffrey.Manikin/monthly/nam
COMIN=/com2/nam/prod/nam.${ymd}
#COMIN=/ptmp/Geoffrey.Manikin/temp/${ymd}
EXECutil=/nwprod/util/exec
EXEC=/meso/save/Geoffrey.Manikin/monthly_mean/exec
mkdir $COMOUT

# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
export tmmark=tm00
export MP_SHARED_MEMORY=yes

cp ${COMIN}/nam.t${cyc}z.awip32${fhr}.tm00.grib2 NAMFCST${fhr}.grib2
/nwprod/util/exec/cnvgrib -g21 NAMFCST${fhr}.grib2 NAMFCST${fhr}
/nwprod/util/exec/grbindex NAMFCST${fhr} NAMFCST${fhr}I

    export XLFUNIT_11="NAMFCST${fhr}"
    export XLFUNIT_12="NAMFCST${fhr}I"
    export XLFUNIT_13="NAMANL"
    export XLFUNIT_14="NAMANLI"
    export XLFUNIT_68="STATSFIL${fhr}"
    $EXEC/nam_daily_stats <<EOF >> nammean.out${fhr}
$fhr
EOF

cp STATSFIL${fhr} $COMOUT/nammean_${validtime}V${fhr}
done
done
exit
