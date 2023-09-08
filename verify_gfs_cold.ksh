#!/bin/ksh --login
#
#BSUB -oo /meso/save/Geoffrey.Manikin/monthly_mean/verify_gfscold.out4
#BSUB -eo /meso/save/Geoffrey.Manikin/monthly_mean/verify_gfscold.err4
#BSUB -J verify_gfs
#BSUB -n 1
#BSUB -W 00:50
#BSUB -q "dev"
#BSUB -P GFS-T2O
#BSUB -x

set -x

mkdir /stmpp1/Geoffrey.Manikin/gfscold4
rm /stmpp1/Geoffrey.Manikin/gfscold4/*
cd /stmpp1/Geoffrey.Manikin/gfscold4

# Set up some constants
export tmmark=tm00
export MP_SHARED_MEMORY=yes

cp /com/date/t00z VERDATE
ymd=`cut -c7-14 VERDATE`
ymd=20190929

cycles="00 06 12 18"
cycles="00 12"
hours="120 132 240 252 360 372"
for cyc in $cycles; do
for fhr in $hours; do
 
typeset -Z3 fhr
COMOUT=/meso/noscrub/Geoffrey.Manikin/monthly/gfs
COMIN1=/gpfs/dell1/nco/ops/com/gfs/prod/gfs.${ymd}/${cyc}
COMIN2=/gpfs/hps/nco/ops/com/gfs/para/gfs.${ymd}
EXEC=/meso/save/Geoffrey.Manikin/monthly_mean/exec
EXECutil=/nwprod2/util/exec
mkdir $COMOUT

cp ${COMIN1}/gfs.t${cyc}z.pgrb2.0p25.f${fhr} . 
$EXECutil/cnvgrib -g21 gfs.t${cyc}z.pgrb2.0p25.f${fhr} GFSFCST1
$EXECutil/copygb -g 221 -x GFSFCST1 GFSFCST1${fhr}
/nwprod/util/exec/grbindex GFSFCST1${fhr} GFSFCST1${fhr}I

cp ${COMIN2}/gfs.t${cyc}z.pgrb2.0p25.f${fhr} . 
$EXECutil/cnvgrib -g21 gfs.t${cyc}z.pgrb2.0p25.f${fhr} GFSFCST2
$EXECutil/copygb -g 221 -x GFSFCST2 GFSFCST2${fhr}
/nwprod/util/exec/grbindex GFSFCST2${fhr} GFSFCST2${fhr}I

    export XLFUNIT_11="GFSFCST1${fhr}"
    export XLFUNIT_12="GFSFCST1${fhr}I"
    export XLFUNIT_13="GFSFCST2${fhr}"
    export XLFUNIT_14="GFSFCST2${fhr}I"
    export XLFUNIT_68="STATSFIL${fhr}"
    $EXEC/gfs_cold_check <<EOF >> cold_${ymd}${cyc}f${fhr}
$fhr
EOF

done
done
mv cold* /meso/noscrub/Geoffrey.Manikin/cold/.
exit

#if [ $fhr -lt 100 ];
#then
#cp STATSFIL${fhr} $COMOUT/gfsmean_${validtime}V${fhr2}
#else
#cp STATSFIL${fhr} $COMOUT/gfsmean_${validtime}V${fhr}
#fi
#exit
