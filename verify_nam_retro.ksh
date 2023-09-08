#!/bin/ksh
#@ output = /meso/save/wx20mg/monthly_mean/retronammean.out
#@ error = /meso/save/wx20mg/monthly_mean/retronammean.err
#@ job_type = parallel
#@ class = devhigh
#@ group = devonprod
#@ total_tasks = 1
#@ resources = ConsumableCpus(1) ConsumableMemory(2 GB)
#@ account_no=NAM-T2O
#@ wall_clock_limit = 00:49:00
#@ network.MPI = sn_all,shared,us
#@ queue
#

mkdir /stmp/wx20mg/namdaily
rm /stmp/wx20mg/namdaily/*
cd /stmp/wx20mg/namdaily

ymdv=20130401
cycles="00 06 12 18"
for cyc in $cycles; do
 
echo 'DATE  '${ymdv}${cyc}'00F00' > VERDATE
validtime=`cut -c 7-16 VERDATE`
hours="00 06 12 18 24 30 36 42 48 54 60 66 72 78 84"
set -x
for fhr in $hours;do

cycle="`/meso/save/wx20mg/board/advtime ${validtime} -$fhr -1`"
ymd=`echo $cycle | cut -c1-8`
cyc=`echo $cycle | cut -c9-10`
ymdv=`echo $validtime | cut -c1-8`
cycv=`echo $validtime | cut -c9-10`
COMOUT=/meso/noscrub/wx20mg/monthly/nam
COMIN=/com/nam/prod/nam.${ymd}
COMIN2=/com/gfs/prod/gdas.${ymdv}
#COMIN=/ptmp/wx20mg/temp/nam.${ymd}
#COMIN2=/ptmp/wx20mg/temp2/${ymdv}
EXECutil=/nwprod/util/exec
EXEC=/meso/save/wx20mg/monthly_mean/exec
mkdir $COMOUT

# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
export tmmark=tm00
export MP_SHARED_MEMORY=yes

cp ${COMIN}/nam.t${cyc}z.awip32${fhr}.tm00 NAMFCST${fhr}
/nwprod/util/exec/grbindex NAMFCST${fhr} NAMFCST${fhr}I

cp ${COMIN2}/gdas1.t${cycv}z.pgrbf00 NAMANL1
$EXECutil/copygb -g 221 -x NAMANL1 NAMANL
/nwprod/util/exec/grbindex NAMANL NAMANLI 

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
