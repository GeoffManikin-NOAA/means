#!/bin/ksh
#@ output = /meso/save/wx20mg/monthly_mean/retrogfsmean.out
#@ error = /meso/save/wx20mg/monthly_mean/retrogfsmean.err
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

mkdir /stmp/wx20mg/gfsdaily
rm /stmp/wx20mg/gfsdaily/*
cd /stmp/wx20mg/gfsdaily

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
COMOUT=/meso/noscrub/wx20mg/monthly/gfs
COMIN=/com/gfs/prod/gfs.${ymd}
COMIN2=/com/gfs/prod/gdas.${ymdv}
#COMIN=/ptmp/wx20mg/gfs.${ymd}
#COMIN2=/ptmp/wx20mg/temp2/gdas.${ymdv}
EXECutil=/nwprod/util/exec
EXEC=/meso/save/wx20mg/monthly_mean/exec
mkdir $COMOUT

# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
export tmmark=tm00
export MP_SHARED_MEMORY=yes

cp ${COMIN}/gfs.t${cyc}z.pgrbf${fhr} GFSFCST1
$EXECutil/copygb -g 221 -x GFSFCST1 GFSFCST${fhr}
/nwprod/util/exec/grbindex GFSFCST${fhr} GFSFCST${fhr}I

cp ${COMIN2}/gdas1.t${cycv}z.pgrbf00 GFSANL1
$EXECutil/copygb -g 221 -x GFSANL1 GFSANL
/nwprod/util/exec/grbindex GFSANL GFSANLI 

    export XLFUNIT_11="GFSFCST${fhr}"
    export XLFUNIT_12="GFSFCST${fhr}I"
    export XLFUNIT_13="GFSANL"
    export XLFUNIT_14="GFSANLI"
    export XLFUNIT_68="STATSFIL${fhr}"
    $EXEC/gfs_daily_stats <<EOF >> gfsmean.out${fhr}
$fhr
EOF

cp STATSFIL${fhr} $COMOUT/gfsmean_${validtime}V${fhr}
done
done
exit
