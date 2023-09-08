       PROGRAM MAKEWEEKLY 
C                                                  .
C SUBPROGRAM:  MAKEWEEKLY 
C   PRGMMR: MANIKIN        ORG: W/NP22     DATE:  05-07-12
C
C ABSTRACT:  READS IN ANALYSIS AND ERROR DATA AND THEN WRITES
C             OUT MEAN FIELDS
C
C PROGRAM HISTORY LOG:
C   08-20-11  GEOFF MANIKIN
C
C REMARKS:

C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  CRAY C-90
C$$$
      INCLUDE "parmg221"
      PARAMETER(ITOT=ILIM*JLIM,NDAYS=31)
      DIMENSION GRID(ITOT)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER LEVS(MAXLEV),IVAR(5)
      LOGICAL*1 MASK(ITOT), MASK2(ITOT), MASK3(ITOT)
      LOGICAL NEEDP
C
      PARAMETER(MBUF=2000000,JF=1000000)
      CHARACTER CBUF(MBUF)
      CHARACTER CBUF2(MBUF)
      CHARACTER*11 ENVVAR
      CHARACTER*80 FNAME
      LOGICAL*1 LB(JF)
      REAL F(JF)
      PARAMETER(MSK1=32000,MSK2=4000)
      INTEGER JENS(200),KENS(200)
      INTEGER FHR
      INTEGER IGDNUM, YEAR, MON, DAY, CYC
      DIMENSION Z500F(ITOT,NDAYS),Z500A(ITOT,NDAYS),
     &  Z500E(ITOT,NDAYS),Z500ASUM(ITOT),Z500ESUM(ITOT),
     &  Z500MEANA(ITOT),Z500MEANF(ITOT),Z500MEANERR(ITOT)
      DIMENSION SLPF(ITOT,NDAYS),SLPA(ITOT,NDAYS),
     &  SLPE(ITOT,NDAYS),SLPASUM(ITOT),SLPESUM(ITOT),
     &  SLPMEANA(ITOT),SLPMEANF(ITOT),SLPMEANERR(ITOT)
      DIMENSION T850F(ITOT,NDAYS),T850A(ITOT,NDAYS),
     &  T850E(ITOT,NDAYS),T850ASUM(ITOT),T850ESUM(ITOT),
     &  T850MEANA(ITOT),T850MEANF(ITOT),T850MEANERR(ITOT)
      DIMENSION T700F(ITOT,NDAYS),T700A(ITOT,NDAYS),
     &  T700E(ITOT,NDAYS),T700ASUM(ITOT),T700ESUM(ITOT),
     &  T700MEANA(ITOT),T700MEANF(ITOT),T700MEANERR(ITOT)
      DIMENSION T2F(ITOT,NDAYS),T2A(ITOT,NDAYS),
     &  T2E(ITOT,NDAYS),T2ASUM(ITOT),T2ESUM(ITOT),
     &  T2MEANA(ITOT),T2MEANF(ITOT),T2MEANERR(ITOT)
      DIMENSION SP250F(ITOT,NDAYS),SP250A(ITOT,NDAYS),
     &  SP250E(ITOT,NDAYS),SP250ASUM(ITOT),SP250ESUM(ITOT),
     &  SP250MEANA(ITOT),SP250MEANF(ITOT),SP250MEANERR(ITOT)
      DIMENSION SP850F(ITOT,NDAYS),SP850A(ITOT,NDAYS),
     &  SP850E(ITOT,NDAYS),SP850ASUM(ITOT),SP850ESUM(ITOT),
     &  SP850MEANA(ITOT),SP850MEANF(ITOT),SP850MEANERR(ITOT)
      DIMENSION SP10F(ITOT,NDAYS),SP10A(ITOT,NDAYS),
     &  SP10E(ITOT,NDAYS),SP10ASUM(ITOT),SP10ESUM(ITOT),
     &  SP10MEANA(ITOT),SP10MEANF(ITOT),SP10MEANERR(ITOT)

      LUGBF=78
      LUGB=11
      LUGI=12

C READ THE PRIMARY FORECAST HOUR
      READ (5,*) FHR
      print *, FHR

      DO LL=1,NDAYS
C  READ IN THE PRIMARY FORECAST FILE
      JJ1 = 1
      JJINC = 1
      ISTAT = 0
C
C  READ 1ST INDEX FILE TO GET GRID SPECS
C
      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGB
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGB,FNAME,IRETGB)
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGI
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGI,FNAME,IRETGI)
      CALL GETGI(LUGI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
      write(6,*)' IRET FROM GETGI ',IRGI
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING 1ST GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
        STOP
      ENDIF
c      REWIND LUGI

C
      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        write(6,*)' IRET FROM GETGB1S ',IRGS
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          ISTAT = IRGS
          STOP
        ENDIF
C
      ENDDO

C    GET GRID NUMBER & DATE INFO FROM PDS
C
      IGDNUM = KPDS(3)
      YEAR = KPDS(8)
      MON = KPDS(9)
      DAY = KPDS(10)
      CYC = KPDS(11)

      print *, YEAR, MON, DAY, CYC
C
C   PROCESS THE PRIMARY GRIB FILE
C
      IMAX = KGDS(2)
      JMAX = KGDS(3)
      NUMVAL = IMAX*JMAX
      KMAX = MAXLEV
  285 FORMAT(' IV, IVAR, L, IRET:  ',4I5)

C -== GET ANALYSIS FIELDS  ==-

C   SEA LEVEL PRESSURE
       L = 0
       IV= 0
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 002
       JPDS(6) = 1 
       JPDS(13) = 1
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED ANL SLP FOR FILE ', LL
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           SLPA(KK,LL) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK ANL SLP FOR FILE ', LL 
          ISTAT = IRET
         STOP
       ENDIF  

C   2-M TEMP 
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 105 
       JPDS(7) = 2
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED ANL 2-M TEMP FOR FILE ', LL 
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           T2F(KK,LL) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK ANL 2-M TEMP FOR FILE ', LL
          ISTAT = IRET
         STOP
       ENDIF

C   10-M SPEED 
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 32 
       JPDS(6) = 105
       JPDS(7) = 10 
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED ANL 10-M WSPD FOR FILE ', LL
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           SP10A(KK,LL) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK ANL 10-M WSP FOR FILE ', LL
          ISTAT = IRET
         STOP
       ENDIF

c   850 mb temperature
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 011
      JPDS(6) = 100
      JPDS(7) = 850
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED ANL T850 FOR FILE ', LL
        DO KK = 1, ITOT
          T850A(KK,LL) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK ANL 850 T FOR FILE ', LL
       ISTAT = IRET
       STOP
      ENDIF

c   700 mb temperature
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 011
      JPDS(6) = 100
      JPDS(7) = 700
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED ANL T700 FOR FILE ', LL
        DO KK = 1, ITOT
          T700A(KK,LL) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK ANL 700 T FOR FILE ', LL
       ISTAT = IRET
       STOP
      ENDIF

c   500 mb height 
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 007
      JPDS(6) = 100
      JPDS(7) = 500
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED ANL Z500 FOR FILE ', LL 
        DO KK = 1, ITOT
          Z500A(KK,LL) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK ANL 500 Z'
       ISTAT = IRET
       STOP
      ENDIF

c   250 mb Wind Speed 
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 032
      JPDS(6) = 100
      JPDS(7) = 250
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED ANL 250 WSP FOR FILE ', LL
        DO KK = 1, ITOT
          SP250A(KK,LL) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK ANL 250 WSP'
       ISTAT = IRET
       STOP
      ENDIF


c   850 mb Wind Speed 
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 032
      JPDS(6) = 100
      JPDS(7) = 850
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED ANL 850 WSP FOR FILE ', LL 
        DO KK = 1, ITOT
          SP850A(KK,LL) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK FCST 850 WSP'
       ISTAT = IRET
c       STOP
      ENDIF

C   ERRORS SECTION 
C   SEA LEVEL PRESSURE 
       L = 0
       IV= 0
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 174 
       JPDS(6) = 1
       JPDS(13) = 1
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED SEA LEVEL PRESSURE ERROR'
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           SLPE(KK,LL) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK SLP ERROR'
          ISTAT = IRET
         STOP
       ENDIF

C   2-M TEMP
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 158
       JPDS(6) = 105
       JPDS(7) = 2
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED 2-M TEMP ERROR'
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           T2E(KK,LL) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK 2-M TEMP ERROR'
          ISTAT = IRET
         STOP
       ENDIF

C   500 HEIGHT
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 027 
       JPDS(6) = 100
       JPDS(7) = 500
       print *, 'anl check ',LUGB, LUGI, NUMVAL
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED 500 MB HEIGHT ERROR'
         DO KK = 1, ITOT
           Z500E(KK,LL) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 500 HEIGHT ERROR ', IRET
         ISTAT = IRET
         STOP
       ENDIF

C   850 TEMP 
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 158 
       JPDS(6) = 100
       JPDS(7) = 850
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED 850 MB TEMP ERROR'
         DO KK = 1, ITOT
           T850E(KK,LL) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 850 TEMP ERROR ', IRET
         ISTAT = IRET
         STOP
       ENDIF

C   700 TEMP
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 158 
       JPDS(6) = 100
       JPDS(7) = 700
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED 700 MB TEMP ERROR'
         DO KK = 1, ITOT
           T700E(KK,LL) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 700 TEMP ERROR ', IRET
         ISTAT = IRET
         STOP
       ENDIF

c   250 mb WIND SPEED 
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 161
      JPDS(6) = 100
      JPDS(7) = 250
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED 250 SPEED ERROR'
        DO KK = 1, ITOT
          SP250E(KK,LL) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK 250 SPEED ERROR ', IRET
       ISTAT = IRET
      ENDIF

c   850 mb WIND SPEED
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 161 
      JPDS(6) = 100
      JPDS(7) = 850
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED 850 SPEED ERROR'
        DO KK = 1, ITOT
          SP850E(KK,LL) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK 850 SPEED ERROR ', IRET
       ISTAT = IRET
      ENDIF

c   10 mb WIND SPEED 
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 161 
      JPDS(6) = 105
      JPDS(7) = 10
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED 10 SPEED ERROR'
        DO KK = 1, ITOT
          SP10E(KK,LL) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK 10 SPEED ERROR ', IRET
       ISTAT = IRET
      ENDIF

      LUGB=LUGB+2
      LUGI=LUGI+2
      ENDDO   !  END OF NDAYS LOOP

C  COMPUTE MEANS 
       
       DO K = 1, ITOT
        Z500ASUM(K)=0.
        Z500ESUM(K)=0.
        SLPASUM(K)=0.
        SLPESUM(K)=0.
        T850ASUM(K)=0.
        T850ESUM(K)=0.
        DO MM=1,NDAYS
         Z500ASUM(K)=Z500ASUM(K) + Z500A(K,MM)
         Z500ESUM(K)=Z500ESUM(K) + Z500E(K,MM)
         SLPASUM(K)=SLPASUM(K) + SLPA(K,MM)
         SLPESUM(K)=SLPESUM(K) + SLPE(K,MM)
         T850ASUM(K)=T850ASUM(K) + T850A(K,MM)
         T850ESUM(K)=T850ESUM(K) + T850E(K,MM)
        ENDDO
        Z500MEANA(K)=Z500ASUM(K)/NDAYS
        Z500MEANERR(K)=Z500ESUM(K)/NDAYS
        SLPMEANA(K)=SLPASUM(K)/NDAYS
        SLPMEANERR(K)=SLPESUM(K)/NDAYS
        T850MEANA(K)=T850ASUM(K)/NDAYS
        T850MEANERR(K)=T850ESUM(K)/NDAYS
       ENDDO

C PACK UP THE MEANS 

      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGBF
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGBF,FNAME,IRET)
      print *,'IRET from BAOPEN on LUGBF = ', IRET

       KPDS(5)=007
       KPDS(6)=100
       KPDS(7)=500
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGBF,ITOT,KPDS,KGDS,MASK,Z500MEANA,IRET)
       print *,'IRET from Z500MEANA PUTGB on LUGBF = ', IRET

       KPDS(5)=002
       KPDS(6)=001
       KPDS(7)=0
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGBF,ITOT,KPDS,KGDS,MASK,SLPMEANA,IRET)
       print *,'IRET from SLPMEANA PUTGB on LUGBF = ', IRET

       KPDS(5)=011
       KPDS(6)=100
       KPDS(7)=850
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGBF,ITOT,KPDS,KGDS,MASK,T850MEANA,IRET)
       print *,'IRET from T850MEANA PUTGB on LUGBF = ', IRET

       KPDS(5)=027
       KPDS(6)=100
       KPDS(7)=500
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGBF,ITOT,KPDS,KGDS,MASK,Z500MEANERR,IRET)
       print *,'IRET from Z500MEANERR PUTGB on LUGBF = ', IRET

       KPDS(5)=026
       KPDS(6)=1
       KPDS(7)=0
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGBF,ITOT,KPDS,KGDS,MASK,SLPMEANERR,IRET)
       print *,'IRET from SLPMEANERR PUTGB on LUGBF = ', IRET

       KPDS(5)=158
       KPDS(6)=100
       KPDS(7)=850
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGBF,ITOT,KPDS,KGDS,MASK,T850MEANERR,IRET)
       print *,'IRET from T850MEANERR PUTGB on LUGBF = ', IRET

      CALL BACLOSE(LUGBF,IRET)
      STOP
      END 
