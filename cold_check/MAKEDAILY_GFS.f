      PROGRAM MAKEDAILY_GFS
C                .      .    .                                       .
C SUBPROGRAM:   MAKEDAILY_GFS 
C   PRGMMR: MANIKIN        ORG: W/NP22     DATE:  08-24-14
C
C ABSTRACT: COMPUTES FORECAST ERRORS AND THEN PACKS THEM UP ALONG WITH
C               THE ANALYSIS AND FORECAST GRIDS 
C
C PROGRAM HISTORY LOG:
C   08-24-14  GEOFF MANIKIN
C
C REMARKS:

C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  CRAY C-90
C$$$
      INCLUDE "parmg221"
      PARAMETER(ITOT=ILIM*JLIM)
      DIMENSION GRID(ITOT),DIFF(5)
      DIMENSION INCDAT(8),JNCDAT(8)
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
      INTEGER FHR,FHRPCP,FHR3,FHRX
      INTEGER IGDNUM, YEAR, MON, DAY, CYC
      DIMENSION Z5001(ITOT),Z5002(ITOT),
     &     T8501(ITOT),T8502(ITOT),T21(ITOT),T22(ITOT),
     &     Z10001(ITOT),Z10002(ITOT)
      INTEGER T2OPS, T2OLD, T850OPS, T850OLD

C READ THE PRIMARY FORECAST HOUR
      READ (5,*) FHR
      print *, FHR
      NUMLEV=MAXLEV

      print *, 'WILL READ 2 GRIB FILES'
      LUGB=11
      LUGI=12
      LUGB2=13
      LUGI2=14
      LUGB4=68

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
      WRITE(6,280) IMAX,JMAX,NUMLEV,KMAX
      print *, 'IMAX JMAX ', IMAX, JMAX
  280 FORMAT(' IMAX,JMAX,NUMLEV,KMAX ',5I4)
  285 FORMAT(' IV, IVAR, L, IRET:  ',4I5)

C -== GET FIELDS FROM FILE 1 ==-

      T2OPS=0
      T850OPS=0
C   2-M TEMP 
       J = 0
       JPDS = -1
c       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 105 
       JPDS(7) = 2
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED OPS FCST 2-M TEMP AT F',FHR
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           T21(KK) = GRID(KK)
           IF (T21(KK) .LT. 273.15) THEN
              T2OPS=T2OPS+1
           ENDIF
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK OPS FCST 2-M TEMP at F',FHR
          ISTAT = IRET
         STOP
       ENDIF

c   850 mb temperature
      J=0
      JPDS = -1
c      JPDS(3) = IGDNUM
      JPDS(5) = 011
      JPDS(6) = 100
      JPDS(7) = 850
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED OPS FCST T850 AT F',FHR
        DO KK = 1, ITOT
          T8501(KK) = GRID(KK)
          IF (T8501(KK).LT.273.15) THEN
            T850OPS=T850OPS+1
          ENDIF
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK OPS FCST 850 T'
       ISTAT = IRET
       STOP
      ENDIF

c   500 mb height 
      J=0
      JPDS = -1
c      JPDS(3) = IGDNUM
      JPDS(5) = 007
      JPDS(6) = 100
      JPDS(7) = 500
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED FCST Z500 AT F',FHR
        DO KK = 1, ITOT
          Z5001(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK FCST 500 Z'
       ISTAT = IRET
       STOP
      ENDIF

c   1000 mb height 
      J=0
      JPDS = -1
      JPDS(5) = 007
      JPDS(6) = 100
      JPDS(7) = 1000
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED FCST Z1000 AT F',FHR
        DO KK = 1, ITOT
          Z10001(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK FCST 1000 Z'
       ISTAT = IRET
       STOP
      ENDIF


C BEGIN WORK ON 2ND FILE
       JJ1 = 1
       JJINC = 1

C  READ INDEX FILE TO GET GRID SPECS
C
      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGB2
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGB2,FNAME,IRETGB2)
      print *, 'baopen ', LUGB2, FNAME, IRETGB2
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGI2
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGI2,FNAME,IRETGI2)
      print *, 'baopen ', LUGI2, FNAME, IRETGI2
      CALL GETGI(LUGI2,KSKIP,MBUF,CBUF2,NLEN,NNUM,IRGI)
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING 2ND GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
        STOP
      ENDIF
c      REWIND LUGI2

      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF2,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        write(6,*)' IRET FROM GETGB1S ',IRGS
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON READ OF 2ND GRIB FILE SO ABORT'
          ISTAT = IRGS
          STOP
        ENDIF
      ENDDO

       T850OLD=0
       T2OLD=0
C   2-M TEMP
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 105
       JPDS(7) = 2
       CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED OLD FCST 2-M TEMP'
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           T22(KK) = GRID(KK)
           IF (T22(KK) .LT. 273.15) THEN
             T2OLD=T2OLD+1
          ENDIF
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK OLD FCST 2-M TEMP'
          ISTAT = IRET
         STOP
       ENDIF

C   500 HEIGHT
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 007 
       JPDS(6) = 100
       JPDS(7) = 500
       print *, 'old check ',LUGB2, LUGI2, NUMVAL
       CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED 500 MB HEIGHT ANALYSIS'
         DO KK = 1, ITOT
           Z5002(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK OLD FCSR 500 HEIGHT', IRET
         ISTAT = IRET
         STOP
       ENDIF

C   850 TEMP 
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 011
       JPDS(6) = 100
       JPDS(7) = 850
       print *, 'old check ',LUGB2, LUGI2, NUMVAL
       CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED OLD FCST 850 MB TEMP'
         DO KK = 1, ITOT
           T8502(KK) = GRID(KK)
            IF (T8502(KK).LT.273.15) THEN
             T850OLD=T850OLD+1 
            ENDIF
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK OLD FCST 850 TEMP', IRET
         ISTAT = IRET
         STOP
       ENDIF

C   1000 HEIGHT
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 007
       JPDS(6) = 100
       JPDS(7) = 1000
       print *, 'anl check ',LUGB2, LUGI2, NUMVAL
       CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED 1000 MB HEIGHT ANALYSIS'
         DO KK = 1, ITOT
           Z10002(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 1000 HEIGHT ANALYSIS ', IRET
         ISTAT = IRET
         STOP
       ENDIF

      print *, '850 TOTALS ', T850OPS, T850OLD
      print *, '2mT TOTALS ', T2OPS, T2OLD
      STOP
      END 
