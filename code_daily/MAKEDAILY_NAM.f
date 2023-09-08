       PROGRAM MAKEDAILY_NAM
C                .      .    .                                       .
C SUBPROGRAM:   MAKEDAILY_NAM 
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
      DIMENSION Z500F(ITOT),Z500A(ITOT),
     &     SLPF(ITOT),SLPA(ITOT),T850F(ITOT),T850A(ITOT),
     &     U850F(ITOT),V850F(ITOT),U850A(ITOT),V850A(ITOT),
     &     T2F(ITOT),T2A(ITOT),T700F(ITOT),T700A(ITOT),
     &     U250F(ITOT),U250A(ITOT),V250F(ITOT),V250A(ITOT), 
     &     U10F(ITOT),U10A(ITOT),V10F(ITOT),V10A(ITOT),
     &     SP850F(ITOT),SP850A(ITOT),SP250F(ITOT),SP250A(ITOT),
     &     SP10F(ITOT),SP10A(ITOT)

      DIMENSION Z500ERR(ITOT),SLPERR(ITOT),T850ERR(ITOT),
     &      T700ERR(ITOT),W850ERR(ITOT),W10ERR(ITOT),T2ERR(ITOT),
     &      W250ERR(ITOT)

C READ THE PRIMARY FORECAST HOUR, THE FORECAST HOUR OF
C   PRECIP, AND THE FORECAST HOUR FOR THE 3-HR PRESSURE
C   TENDENCY.  IF THE FCST HR IS 99, THIS MEANS THAT THE 
C   FILE ALREADY CONTAINS THE FIELDS THAT IT NEEDS 
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

       print *, 'IGDNUM ', IGDNUM
C   SEA LEVEL PRESSURE
       L = 0
       IV= 0
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 002 
       JPDS(6) = 102 
       JPDS(13) = 1
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED FCST SEA LEVEL PRESSURE AT F',FHR
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           SLPF(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK FCST SLP at F',FHR
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
         print *, 'UNPACKED FCST 2-M TEMP AT F',FHR
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           T2F(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK FCST 2-M TEMP at F',FHR
          ISTAT = IRET
         STOP
       ENDIF

C   10-M U 
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 33 
       JPDS(6) = 105
       JPDS(7) = 10 
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED FCST 10-M U AT F',FHR
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           U10F(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK FCST 10-M U at F',FHR
          ISTAT = IRET
         STOP
       ENDIF

C   10-M V  
       J = 0
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 34
       JPDS(6) = 105
       JPDS(7) = 10 
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED FCST 10-M V AT F',FHR
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           V10F(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK FCST 10-M V at F',FHR
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
        print *, 'UNPACKED FCST T850 AT F',FHR
        DO KK = 1, ITOT
          T850F(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK FCST 850 T'
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
        print *, 'UNPACKED FCST T700 AT F',FHR
        DO KK = 1, ITOT
          T700F(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK FCST 700 T'
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
        print *, 'UNPACKED FCST Z500 AT F',FHR
        DO KK = 1, ITOT
          Z500F(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK FCST 500 Z'
       ISTAT = IRET
       STOP
      ENDIF

c   250 mb U
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 033
      JPDS(6) = 100
      JPDS(7) = 250
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED FCST U250 AT F',FHR
        DO KK = 1, ITOT
          U250F(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK FCST 250 U'
       ISTAT = IRET
       STOP
      ENDIF

c   250 mb V
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 034
      JPDS(6) = 100
      JPDS(7) = 250
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED FCST V250 AT F',FHR
        DO KK = 1, ITOT
          V250F(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK FCST 250 V'
       ISTAT = IRET
       STOP
      ENDIF

c   850 mb U
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 033
      JPDS(6) = 100
      JPDS(7) = 850
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED FCST U850 AT F',FHR
        DO KK = 1, ITOT
          U850F(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK FCST 850 U'
       ISTAT = IRET
       STOP
      ENDIF

c   850 mb V
      J=0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 034
      JPDS(6) = 100
      JPDS(7) = 850
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED FCST V850 AT F',FHR
        DO KK = 1, ITOT
          V850F(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK FCST 850 V'
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

       print *, 'IDGNUM ', IGDNUM
C   SEA LEVEL PRESSURE
       L = 0
       IV= 0
       J = 0
       JPDS = -1
c       JPDS(3) = IGDNUM
       JPDS(5) = 002
       JPDS(6) = 102
       CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED ANALYSIS SEA LEVEL PRESSURE'
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           SLPA(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK ANALYSIS SLP'
          ISTAT = IRET
c         STOP
       ENDIF

C   2-M TEMP
       J = 0
       JPDS = -1
c       JPDS(3) = IGDNUM
       JPDS(5) = 11
       JPDS(6) = 105
       JPDS(7) = 2
       CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED ANALYSIS 2-M TEMP'
         II = 1
         JJ = JJ1
         DO KK = 1, ITOT
           T2A(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,285)IV,JPDS(5),L,IRET
         WRITE(6,*)' COULD NOT UNPACK ANALYSIS 2-M TEMP'
          ISTAT = IRET
         STOP
       ENDIF

C   500 HEIGHT
       J = 0
       JPDS = -1
c       JPDS(3) = IGDNUM
       JPDS(5) = 007 
       JPDS(6) = 100
       JPDS(7) = 500
       print *, 'anl check ',LUGB2, LUGI2, NUMVAL
       CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED 500 MB HEIGHT ANALYSIS'
         DO KK = 1, ITOT
           Z500A(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 500 HEIGHT ANALYSIS ', IRET
         ISTAT = IRET
         STOP
       ENDIF

C   850 TEMP 
       J = 0
       JPDS = -1
c       JPDS(3) = IGDNUM
       JPDS(5) = 011
       JPDS(6) = 100
       JPDS(7) = 850
       print *, 'anl check ',LUGB2, LUGI2, NUMVAL
       CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED 850 MB TEMP ANALYSIS'
         DO KK = 1, ITOT
           T850A(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 850 TEMP ANALYSIS ', IRET
         ISTAT = IRET
         STOP
       ENDIF

C   700 TEMP
       J = 0
       JPDS = -1
c       JPDS(3) = IGDNUM
       JPDS(5) = 011
       JPDS(6) = 100
       JPDS(7) = 700
       CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
       IF(IRET.EQ.0) THEN
         print *, 'UNPACKED 700 MB TEMP ANALYSIS'
         DO KK = 1, ITOT
           T700A(KK) = GRID(KK)
         ENDDO
       ELSE
         WRITE(6,*)' COULD NOT UNPACK 700 TEMP ANALYSIS ', IRET
         ISTAT = IRET
         STOP
       ENDIF

c   250 mb U
      J=0
      JPDS = -1
c      JPDS(3) = IGDNUM
      JPDS(5) = 033
      JPDS(6) = 100
      JPDS(7) = 250
      CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED U250 ANALYSIS'
        DO KK = 1, ITOT
          U250A(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK U250 ANALYSIS ', IRET
       ISTAT = IRET
      ENDIF

c   250 mb V
      J=0
      JPDS = -1
c      JPDS(3) = IGDNUM
      JPDS(5) = 034
      JPDS(6) = 100
      JPDS(7) = 250
      CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED V250 ANALYSIS'
        DO KK = 1, ITOT
          V250A(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK V250 ANALYSIS ', IRET
       ISTAT = IRET
      ENDIF

c   850 mb U
      J=0
      JPDS = -1
c      JPDS(3) = IGDNUM
      JPDS(5) = 033
      JPDS(6) = 100
      JPDS(7) = 850
      CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED U850 ANALYSIS'
        DO KK = 1, ITOT
          U850A(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK U850 ANALYSIS ', IRET
       ISTAT = IRET
      ENDIF

c   850 mb V
      J=0
      JPDS = -1
c      JPDS(3) = IGDNUM
      JPDS(5) = 034
      JPDS(6) = 100
      JPDS(7) = 850
      CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED V850 ANALYSIS'
        DO KK = 1, ITOT
          V850A(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK V850 ANALYSIS ', IRET
       ISTAT = IRET
      ENDIF

c   10 mb U
      J=0
      JPDS = -1
c      JPDS(3) = IGDNUM
      JPDS(5) = 033
      JPDS(6) = 105
      JPDS(7) = 10
      CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED U10 ANALYSIS'
        DO KK = 1, ITOT
          U10A(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK U10 ANALYSIS ', IRET
       ISTAT = IRET
      ENDIF

c   10 mb V
      J=0
      JPDS = -1
c      JPDS(3) = IGDNUM
      JPDS(5) = 034
      JPDS(6) = 105
      JPDS(7) = 10
      CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,IRET)
      IF(IRET.EQ.0) THEN
        print *, 'UNPACKED V10 ANALYSIS'
        DO KK = 1, ITOT
          V10A(KK) = GRID(KK)
        ENDDO
      ELSE
       WRITE(6,*)' COULD NOT UNPACK V10 ANALYSIS ', IRET
       ISTAT = IRET
      ENDIF

C  COMPUTE Z500 ERROR
       DO K = 1, ITOT
         Z500ERR(K)=Z500A(K)-Z500F(K)
       ENDDO

C  COMPUTE SLP ERROR
       DO K = 1, ITOT
         SLPERR(K)=SLPA(K)-SLPF(K)
         if (K .eq. 1000) then
           print *, 'slpest ', SLPF(K), SLPA(K)
         endif
       ENDDO

C  COMPUTE T850 ERROR
       DO K = 1, ITOT
         T850ERR(K)=T850A(K)-T850F(K)
         if (K .eq. 1000) then
           print *, '850test ', T850F(K), T850A(K)
         endif
       ENDDO

C  COMPUTE T700 ERROR
       DO K = 1, ITOT
         T700ERR(K)=T700A(K)-T700F(K)
       ENDDO

C  COMPUTE 850 WIND SPEED ERROR
       DO K = 1, ITOT
         SP850A(K)=SQRT(U850A(K)**2+V850A(K)**2)
         SP850F(K)=SQRT(U850F(K)**2+V850F(K)**2)
         W850ERR(K)=SP850A(K)-SP850F(K)
       ENDDO

C  COMPUTE 250 WIND SPEED ERROR
       DO K = 1, ITOT
         SP250A(K)=SQRT(U250A(K)**2+V250A(K)**2)
         SP250F(K)=SQRT(U250F(K)**2+V250F(K)**2)
         W250ERR(K)=SP250A(K)-SP250F(K)
       ENDDO
   
C  COMPUTE 2-M T ERROR
       DO K = 1, ITOT
         T2ERR(K)=T2A(K)-T2F(K)
       ENDDO

C  COMPUTE 10 WIND SPEED ERROR
       DO K = 1, ITOT
         SP10A(K)=SQRT(U10A(K)**2+V10A(K)**2)
         SP10F(K)=SQRT(U10F(K)**2+V10F(K)**2)
         W10ERR(K)=SP10A(K)-SP10F(K)
       ENDDO

C PACK UP THE ERRORS 

      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGB4
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGB4,FNAME,IRET)
      print *,'IRET from BAOPEN on LUGB4 = ', IRET

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
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,Z500ERR,IRET)
       print *,'IRET from Z500ERR PUTGB on LUGB4 = ', IRET

       KPDS(5)=174
       KPDS(6)=1
       KPDS(7)=0
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=129
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,SLPERR,IRET)
       print *,'IRET from SLPERR PUTGB on LUGB4 = ', IRET

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
       KPDS(19)=129
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,T850ERR,IRET)
       print *,'IRET from T850ERR PUTGB on LUGB4 = ', IRET

       KPDS(5)=158
       KPDS(6)=100
       KPDS(7)=700
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=129
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,T700ERR,IRET)
       print *,'IRET from T700ERR PUTGB on LUGB4 = ', IRET

       KPDS(5)=161
       KPDS(6)=100
       KPDS(7)=850
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=129
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,W850ERR,IRET)
       print *,'IRET from W850ERR PUTGB on LUGB4 = ', IRET

       KPDS(5)=161
       KPDS(6)=100
       KPDS(7)=250
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=129
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,W250ERR,IRET)
       print *,'IRET from W250ERR PUTGB on LUGB4 = ', IRET

       KPDS(5)=158
       KPDS(6)=105
       KPDS(7)=002
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=129
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,T2ERR,IRET)
       print *,'IRET from T2ERR PUTGB on LUGB4 = ', IRET

       KPDS(5)=161
       KPDS(6)=105
       KPDS(7)=010
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=129
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,W10ERR,IRET)
       print *,'IRET from W10ERR PUTGB on LUGB4 = ', IRET
 
C  PACK UP THE ANALYSIS FIELDS
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
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,Z500A,IRET)
       print *,'IRET from Z500A PUTGB on LUGB4 = ', IRET

       KPDS(5)=002
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
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,SLPA,IRET)
       print *,'IRET from SLPA PUTGB on LUGB4 = ', IRET

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
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,T850A,IRET)
       print *,'IRET from T850A PUTGB on LUGB4 = ', IRET

       KPDS(5)=011
       KPDS(6)=100
       KPDS(7)=700
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,T700A,IRET)
       print *,'IRET from T700A PUTGB on LUGB4 = ', IRET

       KPDS(5)=032
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
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,SP850A,IRET)
       print *,'IRET from SP850A PUTGB on LUGB4 = ', IRET

       KPDS(5)=032
       KPDS(6)=100
       KPDS(7)=250
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,SP250A,IRET)
       print *,'IRET from SP250A PUTGB on LUGB4 = ', IRET

       KPDS(5)=011
       KPDS(6)=105
       KPDS(7)=002
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,T2A,IRET)
       print *,'IRET from T2A PUTGB on LUGB4 = ', IRET

       KPDS(5)=032
       KPDS(6)=105
       KPDS(7)=010
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,SP10A,IRET)
       print *,'IRET from SP0A PUTGB on LUGB4 = ', IRET

C  PACK UP THE FORECAST FIELDS
       KPDS(5)=008
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
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,Z500F,IRET)
       print *,'IRET from Z500F PUTGB on LUGB4 = ', IRET

       KPDS(5)=001
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
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,SLPF,IRET)
       print *,'IRET from SLPF PUTGB on LUGB4 = ', IRET

       KPDS(5)=012
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
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,T850F,IRET)
       print *,'IRET from T850F PUTGB on LUGB4 = ', IRET

       KPDS(5)=012
       KPDS(6)=100
       KPDS(7)=700
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,T700F,IRET)
       print *,'IRET from T700F PUTGB on LUGB4 = ', IRET

       KPDS(5)=037
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
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,SP850F,IRET)
       print *,'IRET from SP850F PUTGB on LUGB4 = ', IRET

       KPDS(5)=037
       KPDS(6)=100
       KPDS(7)=250
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,SP250F,IRET)
       print *,'IRET from SP250F PUTGB on LUGB4 = ', IRET

       KPDS(5)=012
       KPDS(6)=105
       KPDS(7)=002
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,T2F,IRET)
       print *,'IRET from T2F PUTGB on LUGB4 = ', IRET

       KPDS(5)=037
       KPDS(6)=100
       KPDS(7)=010
       KPDS(8)=YEAR
       KPDS(9)=MON
       KPDS(10)=DAY
       KPDS(11)=CYC
       KPDS(14)=FHR
       KPDS(15)=0
       KPDS(16)=0
       KPDS(19)=2
       KPDS(22)=3
       CALL PUTGB(LUGB4,ITOT,KPDS,KGDS,MASK2,SP10F,IRET)
       print *,'IRET from SP10F PUTGB on LUGB4 = ', IRET

      CALL BACLOSE(LUGB4,IRET)
      STOP
      END 
