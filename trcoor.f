c     Last change:  PB  30 May 2010

c     Program to convert coordinates bteween UTM - 3D - geographic

c     XU, YU, HU : UTM coordinates and elevation above ellipsoid
c     LON, LAT, HU : Geographic coordinates
c     XT, YT, ZT : 3D geocentric coordinates

      IMPLICIT REAL*8(A-H,L,O-Z)
      CHARACTER CHOIX*3, NSTA*8, FICH*20, FICH1*20

      COMMON/COMOB/CC,E2,PI,IFU,AK

      PI=4.d0*DATAN(1.d0)
      AK=0.9996d0
      KH=0
      IEP=0

      WRITE(*,*) 'Transformation UTM - Geographic - 3D '

c     Ellipsoide WGS84
c     Pour autre ellipsoide, recompiler le programme ou utiliser version ancienne de TRCOOR
      CC = 6399593.6257585d0
      EPRIM = 0.08209443794984d0
      E2=EPRIM*EPRIM

c     Choix saisie clavier ou fichier
   99 write(*,*) 'Saisie clavier (1) ou fichier (2) : '
      read(*,*,err=99) IS
      if(IS.ne.1.and.IS.ne.2) goto 99
      if(IS.eq.2) then
        write(*,*) 'Nom du fichier a lire   : '
        read(*,'(a)') FICH
        open(10,file=FICH)
        write(*,*) 'Nom du fichier a ecrire : '
        read(*,'(a)') FICH1
        open(11,file=FICH1)
      endif

c     Selection du fuseau
   98 write(*,*) 'Fuseau (0 = assistance) : '
      READ(*,*,err=98) IFU
      if(IFU.eq.0) then
        write(*,*) 'Entrez la longitude de la zone : '
        read(*,*) xlof
        if(xlof.gt.180.) xlof=xlof-360.
        IFU=DINT((183.d0+xlof)/6.d0)
        write(*,*) 'Fuseau utilise : ', IFU
      endif

c     Select type of input coordinates :

   12 WRITE(*,*) 'Input UTM, GEO or TRD : '
      READ (*,'(a3)',err=12) CHOIX
   13 IF (CHOIX.EQ.'UTM'.or.CHOIX.EQ.'utm') then
        if(KH.eq.0) then
   21     write(*,*) ' Hemisphere (North=1, South=-1) : '
          read(*,*,err=21) KH
          if(KH.ne.1.and.KH.ne.-1) goto 21
        endif
      goto 20
      endif

      IF (CHOIX.EQ.'GEO'.or.CHOIX.EQ.'geo') goto 30

      IF (CHOIX.EQ.'TRD'.or.CHOIX.EQ.'trd') goto 40
      GOTO 12

c ---------------------------------
c     Processing of UTM coordinates
c ---------------------------------
   20 IF(IS.EQ.1) then
        WRITE(*,*) 'Station name (8 characters max) : '
        READ(*,'(a8)') NSTA
        WRITE(*,*) 'UTM coordinates: Xutm, Yutm, Alt. : '
        READ(*,*,err=20) XU,YU,ZU
      else
        read(10,*,end=60,err=61) NSTA, XU, YU, ZU
      endif

      CALL UTMGEO(XU,YU,LON,LAT,KH)
      WRITE (*,*) '-- Coordinates on ellipsoid : '
      WRITE (*,*) '--     Longitude     Latitude       Altitude :'
      WRITE (*,*) '--          Grades :'
      WRITE(*,146) LON,LAT,ZU
      WRITE (*,*) '--          Degres decim.     :'
       LOND=LON*0.9d0
       LATD=LAT*0.9d0
      WRITE(*,146) LOND,LATD,ZU

      if(IS.eq.2) then
        write(11,148) NSTA,';DEC;',LOND,';',LATD,';',ZU
      endif

c     Conversion in deg., min., sec.

       ILON=INT(LOND)
       XLONM=DABS(LOND-ILON)*60.d0
       ILONM=int(XLONM)
       XLONS=(XLONM-ILONM)*60.d0
       ILAT=INT(LATD)
       XLATM=DABS(LATD-ILAT)*60.d0
       ILATM=int(XLATM)
       XLATS=(XLATM-ILATM)*60.d0
   24 WRITE (*,*) '--          Deg., Min., Sec.:'
      WRITE(*,147) ILON,ILONM,XLONS
      WRITE(*,147) ILAT,ILATM,XLATS

      if(IS.eq.2) then
        write(11,'(2a,2(a1,I2,a1,I3,a1,F8.5))') NSTA,';DMS',';',ILON,
     c';',ILONM,';',XLONS,';',ILAT,';',ILATM,';',XLATS
      endif


      CALL GEOCENTR (LON,LAT,ZU,XX,YY,ZZ)
      WRITE (*,*) '-- Tridim coordinates :'
      WRITE(*,143) XX,YY,ZZ

      if(IS.eq.2) then
        write(11,'(2a,3(a,F15.4))') NSTA,';TRD',';',XX,';',YY,';',ZZ
      endif


      GOTO 51

c -----------------------------------------
c     Processing of ellipsoidic coordinates
c -----------------------------------------

   30 if(IS.EQ.1) then
        WRITE(*,*) 'Station name (8 characters max) : '
        READ(*,'(a8)') NSTA
        WRITE(*,*) 'Ellipsoidic coordinates : '
        write(*,*) '  North and East : positives '
        write(*,*) '  South and West : negatives '
        write(*,*) 'Grades : 1 - Degr.dec.: 2 - Deg.Mn.Sec.: 3  : '
        read(*,*,err=30) IEP
      else
        if(IEP.EQ.0) then
          write(*,*) 'Grades : 1 - Degr.dec.: 2 - Deg.Mn.Sec.: 3  : '
          read(*,*,err=30) IEP
        endif
      endif

      GOTO (31,32,33) IEP
      goto 12

   31 if(IS.EQ.1) then
        WRITE(*,*) 'LON, LAT, Alt. (Gr.) : '
        READ(*,*,err=31) LON,LAT,ZU
      else
        read(10,*,end=60,err=61) LON,LAT,ZU
      endif
      goto 34

   32 if(IS.EQ.1) then
        WRITE(*,*) 'LON, LAT, Alt. (Deg. dec.) : '
        READ(*,*,err=32) LOND,LATD,ZU
      else
        read(10,*,end=60,err=61) LOND,LATD,ZU
      endif
      LON=LOND/0.9d0
      LAT=LATD/0.9d0
      goto 34

   33 if(IS.EQ.1) then
        WRITE(*,*) 'Longitude : Deg., Min., Sec.xxxx : '
        READ(*,*,err=33) IDEG,IMIN,FSEC
        WRITE(*,*) 'Latitude  : Deg., Min., Sec.xxxx : '
        READ(*,*,err=33) IDEG1,IMIN1,FSEC1
        WRITE(*,*) 'Altitude (m) : '
        READ(*,*,err=33) ZU
      else
        read(10,*,end=60,err=61) NSTA,IDEG,IMIN,FSEC,IDEG1,IMIN1,FSEC1,
     ,ZU
      endif

      JDEG=IABS(IDEG)
      KH=+1
      if(IDEG.LT.0) KH=-1
      XMIN=IMIN+FSEC/60.d0
      XDEG=JDEG+XMIN/60.d0
      LON=XDEG/0.9d0*KH

      JDEG1=IABS(IDEG1)
      KH=+1
      if(IDEG1.LT.0) KH=-1
      XMIN1=IMIN1+FSEC1/60.d0
      XDEG1=JDEG1+XMIN1/60.d0
      LAT=XDEG1/0.9d0*KH

   34 CALL GEOUTM(LON,LAT,XU,YU)
      WRITE (*,*) '-- UTM coordinates :'
      WRITE (*,143) XU,YU,ZU

      if(IS.eq.2) then
        write(11,'(2a,F15.4,a,F14.4,a,F9.4)') NSTA,';UTM;',XU,';',YU,
     c';',ZU
      endif

      CALL GEOCENTR (LON,LAT,ZU,XX,YY,ZZ)
      WRITE (*,*) '-- Tridim coordinates :'
      WRITE(*,143) XX,YY,ZZ

      if(IS.eq.2) then
        write(11,'(2a,3(a,F15.4))') NSTA,';TRD',';',XX,';',YY,';',ZZ
      endif


      GOTO 51



c ----------------------------------------
c     Processing of geocentric coordinates
c ----------------------------------------
   40 if(IS.EQ.1) then
        WRITE(*,*) 'Station name (8 characters max) : '
        READ(*,'(a8)') NSTA
        WRITE(*,*) 'Tridim coordinates : X, Y, Z : '
        READ(*,*,err=40) XX,YY,ZZ
      else
        read(10,*,end=60,err=61) NSTA, XX, YY, ZZ
      endif
      CALL CENTRGEO(XX,YY,ZZ,LON,LAT,ZU)
      WRITE (*,*) '-- Ellipsoidic coordinates :'
      WRITE (*,*) '--          Grades :'
      WRITE(*,146) LON,LAT,ZU
      WRITE (*,*) '--          Degres decim.     :'
       LOND=LON*0.9d0
       LATD=LAT*0.9d0
      WRITE(*,146) LOND,LATD,ZU

      if(IS.eq.2) then
        write(11,148) NSTA,';DEC;',LOND,';',LATD,';',ZU
      endif

c     Transformation in degres, minutes, sec.
      ILON=INT(LOND)
      XLONM=DABS(LOND-ILON)*60.d0
      ILONM=int(XLONM)
      XLONS=(XLONM-ILONM)*60.d0
      ILAT=INT(LATD)
      XLATM=DABS(LATD-ILAT)*60.d0
      ILATM=int(XLATM)
      XLATS=(XLATM-ILATM)*60.d0
      WRITE (*,*) '--          Degres, Min., Sec.:'
      WRITE(*,147) ILON,ILONM,XLONS
      WRITE(*,147) ILAT,ILATM,XLATS

      if(IS.eq.2) then
        write(11,'(2a,2(a1,I2,a1,I3,a1,F8.5))') NSTA,';DMS',';',ILON,
     c';',ILONM,';',XLONS,';',ILAT,';',ILATM,';',XLATS
      endif

      CALL GEOUTM(LON,LAT,XU,YU)
      WRITE (*,*) '-- UTM coordinates : '
      WRITE (*,143) XU,YU,ZU

      if(IS.eq.2) then
        write(11,'(2a,F15.4,a,F14.4,a,F9.4)') NSTA,';UTM;',XU,';',YU,
     c';',ZU
      endif

c Boucle (demande a continuer ou continue de lire le fichier)
   51 IF(IS.EQ.2) GOTO 13
      WRITE(*,'(a)') ' End: 0 - Other conversion: 1 '
      READ(*,*) IQ
      if(IQ.eq.1) goto 12
      goto 60

c     Formats
  142 FORMAT(2X,I3,3F12.3,1X,3(1X,F6.3))
  143 FORMAT(5X,3(F14.4,3X))
  144 FORMAT(5X,2F13.4,4X)
  145 FORMAT(I2,1X,I3,2F12.3,1X,F8.3,F8.4)
  146 FORMAT(5X,3F17.8)
  147 FORMAT(9X,I3,2x,I2,1x,F9.5)
  148 FORMAT(2a,f13.8,a1,f12.8,a1,f9.4)

   61 write(*,*) 'Data file error'

   60 close(10)
      close(11)
      STOP
      END


c     ---------------------------------
      SUBROUTINE GEOUTM(LONG,LAT,XX,YY)
c     ---------------------------------

c     TRANSFORMATION GEOGRAPHIQUE - U.T.M.
c     ENTREE : LONGITUDE ET LATITUDE EN GRADES
c     SORTIE :  COORD. UTM  EN METRES

      IMPLICIT REAL*8 (A-H,L,O-Z)
      COMMON/COMOB/CC,E2,PI,IFU,AK

      MI=IFU*6-183
      AMI=DBLE(MI)/0.9D0
      RD=PI/200.D0
      AM=LONG*RD
      AL=LAT*RD
      AM0=AMI*RD
      CO=DCOS(AL)
      AMU=AM-AM0
      XI=CO*DSIN(AMU)
      XI=0.5D0*DLOG((1.D0+XI)/(1.D0-XI))
c      TA=DATAN(DSIN(AL)/(CO*DCOS(AMU)))-AL
      TA=DATAN2(DSIN(AL),CO*DCOS(AMU))-AL
      GN=CC/DSQRT(1.D0+E2*CO*CO)
      CX=E2*(CO*XI)**2
      DX=GN*XI*(1.D0+CX/6.D0)
      DY=GN*TA*(1.D0+CX/2.D0)
      XX=500000.D0+AK*DX
      YY=AK*(DY+ARCME(AL))
      if(LAT) 1,2,2
    1 YY=(YY+10000000.d0)
    2 RETURN
      END


c     -----------------------------------
      SUBROUTINE UTMGEO(XX,YY,LON,LAT,KH)
c     -----------------------------------

c     TRANSFORMATION  UTM - GEOGRAPHIQUE
c     ENTREE :  COORD. UTM  EN  METRES
c     SORTIE :  COORD. GEOGRAPH. EN GRADES
c     HEMISPHERE NORD : KH=1
c     SUD  : KH=-1

      IMPLICIT REAL*8 (A-H,L,O-Z)
      COMMON/COMOB/CC,E2,PI,IFU,AK

      MI=IFU*6-183
      AMI=FLOAT(MI)/0.9D0
      RD=PI/200.D0
      AM0=AMI*RD
      YR=YY
      if(KH.EQ.1) goto 11
      YR=10.D6-YR
   11 YR=YR/AK
      P=KH*YR*PI*0.5D-7
      CO=DCOS(P)
      L=P*KH
      S=E2*CO*CO
      R=CC/DSQRT(1.d0+S)
      YR=YR-ARCME(L)
      U=(XX-5.D5)/AK/R
      V=S*U*U
      Q=YR/R*(1.D0-V/2.D0)
      U=U*(1.D0-V/6.D0)
      P=L+Q
      W=DEXP(U)
      AM=DATAN((W*W-1.D0)/(2.D0*W*DCOS(P)))
      V=DSIN(P)/DCOS(P)
      G=DATAN(V*DCOS(AM))
      V=1.D0+S-1.5D0*DSIN(L)*E2*(G-L)*DCOS(L)
      LON=AM+AM0
      LAT=L+V*(G-L)
      LON=(AM+AM0)/RD
      LAT=LAT/RD*KH
      END


c     ------------------
      FUNCTION ARCME(AL)
c     ------------------

c     Fonction Arc de meridien en fonction de la latitude :

      IMPLICIT REAL*8(A-H,L,O-Z)
      COMMON/COMOB/CC,E2,PI,IFU,AK

      AQ=E2*3.d0/4.d0
      BQ=E2*E2*15.d0/16.d0
      CQ=(E2**3)*35.d0/64.d0
      DQ=(E2**4)*315.d0/512.d0
      U=DSIN(AL)*DCOS(AL)
      V=DCOS(AL)**2
      AJ2=AL+U
      AJ4=(AJ2*3.d0+U*V*2.d0)/4.d0
      AJ6=(AJ4*5.d0+2.d0*U*V**2)/3.d0
      AJ8=(AJ6*7.d0+4.d0*U*V**3)/8.d0
      ARCME=CC*(AL-AQ*AJ2+BQ*AJ4-CQ*AJ6+DQ*AJ8)
      END


c     --------------------
      FUNCTION ECHUTM(X,Y)
c     --------------------

c     CALCUL DE L ECHELLE EN UN POINT ( UTM )

      IMPLICIT REAL*8(A-H,L,O-Z)
      COMMON/COMOB/CC,E2,PI,IFU,AK

      YI=Y/1000.D0
      FI=YI*PI/2.D0/AK*1.D-4
      FE=(1.D0+E2*DCOS(FI)*DCOS(FI))/CC/AK*1.D6
      FE=0.5D0*FE**2
      OK=(X-500000.D0)*1.D-6
      OQ=OK*OK
      ECHUTM=AK*(1.D0+FE*OQ+OQ*OQ*.255D-4)
      END


c     ------------------------------------
      SUBROUTINE CENTRGEO(X,Y,Z,LON,LAT,H)
c     ------------------------------------

c     Transformation de coordonnees geocentriques --> ellipsoidiques

      IMPLICIT REAL*8 (A-H,L,O-Z)
      COMMON/COMOB/CC,E2,PI,IFU,AK

      A=CC/DSQRT(1.D0+E2)
      B=A*A/CC
      P= DSQRT(X**2+Y**2)
      PB=B*P
      ZA=Z*A
      TETA= DATAN2(ZA,PB)
      LBD= DATAN2(Y,X)
      ST=DSIN(TETA)
      CT=DCOS(TETA)

      FX=Z+E2*B*ST*ST*ST
      FY=P-E2*A*CT*CT*CT/(1+E2)
      FI= DATAN2(FX,FY)
      CFI=DCOS(FI)
      AN=CC/DSQRT(1.D0+E2*CFI*CFI)

      LAT=FI*200.D0/PI
      LON=LBD*200.D0/PI
      H= P/CFI-AN
      END


c     ------------------------------------
      SUBROUTINE GEOCENTR(LON,LAT,H,X,Y,Z)
c     ------------------------------------

c     Passage coordonnees geographiques --> geocentriques

      IMPLICIT REAL*8(A-H,L,O-Z)
      COMMON/COMOB/CC,E2,PI,IFU,AK
      LATR=LAT*PI/200.D0
      LONR=LON*PI/200.D0
      CB=DCOS(LATR)
      SB=DSIN(LATR)
      CL=DCOS(LONR)
      SL=DSIN(LONR)
c     Grande normale GN
c     Note: CC rayon de courbure au pole - E2 = E **2
c           E = seconde excentricite - e2 =(a2-b2)/b2
      GN=CC/DSQRT(1.D0+E2*CB*CB)

c     Tridim coordinates

      X=(GN+H)*CB*CL
      Y=(GN+H)*CB*SL
      Z=(GN/(1.d0+E2)+H)*SB
      END
