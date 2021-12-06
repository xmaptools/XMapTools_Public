!-----Version: 09.03.2019
!               ***********
!               * gmini.f *
!               ***********
!     Subroutines for G-Minimization
!
!     Any suggestions, complaints or comments are greatly appreciated
!     by the author and should be sent to:
!
!          Christian de Capitani
!          Mineralogisch-Petrographisches Institut
!          Universitaet Basel
!          Bernoullistrasse 30
!          CH-4056 BASEL
!
!
!----- GGK(i) = G for each phase (original, unchanged)
!----- GG(i) = G for each phase (adjusted after each loop)
!----- G(i) = G for phases in calc.str.
!----- XX(n,i) = composition of phase n
!----- X(k,i) = composition of phase k in calc.str.
!
!----- coding of SUGCOD(i)
!----- 1: starting point for minimization
!-----    endm : endmember
!-----    end2 : second try: close to endmember
!-----    stre : structural endmember (do not use??)
!-----    scan : found by scan
!-----    stab : previously stable composition
!-----    usta : previously unstable but minimal composition
!-----    seed : initial conposition from 'SEED'
!-----    phas : (no minimization) is a phase
!-----    elem : (no minimization) is an element
!-----    null : (no minimization) is excluded
!----- 2: first minimization
!-----    norm : normal steepest descent (step > 0)
!-----    endm : added steps towards endmembers (step < 0)
!-----    stre : added steps towards structural endmembers (step < 0)
!----- 3: refinement
!-----    minn : Newton-Raphson
!-----    end+ : direction of + endmembers
!-----    end- : direction of - endmembers
!-----    str+ : direction of + structural endmembers
!-----    str- : direction of - structural endmembers
!
!      SUGUSE = 10 for phases and elements
!             =  1 minimum for step
!             =  2 minisug
!
!----- LOGMIN(IS) = number of stable phases of IS in last loop
!----- LOMIN1(IS) = number of stable phases of IS in previous loop
!----- SOSKIP(IS) used for fixed phases. 
!-----            = 1 if not used in calculations
!----- SOJUMP(IS) if MUE of all endmembers are > 10^20,
!-----            solution is not used.
!
      SUBROUTINE CALSTR
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 N,I
!-----
      DO 500,N=1,NPHA
      DO 510,I=1,NUN
  510 X(N,I)=XX(N,I)
      NN(N)=0.0D0
!!!
      GG(N)=GGK(N)
      G(N)=GGK(N)
      NUMMER(N)=N
      EMCODE(N)=0
      ISTAB(N)=0
      SUGUSE(N)=10
      IF (NULL(N)) THEN
      SUGG(N)=-N
      SUGCOD(N)='null'
      ELSE
      SUGG(N)=N
      SUGCOD(N)='phas'
      END IF
      DO 520,I=1,EMAX
  520 XEM(N,I)=0.0D0
  500 CONTINUE
      BLKSUM=0.0D0
      DO 600,N=1,NUN
      BLKSUM=BLKSUM+BULK(N)
!Cfeb07      SUGG(N)=-N
      SUGG(N)=-N
      SUGCOD(N)='elem'
  600 NN(N)=BULK(N)
      SUGNR=NPHA
      RETURN
      END
!-----
!******************************
      SUBROUTINE THERIA
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 I,II,IIX,I001,K,LOO2,CLCOD,ADDCOD,J,I1,I2,ISE,IS,IE
      REAL*8 SUMME,NEWN(COMAX)
!      REAL*8 TIM1,TIM2
!      REAL*8 GSC,XXSC(EMAX)
      CHARACTER*20 CH23
      CHARACTER*8 TEXT
      CHARACTER*250 ASE1,ASE2
      LOGICAL*4 CODE,AFAIL,SAMEAS,WAFA
!-----
!====
!      call cpu_time(TIM1)
!---   ISGMIN is set .FALSE. in PROREAD
!---   signalisiert, dass MUE und anderes veraendert werden darf
!---   nicht fuer thermo oder thalia, nur wenn THERIA aufgerufen wird.
!---   wird in PRTCAL wieder .FALSE. gesetzt
       ISGMIN=.TRUE.
!====
      IF (.NOT.GIBBSFLAG) THEN
        I1=1
        I2=NSOL
        WAFA=DRU
        DRU=.TRUE.
!        WRITE (UNIT=scr, &
!        FMT='(''-------------> Gibbstest: from THERIA'')')
        CALL GIBBSTEST(I1,I2)
        DRU=WAFA
      END IF
      TEXT=' '
      NMAX=NPHA
      NMAXMAX=NPHA
      SUGLOOP=NPHA+1
      DISTAMAX=1.0D0
      GTOT=100.0D0
      GTEST=DABS(TEST)
      TRY=1
      GNOM=0
      ASE1=' '
      ASE2=' '
      IF (LO1MAX.LT.0) THEN
      GTEST=-100.D0
      LO1MAX=-LO1MAX
      END IF
      DO 400,I=1,NSOL
      LOCMIN(I)=0
      LOMIN1(I)=0
      NPREV(I)=0
      MINIG(I)=1D20
      MINISUG(I)=0
      DO 402,II=1,NEND(I)
      GTRAC(I,II)=1.0D30
      MINIX(I,II)=0.0D0
  402 MINIX1(I,II)=0.0D0
  400 CONTINUE
      IF (NMAX.GT.CALMAX) THEN
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=1000) CALMAX,NMAX
      WRITE (UNIT=out,FMT=1000) CALMAX,NMAX
 1000 FORMAT (// &
      ' The maximum number of phases considered for minimization', &
      ' is set to: ',I5/ &
      ' The number of phases is larger (even without solutions) :',I5/ &
      ' possible actions:'/ &
      '   a) Increase the value of CALMAX in the parameter statements'/ &
      '   b) Check the stabilities in smaller subsystems and then'/ &
      '      exclude (or delete) the unstable phases.')
      STOP
      END IF
!
      DO I=1,3
      DO II=1,12
      STATI(I,II)=0
      END DO
      END DO  
!
!-----
      DO 500,I=1,NSOL
      IIX=0
      DO 502,II=1,NEND(I)
      ZEND(I,II)=0
      IF (G(EM(I,II)).GT.1D20) IIX=IIX+1
  502 CONTINUE
      IF (IIX.EQ.NEND(I)) THEN
      SOJUMP(I)=1
      ELSE
      SOJUMP(I)=0
      END IF
  500 CONTINUE
      LOO1A=0
      SAMEAS=.FALSE.
      AFAIL=.TRUE.
!-----start main loop
      DO 650,LOO1=1,LO1MAX
!C====
!C      IF (LOO1.GT.50) TEST=-TEST
!C====
      IF (TRY.GT.1) LOO1A=LOO1A+1
!DC      IF (GTOT.LE.GTEST.OR.(TRY.GT.1.AND.DISTAMAX.LT.DXMIN)) THEN
!DC      IF (GTOT.LE.GTEST) THEN
!DC      IF (GTOT.LE.GTEST.OR.DISTAMAX.LT.DXMIN) THEN
      CLCOD=1
      ADDCOD=0
      IF (LOO1.EQ.2) ADDCOD=1
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=scr,FMT='(''AFAIL,SAMEAS,GTOT ''L2,L2,1PE15.8)') & 
      AFAIL,SAMEAS,GTOT
      WRITE (UNIT=out,FMT='(''AFAIL,SAMEAS,GTOT ''L2,L2,1PE15.8)') &
      AFAIL,SAMEAS,GTOT
      END IF
!==
!cdc16feb2017      IF ((.NOT.AFAIL.AND.GTOT.LE.GTEST).OR.SAMEAS.AND.LOO1.GT.10) THEN
      IF ((.NOT.AFAIL.AND.GTOT.LE.GTEST).OR.SAMEAS.AND.LOO1.GT.10) THEN
      TRY=TRY+1
      CLCOD=0
      IF (TRY.GT.2) GO TO 25
      ADDCOD=1
      END IF
!==
      DO 660,I=1,NSOL
      MINIG(I)=1D20
      MINISUG(I)=0
      DO 662,II=1,NEND(I)
      MINIX1(I,II)=MINIX(I,II)
  662 MINIX(I,II)=0.0D0
  660 CONTINUE
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      END IF
!==
      CALL REBULK
      IF (LOO1.GT.1) THEN
!====
      IF (LOO1.EQ.2.AND.NSED.GT.0) THEN
        DO ISE=1,NSED
          IS=SIS(ISE)
          DO IE=1,NEND(IS)
            XXEM(IE)=SXX(ISE,IE)
          END DO
          SUGCOD(0)='seed'
          CALL NEWPH(IS)
        END DO
      ELSE
      CALL ADDPH(ADDCOD)
      END IF
!===
!-----
      ELSE
      DO 665,I=1,NUN
  665 G(I)=1D20
      END IF
!====
!====
      CALL FULLRED
      CODE=.TRUE.
!-----start secondary loop
!-----in subr. REDUCE, DABS(X)<1D-16 is set to zero (NOT NOW!!!!)
!-----therefore test for smallest positive N/X is X>0.0D0
      DO 640,LOO2=1,100
      IF (.NOT.CODE) GOTO 24
      CODE=.FALSE.
      DO 630,K=(NUN+1),NMAX
      IF (SUGG(K).LT.0) GOTO 630
!=====
!!!!      IF (SUGUSE(K).NE.1) GOTO 630
!=====
      SUMME=0D0
      DO 633,II=1,NUN
  633 SUMME=SUMME+G(II)*X(K,II)
      IF (SUMME-G(K).GT.0D0) THEN
!CCCC      IF (SUMME-G(K).GT.1D-16) THEN
!===== mach rea
!==
      IF (TEST.LT.0.0D0) THEN
      IF (EMCODE(K).GT.0) THEN
        TEXT=SOLNAM(EMCODE(K))
      ELSE
        TEXT=NAME(NUMMER(K))
      END IF
      WRITE (UNIT=scr,FMT='('' now: '',I6,2X,A,I4)') SUGG(K),TEXT, &
      SUGUSE(K)
      WRITE (UNIT=out,FMT='('' now: '',I6,2X,A,I4)') SUGG(K),TEXT, &
      SUGUSE(K)
      WRITE (UNIT=scr,FMT='('' cf= '',100(2X,1PE15.8))') &
       (X(K,J),J=1,NUN)
      WRITE (UNIT=out,FMT='('' cf= '',100(2X,1PE15.8))') &
       (X(K,J),J=1,NUN)
      WRITE (UNIT=scr,FMT='('' dG = '',1PE15.8)') SUMME-G(K)
      WRITE (UNIT=out,FMT='('' dG = '',1PE15.8)') SUMME-G(K)
      END IF
!==
      I001=K
      CALL MACHREA(I001,IIX,NEWN)
!---- follow machrea
      IF (IIX.NE.0) THEN
      CALL COLCHG(I001,IIX)
      DO 635,I=1,NUN
  635 NN(I)=NEWN(I)
      NN(K)=0D0
      ELSE
      GOTO 630
      END IF
!-----
      CALL REDUCE(IIX)
!----- the following two lines add to robustness, but slow down
!CCCC      CALL FULLRES
!CCCC      CALL FULLRED
      CODE=.TRUE.
!===== end mach rea
      END IF
!-----
  630 CONTINUE
!----- the following two lines reconstruct calc-matrix
      CALL FULLRES
      CALL FULLRED
  640 CONTINUE
!-----end of secondary loop
   24 GTOT=0.0D0
      DO 680,I=1,NUN
  680 GTOT=GTOT+DABS(G(I))
!CCCC      IF (LOO2.GE.80) TEST=-DABS(TEST)
!CCCC      IF (LOO1.GE.100) TEST=-DABS(TEST)
      CALL CLEAN(CLCOD)
      CALL ATEST(AFAIL)
      WRITE (UNIT=ASE2,FMT='(40I7)') (SUGG(I),I=1,NUN)
      IF (ASE1.EQ.ASE2) SAMEAS=.TRUE.
!--
      IF (TEST.LT.0.0D0) THEN
      I001=LOO2-1
      CH23='------------------'
      WRITE (UNIT=scr,FMT=1050) LOO1,TRY,I001,GTOT,DISTAMAX,CH23
      WRITE (UNIT=out,FMT=1050) LOO1,TRY,I001,GTOT,DISTAMAX,CH23
 1050 FORMAT (/' ',I4,4X,'TRY =',I3,4X,'LOO2 =',I4,4X, &
      'G(-) =',1PE12.5,4X,'stepsize =',1PE12.5,2X,A17)
      WRITE (UNIT=scr,FMT='('' MaxG(-) = '',1PE12.5)') GROSSG
      WRITE (UNIT=out,FMT='('' MaxG(-) = '',1PE12.5)') GROSSG
      WRITE (UNIT=scr,FMT=1051) CLCOD,AFAIL
      WRITE (UNIT=out,FMT=1051) CLCOD,AFAIL
 1051 FORMAT (' CLCOD =',I4,4X,' AFAIL =',L4)
      WRITE (UNIT=scr,FMT='('' ase1 '',A132)') ASE1
      WRITE (UNIT=out,FMT='('' ase1 '',A132)') ASE1
      WRITE (UNIT=scr,FMT='('' ase2 '',A132)') ASE2
      WRITE (UNIT=out,FMT='('' ase2 '',A132)') ASE2
      CALL PRTSTR(1,NUN)
!!      CALL PRTSTR(1,NPHA)  

      DO I=1,NUN
        IF (SUGCOD(I)(1:4).EQ.'seed') STATI(1,1)=STATI(1,1)+1
        IF (SUGCOD(I)(1:4).EQ.'see2') STATI(1,2)=STATI(1,2)+1
        IF (SUGCOD(I)(1:4).EQ.'avrg') STATI(1,3)=STATI(1,3)+1
        IF (SUGCOD(I)(1:4).EQ.'prev') STATI(1,4)=STATI(1,4)+1
        IF (SUGCOD(I)(1:4).EQ.'last') STATI(1,5)=STATI(1,5)+1
        IF (SUGCOD(I)(1:4).EQ.'scan') STATI(1,6)=STATI(1,6)+1
        IF (SUGCOD(I)(1:4).EQ.'endm') STATI(1,7)=STATI(1,7)+1
        IF (SUGCOD(I)(1:4).EQ.'stre') STATI(1,8)=STATI(1,8)+1
        IF (SUGCOD(I)(1:4).EQ.'stab') STATI(1,9)=STATI(1,9)+1
        IF (SUGCOD(I)(1:4).EQ.'usta') STATI(1,10)=STATI(1,10)+1
        IF (SUGCOD(I)(1:4).EQ.'scaw') STATI(1,11)=STATI(1,11)+1
        IF (SUGCOD(I)(1:4).EQ.'idea') STATI(1,12)=STATI(1,12)+1

        IF (SUGCOD(I)(6:9).EQ.'norm') STATI(2,1)=STATI(2,1)+1
        IF (SUGCOD(I)(6:9).EQ.'endm') STATI(2,2)=STATI(2,2)+1
        IF (SUGCOD(I)(6:9).EQ.'dirs') STATI(2,3)=STATI(2,3)+1
        IF (SUGCOD(I)(6:9).EQ.'end2') STATI(2,4)=STATI(2,4)+1

        IF (SUGCOD(I)(11:14).EQ.'minn') STATI(3,1)=STATI(3,1)+1
        IF (SUGCOD(I)(11:14).EQ.'end+') STATI(3,2)=STATI(3,2)+1
        IF (SUGCOD(I)(11:14).EQ.'end-') STATI(3,3)=STATI(3,3)+1
        IF (SUGCOD(I)(11:14).EQ.'str+') STATI(3,4)=STATI(3,4)+1
        IF (SUGCOD(I)(11:14).EQ.'str-') STATI(3,5)=STATI(3,5)+1
      END DO

      END IF
!--
      ASE1=ASE2
  650 CONTINUE
!-----end of main loop
   25 CONTINUE
!====
!      call cpu_time(TIM2)
!      print '("TheriaTime = ",f10.7," seconds.")',TIM2-TIM1
!      TIM1=TIM2
!====
!--
      IF (TEST.LT.0.0D0) THEN
       WRITE (UNIT=scr,FMT='(//''seed '',I5)') STATI(1,1)
       WRITE (UNIT=scr,FMT='(''see2 '',I5)') STATI(1,2)
       WRITE (UNIT=scr,FMT='(''avrg '',I5)') STATI(1,3)
       WRITE (UNIT=scr,FMT='(''prev '',I5)') STATI(1,4)
       WRITE (UNIT=scr,FMT='(''last '',I5)') STATI(1,5)
       WRITE (UNIT=scr,FMT='(''scan '',I5)') STATI(1,6)
       WRITE (UNIT=scr,FMT='(''endm '',I5)') STATI(1,7)
       WRITE (UNIT=scr,FMT='(''stre '',I5)') STATI(1,8)
       WRITE (UNIT=scr,FMT='(''stab '',I5)') STATI(1,9)
       WRITE (UNIT=scr,FMT='(''usta '',I5)') STATI(1,10)
       WRITE (UNIT=scr,FMT='(''scaw '',I5)') STATI(1,11)
       WRITE (UNIT=scr,FMT='(''idea '',I5)') STATI(1,12)
       WRITE (UNIT=scr,FMT='(/''norm '',I5)') STATI(2,1)
       WRITE (UNIT=scr,FMT='(''endm '',I5)') STATI(2,2)
       WRITE (UNIT=scr,FMT='(''dirs '',I5)') STATI(2,3)
       WRITE (UNIT=scr,FMT='(''end2 '',I5)') STATI(2,4)
        WRITE (UNIT=scr,FMT='(/''minn '',I5)') STATI(3,1)
        WRITE (UNIT=scr,FMT='(''end+ '',I5)') STATI(3,2)
        WRITE (UNIT=scr,FMT='(''end- '',I5)') STATI(3,3)
        WRITE (UNIT=scr,FMT='(''str+ '',I5)') STATI(3,4)
        WRITE (UNIT=scr,FMT='(''str- '',I5)') STATI(3,5)
     END IF
!--
!====
      CALL PRTCAL
      RETURN
      END
!-----
!******************************
      SUBROUTINE REBULK
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 I,K
      REAL*8 B(COMAX),F1,REST
!-----
      DO 500,I=1,NUN
      B(I)=0.0D0
      DO 500,K=1,NUN
      B(I)=B(I)+X(K,I)*NN(K)
  500 CONTINUE
      REST=0.0D0
!-
      IF (TEST.LT.0.0D0) THEN
      WRITE (6,1000) LOO1
      WRITE (UNIT=out,FMT=1000) LOO1
 1000 FORMAT (' bulk test:  original',17X,'new',8X,'loop =',I5)
      END IF
!-
      DO 600,I=1,NUN
      REST=REST+DABS(B(I)-BULK(I))
!-
      IF (TEST.LT.0.0D0) THEN
      F1=B(I)-BULK(I)
      WRITE (6,1001) BULK(I),B(I),F1
      WRITE (UNIT=out,FMT=1001) BULK(I),B(I),F1
 1001 FORMAT (1X,3F20.10)
      END IF
!-
  600 CONTINUE
      BLKSHIFT=REST
!      WRITE (UNIT=6,FMT=1005) LOO1,REST
! 1005 FORMAT ('loop:',I4,'   restbulk:', 1PE12.5)
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE RKOFCHG(RKOF,I1,I2)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 I1,I2,I
      REAL*8 RKOF(0:CALMAX,COMAX)
!-----
      DO 500,I=1,NUN
  500 RKOF(0,I)=RKOF(I1,I)
      DO 502,I=1,NUN
  502 RKOF(I1,I)=RKOF(I2,I)
      DO 504,I=1,NUN
  504 RKOF(I2,I)=RKOF(0,I)
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE CLEAN(CLCOD)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 K,II,IS,I,J,IE,I001,I1,I2,CLCOD
      REAL*8 XXSC(EMAX),GSC,RKOF(0:CALMAX,COMAX),FF(COMAX)
!-----
      IF (NMAX.GT.NMAXMAX) NMAXMAX=NMAX
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=6,FMT='('' NMAX= '',I9)') NMAX
      WRITE (UNIT=out,FMT='('' NMAX='',I9)') NMAX
      CALL PRTSTR(1,NUN)
      END IF
!==
      IF (LOO1.EQ.1) THEN
      DO 500,K=1,NMAX
  500 IF (SUGG(K).LT.0.AND.SUGG(K).GE.-NUN) G(K)=0.0D0
      GTOT=100.0D0
      END IF
!==
!!----
!!---- check for stable endmembers
!!---- is in prtcal and should stay there
!!---- cdc mar2018: do not put it here
!==
!----- check if G(I) are OK
      IF (TEST.LT.0.0D0) THEN
      DO 600,I=1,NMAX
      IF (EMCODE(I).GT.0) THEN
      IS=EMCODE(I)
      DO 602,J=1,NEND(IS)
  602 XXSC(J)=XEM(I,J)
      CALL GNONID(IS,XXSC,GSC)
      IF (DABS(G(I)-GSC).GT.1D-10) THEN
      WRITE (6,1000) SUGG(I),I,SOLNAM(EMCODE(I))
      WRITE (out,1000) SUGG(I),I,SOLNAM(EMCODE(I))
 1000 FORMAT (/' before clean: DABS(G-GSC) > 1D-10 SUGG/ph:' &
      ,I6,I6,2X,A)
      END IF
      END IF
  600 CONTINUE
!----- check if reactions are balanced
!1---- 1: store all coefficients (2 IS FURTHER DOWN)
      DO 605,I=1,NMAX
      DO 605,J=1,NUN
      RKOF(I,J)=X(I,J)
  605 CONTINUE
      END IF
!==
!==
!-----
      DO 502,K=(NUN+1),NMAX
      DO 502,II=1,NUN
  502 G(K)=G(K)-X(K,II)*G(II)
      DO 504,K=1,NUN
      IF (ISTAB(K).NE.1) ISTAB(K)=LOO1
  504 G(K)=0.0D0
!-----
      DO 506,K=1,NUN
      IF (ISTAB(K).NE.1) ISTAB(K)=LOO1
  506 CONTINUE
      DO 508,II=1,NSOL
      LOMIN1(II)=LOCMIN(II)
      LOCMIN(II)=0
  508 STEM(II)=0
!-----
      K=1
   25 IF (K.GT.NMAX) GO TO 1
!=====
      IF (NUMMER(K).GT.0) THEN
      IF (EMSOL(NUMMER(K)).GT.0.AND.K.LE.NUN) THEN
      STEM(EMSOL(NUMMER(K)))=STEM(EMSOL(NUMMER(K)))+1
      END IF
!-----
      DO 510,II=1,NUN
  510 X(K,II)=XX(NUMMER(K),II)
      GG(NUMMER(K))=G(K)
!-----
      IF (ISOFIX(NUMMER(K)).NE.0.AND.K.LE.NUN) THEN
      IS=ISOFIX(NUMMER(K))
      LOCMIN(IS)=LOCMIN(IS)+1
      DO 512,II=1,NEND(IS)
  512 MINCOM(IS,LOCMIN(IS),II)=XEMFIX(NUMMER(K),II)
      END IF
!=====
      ELSE
      IS=EMCODE(K)
!CCC      IF (K.GT.NUN.AND.(GTOT.GT.GTEST.OR.TRY.EQ.1)) THEN
      IF (K.GT.NUN.AND.CLCOD.EQ.1) THEN
!----- kann wegen MINIX und MINIX1 geaendert werden
!!!!!! 14. feb. 2017 das seed zu entfernen scheint die rechenzeit etwas zu erhoehen   !!!!!
      IF (NUMMER(K).EQ.0.AND.ISTAB(K).NE.1.AND.SUGG(K).NE.MINISUG(IS) &
!      .AND.SUGCOD(K)(1:4).NE.'seed' &
      .AND.(ISTAB(K).EQ.0.OR.ISTAB(K).LT.(LOO1-20))) THEN
      I1=K
      I2=NMAX
      CALL COLCHG(I1,I2)
      IF (TEST.LT.0.0D0) CALL RKOFCHG(RKOF,I1,I2)
      NMAX=NMAX-1
      K=K-1
      ELSE
      I1=K
      CALL XSOL(I1)
      END IF
      ELSE
      I1=K
      CALL XSOL(I1)
      IF (K.LE.NUN) THEN
      LOCMIN(EMCODE(K))=LOCMIN(EMCODE(K))+1
      NPREV(EMCODE(K))=NPREV(EMCODE(K))+1
! HIER
      IF (NPREV(EMCODE(K)).GT.NPREVMAX) THEN
        NPREV(EMCODE(K))=NPREV(EMCODE(K))-1
        DO II=1,NPREV(EMCODE(K))-1
          DO J=1,NEND(EMCODE(K))
            PREV(EMCODE(K),II,J)=PREV(EMCODE(K),II+1,J)
          END DO
        END DO
      END IF
      DO J=1,NEND(EMCODE(K))
        PREV(EMCODE(K),NPREV(EMCODE(K)),J)=XEM(K,J)
      END DO
! HIER
      IF (SUGG(K).LT.SUGLOOP) THEN
      DXLAST(EMCODE(K),LOCMIN(EMCODE(K)))=DXMIN
      ELSE
      DXLAST(EMCODE(K),LOCMIN(EMCODE(K)))=DELXXX(K)
      END IF
      DO 514,II=1,NEND(EMCODE(K))
  514 MINCOM(EMCODE(K),LOCMIN(EMCODE(K)),II)=XEM(K,II)
      END IF
      END IF
      END IF
!=====
      K=K+1
      GO TO 25
    1 CONTINUE
!=====
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=6,FMT='('' NMAX= '',I9)') NMAX
      WRITE (UNIT=out,FMT='('' NMAX='',I9)') NMAX
      END IF
!==
      DO 520,I=1,NMAX
      IF (EMCODE(I).GT.0) THEN
      IS=EMCODE(I)
      DO 522,J=1,NEND(IS)
  522 XXSC(J)=XEM(I,J)
      CALL GNONID(IS,XXSC,GSC)
!==
!==
!----- check if G(I) are OK
      IF (TEST.LT.0.0D0) THEN
      IF (DABS(G(I)-GSC).GT.1D-10) THEN
      WRITE (6,1005) SUGG(I),I,SOLNAM(EMCODE(I)),G(I)-GSC
      WRITE (out,1005) SUGG(I),I,SOLNAM(EMCODE(I)),G(I)-GSC
 1005 FORMAT (/' after clean: DABS(G-GSC) > 1D-10 SUGG/ph:' &
      ,I6,I6,2X,A,2X,1PE15.8)
      END IF
      END IF
!==
!==
      G(I)=GSC
      ELSE
      G(I)=GG(NUMMER(I))
      END IF
!==
!==
!----- check if reactions are balanced???
!2---- 2: recalculate each phase with stored coefficients
      IF (TEST.LT.0.0D0) THEN
      IF (I.GT.NUN) THEN
      I001=0
      DO 610,IE=1,NUN
      FF(IE)=0.0D0
      DO 612,J=1,NUN
      FF(IE)=FF(IE)+RKOF(I,J)*X(J,IE)
  612 CONTINUE
      IF (DABS(FF(IE)-X(I,IE)).GT.1D-6) THEN
      I001=1
      END IF
  610 CONTINUE
      IF (I001.EQ.1) THEN
      WRITE (UNIT=6,FMT='('' reaction misfit > 1D-6:'')')
      WRITE (UNIT=out,FMT='('' reaction misfit > 1D-6:'')')
      WRITE (6,1010) (J,SUGG(J),EMCODE(J),J=1,NUN), &
      I,SUGG(I),EMCODE(I)
      WRITE (out,1010) (J,SUGG(J),EMCODE(J),J=1,NUN), &
      I,SUGG(I),EMCODE(I)
 1010 FORMAT (6X,100(I4,I6,I4))
      DO 614,IE=1,NUN
      WRITE (6,1012) NAME(IE),(X(J,IE),J=1,NUN),X(I,IE), &
      RKOF(I,IE),FF(IE),FF(IE)-X(I,IE)
      WRITE (out,1012) NAME(IE),(X(J,IE),J=1,NUN),X(I,IE), &
      RKOF(I,IE),FF(IE),FF(IE)-X(I,IE)
 1012 FORMAT (A6,100(2X,1PE12.5))
  614 CONTINUE
      END IF
      END IF
      END IF
!==
!==
  520 CONTINUE
!+++++
      RETURN
      END
!-----
!******************************
      SUBROUTINE ATEST(AFAIL)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 I,II,IS,I001,CODE,I1,ANZNEG
      LOGICAL*4 AFAIL,BFAIL(EMAX)
      REAL*8 AR001(EMAX),ACT(EMAX),XACT(EMAX),EXPO, &
      MUE(EMAX),MUE2(EMAX)
!-----
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=6,FMT='(/'' activity test'')')
      WRITE (UNIT=out,FMT='(/'' activity test'')')
      I1=NUN
      CALL PRTSTR(1,I1)
      END IF
!==
      AFAIL=.FALSE.
      DO 500,I=1,NUN
      CODE=0
      IF (EMCODE(I).GT.0) THEN
      IS=EMCODE(I)
      DO 502,I001=1,EMAX
  502 AR001(I001)=XEM(I,I001)
      CALL MUECAL(IS,AR001,MUE)
!-----
      DO 510,II=1,NEND(IS)
      BFAIL(II)=.FALSE.
      EXPO=-GG(EM(IS,II))/RT
      IF (EXPO.LT.-150) THEN
      ACT(II)=0.0D0
      ELSE
!ctest
!      IF (EXPO.GT.00) THEN
!      WRITE (UNIT=6,FMT='(''atest01: expo= '',1PE12.5)') EXPO
!      WRITE (UNIT=out,FMT='(''atest01: expo= '',1PE12.5)') EXPO
!      CODE=1
!      END IF
!ctest
      ACT(II)=DEXP(EXPO)
      END IF
      EXPO=(MUE(II)-GG(EM(IS,II)))/RT
      IF (EXPO.LT.-150) THEN
      XACT(II)=0.0D0
      ELSE
!ctest
!      IF (EXPO.GT.00) THEN
!      WRITE (UNIT=6,FMT='(''atest02: expo= '',1PE12.5)') EXPO
!      WRITE (UNIT=out,FMT='(''atest02: expo= '',1PE12.5)') EXPO
!      CODE=1
!      END IF
!ctest
      XACT(II)=DEXP(EXPO)
      END IF
      IF (DABS(ACT(II)-XACT(II)).GT.1D-4) AFAIL=.TRUE.
      IF (DABS(ACT(II)-XACT(II)).GT.1D-4) BFAIL(II)=.TRUE.
!-----
!CCCC      IF (AFAIL) RETURN
  510 CONTINUE
!==
      IF (TEST.LT.0.0D0.OR.CODE.EQ.1) THEN
      WRITE (6,1001) SOLNAM(IS),(AR001(II),II=1,NEND(IS))
      WRITE (out,1001) SOLNAM(IS),(AR001(II),II=1,NEND(IS))
 1001 FORMAT (/' x   : ',A16,100(2X,1PE12.5))
      WRITE (6,1000) SOLNAM(IS),(MUE(II),II=1,NEND(IS))
      WRITE (out,1000) SOLNAM(IS),(MUE(II),II=1,NEND(IS))
 1000 FORMAT (' mue : ',A16,100(2X,1PE12.5))
      CALL MUEOFG(IS,AR001,MUE2)
      WRITE (6,1002) SOLNAM(IS),(MUE2(II),II=1,NEND(IS))
      WRITE (out,1002) SOLNAM(IS),(MUE2(II),II=1,NEND(IS))
 1002 FORMAT (' G(i): ',A16,100(2X,1PE12.5))
      WRITE (6,1004) SOLNAM(IS),(ACT(II),II=1,NEND(IS))
      WRITE (out,1004) SOLNAM(IS),(ACT(II),II=1,NEND(IS))
 1004 FORMAT (' act : ',A16,100(2X,1PE12.5))
      WRITE (6,1006) SOLNAM(IS),(XACT(II),II=1,NEND(IS))
      WRITE (out,1006) SOLNAM(IS),(XACT(II),II=1,NEND(IS))
 1006 FORMAT (' xact: ',A16,100(2X,1PE12.5))
      WRITE (6,1008) SOLNAM(IS),(BFAIL(II),II=1,NEND(IS))
      WRITE (out,1008) SOLNAM(IS),(BFAIL(II),II=1,NEND(IS))
 1008 FORMAT (' fail: ',A16,100(2X,L4,8X))
      END IF
!==
      END IF
  500 CONTINUE
!-----
      ANZNEG=0
      DO 600,I=1,NMAX
      IF (SUGG(I).GT.0.AND.G(I).LT.-1D-9) ANZNEG=ANZNEG+1
  600 CONTINUE
!==
      IF (TEST.LT.0.0D0.OR.CODE.EQ.1) THEN
      WRITE (6,1010) ANZNEG
      WRITE (out,1010) ANZNEG
 1010 FORMAT (' phases with negative G: ',I4)
      END IF
!==
      IF (ANZNEG.GT.0) AFAIL=.TRUE.
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE MACHREA(IEX,IFOUND,DX0)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IEX,I,II,IS,N0,K,N,IFOUND,ERRC,IPOS,J,INUL, &
      IW10,I10POS,IREPOS,ZEROS
      REAL*8 ANULL(COMAX,COMAX),AEINS(COMAX,COMAX),CNULL(COMAX), &
      DX0(COMAX),ANEW(COMAX),NULWERT,RTEST,XW10(COMAX)
      CHARACTER*12 WARUM(COMAX)
      CHARACTER*16 TEXT(COMAX)
!only needed for GAUSSEL:
!      REAL*8 MAT(45,45)
!      LOGICAL*4 SING
!-----: replace phase so that all NN(1 to NUN) >= 0 ( NN/BLKSUM >-1D-14 )
!-----: never replace a phase with reaction coefficient (X(IEX,K)) < 1D-10
!-----: above avoids "something wrong with.." and some "reaction misfit"
!---- attention: aeins is transpose of anull
!---- if all reaction coefficients are <=0 then make SUGG of this phase negative
!==
      NULWERT=1D-14
    1 CONTINUE
      ZEROS=0
      IF (NULWERT.GT.1D-12) RETURN
!==
!---- make anull
      DO 500,K=1,NUN
      WRITE (UNIT=WARUM(K),FMT='(I3,'': not'')') K
      IF (EMCODE(K).GT.0) THEN
      IS=EMCODE(K)
      TEXT(K)=SOLNAM(IS)
!---- the following is XSOL
      DO 502,N=1,NUN
      ANULL(K,N)=0.0D0
      DO 502,I=1,NEND(IS)
      ANULL(K,N)=ANULL(K,N)+XEM(K,I)*XX(EM(IS,I),N)
  502 CONTINUE
      ELSE
      TEXT(K)=NAME(NUMMER(K))
      DO 504,II=1,NUN
  504 ANULL(K,II)=XX(NUMMER(K),II)
      END IF
  500 CONTINUE
!---- make anew
      IF (EMCODE(IEX).GT.0) THEN
      IS=EMCODE(IEX)
!---- the following is XSOL
      DO 510,N=1,NUN
      ANEW(N)=0.0D0
      DO 510,I=1,NEND(IS)
      ANEW(N)=ANEW(N)+XEM(IEX,I)*XX(EM(IS,I),N)
  510 CONTINUE
      ELSE
      DO 512,II=1,NUN
  512 ANEW(II)=XX(NUMMER(IEX),II)
      END IF
!---- end of XSOL
!---- make cnull
      DO 520,I=1,NUN
  520 CNULL(I)=BULK(I)
!=====
      N0=NUN
      IFOUND=0
      IW10=0
      I10POS=0
      DO 530,K=1,NUN
!----- skip phases with zero or negative reaction coefficients
!CCCC      IF (X(IEX,K).LT.1.0D-12) THEN
      IF (X(IEX,K).LT.1.0D-10) THEN
      WRITE (UNIT=WARUM(K),FMT='(I3,'': zero'')') K
      ZEROS=ZEROS+1
      GOTO 530
      END IF
!-----
!Cfeb07
      IF (NUMMER(IEX).GT.0.AND.NUMMER(IEX).LE.NUN &
      .AND.SUGG(K).GT.NUN) THEN
      WRITE (UNIT=WARUM(K),FMT='(I3,'': eles'')') K
      GOTO 530
      END IF
!-----
      DO 540,I=1,NUN
      IF (I.EQ.K) THEN
      DO 542,II=1,NUN
  542 AEINS(II,I)=ANEW(II)
      ELSE
      DO 544,II=1,NUN
  544 AEINS(II,I)=ANULL(I,II)
      END IF
  540 CONTINUE
!==
!      IF (TEST.LT.0.0D0) THEN
!      WRITE (scr,1000) (SUGG(I),I=1,NUN)
!      WRITE (out,1000) (SUGG(I),I=1,NUN)
! 1000 FORMAT (/,100(12X,I5))
!      DO 532,J=1,NUN
!      WRITE (scr,1002) (AEINS(I,J),I=1,NUN),CNULL(J)
!      WRITE (out,1002) (AEINS(I,J),I=1,NUN),CNULL(J)
! 1002 FORMAT (100(2X,1PE15.8))
!  532 CONTINUE
!      END IF
!==

!cc      RTEST=1.0D-7
      RTEST=0.0D0
      CALL LINCOMP(N0,AEINS,CNULL,DX0,RTEST,ERRC)
!--------------
! GAUSSEL and LINCOMP seem to give quite identical results
! decide later which one to use (7. Feb 2015)
! only needed for GAUSSEL:
!      WRITE (UNIT=scr,FMT='('' dx0LC'',I4,100(2X,1PE15.8))') &
!       K,(DX0(J),J=1,NUN)
!      COMAY=COMAX
!      SING=.FALSE.
!      ERRC=0
!      DO I=1,NUN
!       DO J=1,NUN
!        MAT(I,J)=AEINS(I,J)
!       END DO
!       MAT(I,NUN+1)=CNULL(I)
!      END DO

!!      CALL GAUSSEL(MAT,I,J,N0,DX0,SING)
!!      IF (SING) ERRC=1
!!      IF (SING) PRINT *, 'Matrix is (nearly) singular'
!      WRITE (UNIT=scr,FMT='('' dx0GA'',I4,100(2X,1PE15.8))') &
!       K,(DX0(J),J=1,NUN)
!--------------
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=scr,FMT='('' dx0'',I4,100(2X,1PE15.8))') &
       K,(DX0(J),J=1,NUN)
      WRITE (UNIT=out,FMT='('' dx0'',I4,100(2X,1PE15.8))') &
       K,(DX0(J),J=1,NUN)
      END IF
!==
      IF (ERRC.NE.0) WRITE (UNIT=WARUM(K),FMT='(I3,'':errc'')') K
!+0+0+0
      IF (ERRC.EQ.0) THEN
      IPOS=0
      IREPOS=0
      INUL=0
      DO 546,N=1,NUN
      IF (DX0(N)/BLKSUM.GE.-NULWERT) IPOS=IPOS+1
      IF (DX0(N).GE.0.0D0) IREPOS=IREPOS+1
      IF (DABS(DX0(N)/BLKSUM).LT.NULWERT) INUL=INUL+1
  546 CONTINUE
      WRITE (UNIT=WARUM(K),FMT='(I3,'':'',I2,''+'',I2,''z'',I2)') &
      K,IPOS,INUL,IREPOS
!-----
      IF (IPOS.EQ.N0) THEN
      IF (IREPOS.EQ.N0) THEN
      IFOUND=K
      GOTO 531
      ELSE
      IF (IREPOS.GT.I10POS) THEN
      IW10=K
      I10POS=IREPOS
      DO 547,J=1,NUN
  547 XW10(J)=DX0(J)
      END IF
      END IF
      END IF
      END IF
!+0+0+0
  530 CONTINUE
!-----
  531 CONTINUE
!=====
      IF (IFOUND.EQ.0.AND.IW10.GT.0) THEN
      IFOUND=IW10
      DO 556,J=1,NUN
  556 DX0(J)=XW10(J)
      END IF
!CCCC      IF (IFOUND.EQ.0) TEST=-DABS(TEST)
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (scr,1010) SUGG(IEX),(WARUM(I),I=1,NUN)
      WRITE (out,1010) SUGG(IEX),(WARUM(I),I=1,NUN)
 1010 FORMAT (' dG < 0 : ',I5,2X,11A12,100(/,12X,11A12))
      IF (IFOUND.NE.0) THEN
      WRITE (scr,1012) IEX,SUGG(IEX),IFOUND,SUGG(IFOUND), &
      (DX0(I),I=1,NUN)
      WRITE (out,1012) IEX,SUGG(IEX),IFOUND,SUGG(IFOUND), &
      (DX0(I),I=1,NUN)
 1012 FORMAT (' exchange col:',I4,' (sug',I5,') col',I4,' (sug',I5, &
      ') |',/,' new NN:',7(2X,1PE15.8),100(/8X,7(2X,1PE15.8)))
      WRITE (scr,1016) (TEXT(I),I=1,NUN)
      WRITE (out,1016) (TEXT(I),I=1,NUN)
 1016 FORMAT (' old ass:  ',7(1X,A16),100(/8X,7(1X,A16)))
      WRITE (scr,1017) (SUGG(I),I=1,NUN)
      WRITE (out,1017) (SUGG(I),I=1,NUN)
 1017 FORMAT (' old ass:  ',7(I7,10X),100(/8X,7(I7,10X)))
      ELSE
      WRITE (scr,1014) NULWERT
      WRITE (out,1014) NULWERT
 1014 FORMAT (' !!!!!!! danger: could not make reaction', &
      '   zero is:',2X,1PE12.5)
      END IF
      END IF
!==
      IF (ZEROS.EQ.NUN) THEN
      SUGG(IEX)=-SUGG(IEX)
!CC      WRITE (UNIT=scr,FMT='('' ZEROS='',I4)') ZEROS
!CC      WRITE (UNIT=out,FMT='('' ZEROS='',I4)') ZEROS
      RETURN
      END IF
!==
      IF (IFOUND.EQ.0) THEN
      NULWERT=NULWERT*10.0D0
      GOTO 1
      END IF
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE ADDPH(ADDCOD)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IS,I1,START,I001,I002,I,I2,J,II,ADDCOD,ANUMBER
      REAL*8 XXSC(EMAX),AR001(EMAX),FF,GMIN,RTA,SUMME
!      REAL*8 GSC
!
      GROSSG=0.0D0
!==
!---- add averages
!==
      ANUMBER=0
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=6,FMT='('' add average'')')
      WRITE (UNIT=out,FMT='('' add average'')')
      END IF
!--
      DO 400,I=1,NUN-1
      IF (EMCODE(I).GT.0) THEN
      IS=EMCODE(I)
      DO 405,II=I+1,NUN
      IF (EMCODE(II).GT.0) THEN
      I2=EMCODE(II)
      IF (IS.EQ.I2) THEN
      DO 410,J=1,NEND(IS)
  410 XXEM(J)=(XEM(I,J)+XEM(II,J))/2.0D0
!2      CALL GNONID(IS,XXSC,GSC)
!2      DO 415,J=1,NEND(IS)
!2  415 XXEM(J)=XXSC(J)
!2      GSOL=GSC
      DXEND=0.1D0
      SUGCOD(0)='avrg'
      CALL NEWPH(IS)
      END IF
      END IF
  405 CONTINUE
      END IF
  400 CONTINUE
!==
!---- END add averages
!==
!---
      DISTAMAX=0.0D0
      SUGLOOP=SUGNR+1
      DO 500,IS=1,NSOL
      IF (SOJUMP(IS).EQ.1) GOTO 888
      IF (SOSKIP(IS).NE.0) GOTO 888
      I001=IS
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=6,FMT='(80A1)') ('-',I=1,80)
      WRITE (UNIT=out,FMT='(80A1)') ('-',I=1,80)
      WRITE (scr,1000) SOLNAM(IS),LOCMIN(IS),LOMIN1(IS), &
      STEM(IS),ADDCOD
      WRITE (out,1000) SOLNAM(IS),LOCMIN(IS),LOMIN1(IS), &
      STEM(IS),ADDCOD
 1000 FORMAT (1X,A12,2X,' locmin =',I3,4X,'lomin1 =',I3,4X, &
      'stem =',I3,8X,'ADDCOD =',I3)
      END IF
!==
!----- use tracing from last loop
!==
      DO I=1,NEND(IS)
        IF (GTRAC(IS,I).LT.1D30) THEN
          DO J=1,NEND(IS)
            XXSC(J)=TRAC(IS,I,J)
          END DO
        I002=888
        ANUMBER=I
!
        IF (TEST.LT.0.0D0) THEN
        WRITE (UNIT=scr,FMT='(/,'' TRAC: '',A16,2X,A)') &
        SOLNAM(IS),NAME(EM(IS,I))
        WRITE (UNIT=out,FMT='(/,'' TRAC: '',A16,2X,A)') &
        SOLNAM(IS),NAME(EM(IS,I))
        END IF
!       I001=IS, I002=888,anumber=em of TRAC
        CALL MARMIN(I001,I002,ANUMBER,XXSC)
        END IF
      END DO
      ANUMBER=0
!==
!+++  if non-ideal and previously stable i1=0 (start with solution scan)
!+++  if non-ideal and not previously stable i1=-NEND(IS) (start with endmenbers)
      IF (NMARG(IS).GT.0.OR.NSMARG(IS).GT.0.OR.MODELL(IS).NE.'I') THEN
        IF (STEM(IS).EQ.0) THEN
          IF (LOMIN1(IS).EQ.LOCMIN(IS)) THEN
          I1=0
        ELSE
          I1=-NEND(IS)
        END IF
      END IF
!----
!---- note: do not get near the structural endmembers!!!
!     i2=LOCMIN(IS) end = use all LOCMINS
      I2=LOCMIN(IS)
!--
!Cmar2007      IF (LOCMIN(IS).GE.1.AND.LOMIN1(IS).GE.1) I1=1
!---- this slows down, but adds robustness
!---- in future: maybe check assemblage. if same sassemblage for
!---- two (three?) loops e.g. i1=1
      IF (STEM(IS).EQ.0.AND.LOCMIN(IS).EQ.0.AND.LOMIN1(IS).EQ.0) THEN
        I1=0
        IF (LOO1.GT.2) I2=1
        IF (LOO1.GT.3) I1=1
      END IF
!
!      IF (ADDCOD.EQ.1) I1=-NEND(IS)
!---- above line obsolete with TRAC ???
!cdcFeb11
!      IF (I1.GT.0) I1=0
!      IF (I2.LT.0) I2=0      
!cdcFeb11 do not start with em, but only scan
!cdcFeb11 (OK, but not so good at finding exsolutions)
!===
      IF (LOO1.LT.5) I1=-NEND(IS)
!===
!===
! with PREV no need to use I2>0
      I1=-NEND(IS)
      I2=0
!===
!===
!********
      DO 510,START=I1,I2
      I002=START
      CALL MARMIN(I001,I002,ANUMBER,XXSC)
!********
  510 CONTINUE
!===
!----- use initial guess from 'SEED'
!===
!      DO I=1,NEND(IS)
!       XXSC(I)=0.0D0
!      END DO
!
      IF (NSED.GT.0.AND.LOO1.LT.5) THEN
       DO I=1,NSED
        IF (SIS(I).EQ.IS) THEN
         DO J=1,NEND(IS)
          XXSC(J)=SXX(I,J)
         END DO
         I002=111
         CALL MARMIN(I001,I002,ANUMBER,XXSC)
        END IF
       END DO
      END IF
!===
!----- from collecion of stable phases...
!===
      IF (NPREV(IS).GT.0) THEN
       DO I=1,NPREV(IS)
        DO J=1,NEND(IS)
         XXSC(J)=PREV(IS,I,J)
        END DO
        I002=666
!
        IF (TEST.LT.0.0D0) THEN
         WRITE (UNIT=scr,FMT='(/,'' PREV: '',A16,2X,''nr. '',I4)') &
         SOLNAM(IS),I
         WRITE (UNIT=out,FMT='(/,'' PREV: '',A16,2X,''nr. '',I4)') &
         SOLNAM(IS),I
        END IF
!
        CALL MARMIN(I001,I002,ANUMBER,XXSC)
       END DO
      END IF
!===
!----- as ideal solution...
!===
      RTA=RT*ALPHA(IS)
      GMIN=GG(EM(IS,1))
      DO 501,I=2,NEND(IS)
  501 GMIN=DMIN1(GMIN,GG(EM(IS,I)))
      SUMME=0.0D0
      DO 502,I=1,NEND(IS)
      AR001(I)=-(GG(EM(IS,I))-GMIN)
      XXSC(I)=1D0/DEXP(DMIN1(150D0,(GG(EM(IS,I))-GMIN)/RTA))
  502 SUMME=SUMME+XXSC(I)
      FF=DLOG(SUMME)*RTA
      DO 503,I=1,NEND(IS)
      AR001(I)=AR001(I)-FF
  503 XXSC(I)=XXSC(I)/SUMME

      I002=222
      CALL MARMIN(I001,I002,ANUMBER,XXSC)
!===
!+++
!     else is ideal solution
      ELSE
        CALL MING(I001)
      END IF
!+++
  888 CONTINUE
!==
!==
!==
      DO J=1,NMAX
      IF (SUGG(J).EQ.MINISUG(IS)) THEN
       SUGUSE(J)=2
      END IF
      END DO
!==
! delete everthing with SUGUSE =0 (and =1) 
! IS NOT A GOOD IDEA
!      J=NUN+1
!  810 IF (J.GT.NMAX) GOTO 811
!      IF (SUGUSE(I).LT.2) THEN
!        I1=J
!        I2=NMAX
!        CALL COLCHG(I1,I2)
!!!!!!!        IF (TEST.LT.0.0D0) CALL RKOFCHG(RKOF,I1,I2)
!        NMAX=NMAX-1
!        J=J-1
!      END IF
!      J=J+1
!      GOTO 810
!  811 CONTINUE
!==
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=6,FMT='('' minisug = '',i7)') MINISUG(IS)
      WRITE (UNIT=out,FMT='('' minisug = '',i7)') MINISUG(IS)
      END IF
!==
  500 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE NEWPH(IS)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IS,I,II,CODE,N,PROB,PR01,I001
      REAL*8 XXSC(EMAX),GSC,FF,MUE2(EMAX),AR001(EMAX),AR002(EMAX)
!-----
      PROB=0
!+++++ below added for security
      FF=0.0D0
      DO 400,I=1,NEND(IS)
  400 FF=FF+XXEM(I)
      IF (DABS(FF-1.0D0).GT.1D-12) THEN
        PROB=PROB+1
!==
        IF (TEST.LT.0.0D0) THEN
          WRITE (6,1000) SUGNR+1,FF-1.0D0
          WRITE (out,1000) SUGNR+1,FF-1.0D0
 1000     FORMAT (' sum not 1: ',I6,2X,1PE12.5)
        END IF
!==
      END IF
      DO 410,I=1,NEND(IS)
  410 XXSC(I)=XXEM(I)
!==
      CODE=1
      CALL SPACETEST(IS,XXSC,CODE)
      IF (CODE.EQ.0) THEN
        PROB=PROB+1
!==
        IF (TEST.LT.0.0D0) THEN
          WRITE (6,1002) SUGNR+1,SOLNAM(IS),SUGCOD(0)
          WRITE (out,1002) SUGNR+1,SOLNAM(IS),SUGCOD(0)
 1002     FORMAT (' spacetest failed: ',I6,2X,A,2X,A)
        END IF
!==
      END IF
!====
      CALL MUECAL(IS,XXSC,MUE2)
      PR01=0
      DO I=1,NEND(IS)
        IF(MUE2(I).LT.-1D19) PR01=PR01+1
      END DO
      IF (PR01.NE.0) THEN
        IF (TEST.LT.0.0D0) THEN
          WRITE (scr,1004) SUGNR+1,SOLNAM(IS)
          WRITE (out,1002) SUGNR+1,SOLNAM(IS)
 1004     FORMAT (' muetest failed: ',I6,2X,A)
        END IF
        PROB=PROB+1
      END IF
!
      IF (PROB.NE.0) RETURN
!====
!! here: check if colposition is similar to already added phase
!      DO II=NUN+1,NMAX
!       IF (EMCODE(II).EQ.IS) THEN
!        DO I=1,NEND(IS)
!         AR001=XEM(II,I)
!         AR002=XXEM(I)
!        END DO
!        I001=IS
!        CALL DISTAN(I001,AR001,AR002)
!!      WRITE (6,1020) EMCODE(II),SUGG(II),SUGNR+1,DISTA
!! 1020 FORMAT (' emcode = ',i7,'  sugg = ',2i7,5x,'dist = ',1PE12.5)
!       IF (DISTA.LT.1D-15) RETURN
!       END IF
!      END DO
!---
      CALL GNONID(IS,XXSC,GSC)
      GSOL=GSC
!+++++ above replaces calls before NEWPH
!DC
      NMAX=NMAX+1
      SUGNR=SUGNR+1
      IF (NMAX.GT.CALMAX) THEN
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=2000) CALMAX
      WRITE (UNIT=out,FMT=2000) CALMAX
 2000 FORMAT (// &
      ' The maximum number of phases considered for minimization', &
      ' is set to: ',I5/ &
      ' The addition of a phase in NEWPH results in a larger number'/ &
      ' possible actions:'/ &
      '   a) Increase the value of CALMAX in the parameter statements'/ &
      '   b) Check the stabilities in smaller subsystems and then'/ &
      '      exclude (or delete) the unstable phases.'/ &
      '   c) Keep fewer solution phases in memory. Change program in'/ &
      '      subroutine CLEAN, line:'/ &
      '      ...... (ISTAB(K).EQ.0.OR.ISTAB(K).LT.(LOO1-5))) .... '/ &
      '   d) Add fewer solution phases in each loop. Change program'/ &
      '      in subroutines MING and MARMIN: skip some of the'/ &
      '      CALL NEWPH(IS), but not the first in each subroutine')
      STOP
      END IF
!-----
      IF (GROSSG.GT.GSOL) GROSSG=GSOL
!-----
      NN(NMAX)=0.0D0
      G(NMAX)=GSOL
      NUMMER(NMAX)=0
      EMCODE(NMAX)=IS
      DELXXX(NMAX)=DXEND
      DO 500,I=1,NEND(IS)
  500 XEM(NMAX,I)=XXEM(I)
      SUGG(NMAX)=SUGNR
      SUGCOD(NMAX)=SUGCOD(0)
      ISTAB(NMAX)=0
      SUGUSE(NMAX)=0
      IF (PROB.EQ.0) SUGUSE(NMAX)=1
!-----
      IF (EXSOL(IS)) SUGG(NMAX)=-SUGNR
!-----
!---- the following is XSOL
      DO 502,N=1,NUN
      X(NMAX,N)=0.0D0
      DO 502,I=1,NEND(IS)
      X(NMAX,N)=X(NMAX,N)+XEM(NMAX,I)*XX(EM(IS,I),N)
  502 CONTINUE
!---- end XSOL
      IF (GSOL.LT.MINIG(IS)) THEN
      MINIG(IS)=GSOL
!      MINISUG(IS)=SUGNR
      MINISUG(IS)=SUGG(NMAX)
      DO 504,I=1,NEND(IS)
  504 MINIX(IS,I)=XXEM(I)
      END IF
      I=NMAX
      IF (TEST.LT.0.0D0) CALL PRTSTR(NMAX,I)
      IF (TEST.LT.0.0D0) CALL PRTAAA(NMAX,I)
      RETURN
      END
!-----
!******************************
      SUBROUTINE XSOL(K)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 I,K,N
!-----
      DO 501,N=1,NUN
      X(K,N)=0.0D0
      DO 501,I=1,NEND(EMCODE(K))
      X(K,N)=X(K,N)+XEM(K,I)*XX(EM(EMCODE(K),I),N)
  501 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE MING(IS)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IS,I,II
      REAL*8 GMIN,SUMME,RTA, &
      F001,AR001(EMAX),FN,FX,FF,XXSC(EMAX),AR003(EMAX)
!-----
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=6,FMT='('' MING'',2X,A16)') SOLNAM(IS)
      WRITE (UNIT=6,FMT='('' ----'')')
      WRITE (UNIT=out,FMT='('' MING'',2X,A16)') SOLNAM(IS)
      WRITE (UNIT=out,FMT='('' ----'')')
      END IF
!-----
      RTA=RT*ALPHA(IS)
      GMIN=GG(EM(IS,1))
      DO 501,I=2,NEND(IS)
  501 GMIN=DMIN1(GMIN,GG(EM(IS,I)))
      SUMME=0.0D0
      DO 502,I=1,NEND(IS)
      AR001(I)=-(GG(EM(IS,I))-GMIN)
      XXEM(I)=1D0/DEXP(DMIN1(150D0,(GG(EM(IS,I))-GMIN)/RTA))
  502 SUMME=SUMME+XXEM(I)
      FF=DLOG(SUMME)*RTA
      DO 503,I=1,NEND(IS)
      AR001(I)=AR001(I)-FF
  503 XXEM(I)=XXEM(I)/SUMME
      GSOL=0.0D0
      DO 504,I=1,NEND(IS)
  504 GSOL=GSOL+XXEM(I)*(GG(EM(IS,I))+AR001(I))
! 504 GSOL=GSOL+XXEM(I)*GG(EM(IS,I))+RTA*XXEM(I)*DLOG(XXEM(I))
!-----
      IF (LOCMIN(IS).NE.0) THEN
      DXEND=1D20
      DO 600,II=1,LOCMIN(IS)
      DO 610,I=1,NEND(IS)
      AR003(I)=XXEM(I)
  610 AR001(I)=MINCOM(IS,II,I)
      CALL DISTAN(IS,AR003,AR001)
      IF (DISTA.GT.DISTAMAX.AND.GSOL.LT.0.0D0) DISTAMAX=DISTA
  600 DXEND=DMIN1(DXEND,DISTA)
      ELSE
      DXEND=0.5D0
      END IF
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=6,FMT='('' add minimum'')')
      WRITE (UNIT=out,FMT='('' add minimum'')')
      END IF
!==
      SUGCOD(0)='idmn'
      CALL NEWPH(IS)
!-----
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=6,FMT='('' add scan'')')
      WRITE (UNIT=out,FMT='('' add scan'')')
      END IF
!==
      FN=DBLE(NEND(IS))
      FX=DSQRT((FN-1.0D0)/FN)
      FF=-DSQRT(1.0D0/(FN*(FN-1.0D0)))
      DO 650,I=1,NEND(IS)
      AR001(I)=XXEM(I)
  650 VEKTOR(I)=FF
      DO 700,II=1,NEND(IS)
      IF (II.NE.1) VEKTOR(II-1)=FF
      VEKTOR(II)=FX
      F001=DXEND/2.0D0
      CALL VECADD(IS,AR001,F001,XXSC)
      DO 710,I=1,NEND(IS)
  710 XXEM(I)=XXSC(I)
      SUGCOD(0)='idsc'
      CALL NEWPH(IS)
  700 CONTINUE
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE MARMIN(IS,START,ANUMBER,XXSC)
!-----called by ADDPH. IS = solution phase ID-number.
!-----START < -NEND : Initial guess is composition of STRUCT endmember(-START+NEND)
!-----START < 0 : Initial guess is composition of endmember(-START)
!-----START = 0 : solution is scanned (grid = DXSCAN)
!-----            composition of smallest value found is initial guess
!-----START > 0 : Initial guess is a previously found stable phase.
!-----START = 666:Initial gess from previously stable phases
!-----START = 888:Initial gess from initials produced in last loop (TRAC)
!-----START = 111:Initial gess from 'SEED'
!-----START = 222:Initial gess from ideal solution
!-----START = 777:reserved for test !!!
!-----
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 I,IS,START,STEP,STEP2,NSTEP,L3,L1,II,I001,I002, &
      I1,FRAC,COMBO(10),IC,BESTN,BESTS,ANUMBER,IPM,VERSUCH, &
      GNOMSOFAR,DOMEM
      REAL*8 XXSC(EMAX),GSC,XXX(3,EMAX),GGG(3),AR001(EMAX), &
      AR002(EMAX),SUMME,F001,FF,LESCAN,MUE(EMAX), &
      AR003(EMAX),REFX(EMAX),REFG,BESTG,REFG0,PLUMI,DDX,GAIN
!      CHARACTER*133 CH001
      CHARACTER*500 CH001
      CHARACTER*6 MINKOM
!==
      BESTG=1D30
      BESTN=0
      BESTS=0
      VERSUCH=1
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=6,FMT='('' ------------'')')
      WRITE (UNIT=out,FMT='('' ------------'')')
      WRITE (scr,401) START,SOLNAM(IS),ANUMBER,TC,P
      WRITE (out,401) START,SOLNAM(IS),ANUMBER,TC,P
  401 FORMAT (' START = ',I4,2X,A16,2X,' nr = ',I6,4X, &
      'T = ',F10.4,'  P = ',F10.2)
      WRITE (UNIT=6,FMT='('' ------------'')')
      WRITE (UNIT=out,FMT='('' ------------'')')
      END IF
!-----SEEDS
      IF (START.EQ.111) THEN
       CALL GNONID(IS,XXSC,GSC)
       DO I001=1,NEND(IS)
        XXEM(I001)=XXSC(I001)
       END DO
       GSOL=GSC
       DELTAX=DXSTAR
       NSTEP=STPSTA
       SUGCOD(0)='see2'
       GOTO 22
      END IF
!-----ideal
      IF (START.EQ.222) THEN
       CALL GNONID(IS,XXSC,GSC)
       DO I001=1,NEND(IS)
        XXEM(I001)=XXSC(I001)
       END DO
       GSOL=GSC
       DELTAX=DXSTAR
       NSTEP=STPSTA
       SUGCOD(0)='idea'
       GOTO 22
      END IF
!-----previously stable phases
      IF (START.EQ.666) THEN
       CALL GNONID(IS,XXSC,GSC)
       DO I001=1,NEND(IS)
        XXEM(I001)=XXSC(I001)
       END DO
       GSOL=GSC
       DELTAX=DXSTAR
       NSTEP=STPSTA
       SUGCOD(0)='prev'
       GOTO 22
      END IF
!-----initials produced in last loop (TRAC)
      IF (START.EQ.888) THEN
       CALL GNONID(IS,XXSC,GSC)
       DO I001=1,NEND(IS)
        XXEM(I001)=XXSC(I001)
       END DO
       GSOL=GSC
       GTRAC(IS,ANUMBER)=GSOL
       DELTAX=DXSTAR
       NSTEP=STPSTA
       SUGCOD(0)='last'
       GOTO 22
      END IF
!-----scan normal solutions
      IF (START.EQ.0.AND.NNEGEM(IS).EQ.0) THEN
!dC
      LESCAN=DXSCAN
      IF (TRY.GT.1.AND.GTOT.LE.GTEST) LESCAN=DXSCAN/4.0D0
!dC
      DO 501,I=1,(NEND(IS)-1)
  501 XXSC(I)=0.0D0
      XXSC(NEND(IS))=1.0D0
      GSOL=GG(EM(IS,NEND(IS)))
      DO 502,I001=1,NEND(IS)
  502 XXEM(I001)=XXSC(I001)
      SUMME=0.0D0
 1001 XXSC(NEND(IS)-1)=XXSC(NEND(IS)-1)+LESCAN
      SUMME=SUMME+LESCAN
      XXSC(NEND(IS))=1.0D0-SUMME
      DO 503,I=(NEND(IS)-1),2,-1
      IF (XXSC(NEND(IS)).GE.0.0D0) GO TO 1
      XXSC(I-1)=XXSC(I-1)+LESCAN
      SUMME=SUMME+LESCAN
      DO 504,II=I,(NEND(IS)-1)
      SUMME=SUMME-XXSC(I)
  504 XXSC(I)=0.0D0
  503 XXSC(NEND(IS))=1.0D0-SUMME
    1 IF (XXSC(NEND(IS)).LT.0.0D0) THEN
      GO TO 10
      ELSE
      CALL GNONID(IS,XXSC,GSC)
      IF (GSC.LT.GSOL) THEN
      DO 505,I001=1,NEND(IS)
  505 XXEM(I001)=XXSC(I001)
      GSOL=GSC
      END IF
      END IF
      GO TO 1001
   10 DELTAX=DXSTAR
      NSTEP=STPSTA
      SUGCOD(0)='scan'
      END IF
!
!-----half scan for weird solutions
      GSOL=1D30
      IF (START.EQ.0.AND.NNEGEM(IS).GT.0.AND.WSCAN.GT.0) THEN
!
       FRAC=WSCAN
       DO I=1,FRAC-1
        COMBO(I)=1
       END DO
       COMBO(FRAC)=0
       IC=FRAC
   90  IF (IC.LT.1) GOTO 91
       IF (COMBO(IC).EQ.NALLEM(IS)) THEN
        IC=IC-1
        GOTO 90
       END IF
       COMBO(IC)=COMBO(IC)+1
       DO I=IC+1,FRAC
        COMBO(I)=COMBO(IC)
       END DO
!
       DO I=1,NEND(IS)
        XXSC(I)=0.0D0
       END DO
       DO I1=1,FRAC
        DO I=1,NEND(IS)
         XXSC(I)=XXSC(I)+ALLEM(IS,COMBO(I1),I)/DBLE(FRAC)
        END DO
       END DO
       SUMME=1.0D0+DBLE(NEND(IS))*1D-3
       DO I=1,NEND(IS)
        XXSC(I)=XXSC(I)+1D-3
        XXSC(I)=XXSC(I)/SUMME
       END DO
       CALL GNONID(IS,XXSC,GSC)
       IF (GSC.LT.GSOL) THEN
        DO I001=1,NEND(IS)
         XXEM(I001)=XXSC(I001)
        END DO
        GSOL=GSC
       END IF
       IC=FRAC
       GOTO 90
!
   91  DELTAX=DXSTAR
       NSTEP=STPSTA
       SUGCOD(0)='scaw'
      END IF
!-----endmembers
      IF (START.LT.0.AND.START.GE.-NEND(IS)) THEN
      II=-START
      DO 701,I=1,NEND(IS)
  701 XXEM(I)=0.0D0
      XXEM(II)=1.0D0
      GSOL=GG(EM(IS,II))
      DELTAX=DXSTAR
      NSTEP=STPSTA
      SUGCOD(0)='endm'
      END IF
!-----structural endmembers (not used at the moment?? (only with +1d-2)
      IF (START.LT.0.AND.START.LT.-NEND(IS)) THEN
      II=-START-NEND(IS)
      SUMME=1.0D0+DBLE(NEND(IS))*1D-2
      DO 702,I=1,NEND(IS)
      XXSC(I)=STREM(IS,II,I)+1D-2
      XXSC(I)=XXSC(I)/SUMME
      XXEM(I)=STREM(IS,II,I)+1D-2
      XXEM(I)=XXSC(I)/SUMME
  702 CONTINUE
      CALL GNONID(IS,XXSC,GSC)
      GSOL=GSC
      DELTAX=DXSTAR
      NSTEP=STPSTA
      SUGCOD(0)='stre'
      END IF
!-----previous stable phases (not used at the moment)
!-----replaced by PREV
      IF (START.GT.0.AND.LOCMIN(IS).GT.0) THEN
      DO 506,I=1,NEND(IS)
  506 XXEM(I)=MINCOM(IS,START,I)
      GSOL=0.0D0
!dc      DELTAX=DXLAST(IS,START)
      DELTAX=DXSTAR
      NSTEP=STPMAX
      SUGCOD(0)='stab'
      END IF
!-----previous unstable phases (at minimum G)
      IF (START.EQ.1.AND.LOCMIN(IS).EQ.0) THEN
      FF=0.0D0
      DO 507,I=1,NEND(IS)
      FF=FF+MINIX1(IS,I)
      XXSC(I)=MINIX1(IS,I)
  507 XXEM(I)=MINIX1(IS,I)
      IF (FF.EQ.0.0D0) RETURN
      CALL GNONID(IS,XXSC,GSC)
      GSOL=GSC
!dc      DELTAX=DXLAST(IS,START)
      DELTAX=DXSTAR
      NSTEP=STPMAX
      SUGCOD(0)='usta'
      END IF
!
!     XXEM(i): starting values (global)
!     GSOL: G for XXEM(i) (global)
!     NSTEP: number of steps for Gauchy
!     DELTAX: starting value of stepsize
!
   22 CONTINUE
!
      MINKOM='normal'
      DO 513,I=1,NEND(IS)
  513 XXX(1,I)=XXEM(I)
      GGG(1)=GSOL
      REFG0=GSOL
!-----
      IF (TEST.LT.0.0D0) THEN
      WRITE (scr,101) REFG0,LOO1
      WRITE (out,101) REFG0,LOO1
  101 FORMAT (' START:      G = ',1PE12.5,4X,'loop = ',I6)
      WRITE (UNIT=CH001,FMT=100) DELTAX
  100 FORMAT (' START: DELTAX = ',1PE12.5)
      DO 512,I001=1,NEND(IS),10
      DO 511,I=I001,MIN0(I001+9,NEND(IS))
      I002=42+MOD(I-1,10)*10
  511 WRITE (UNIT=CH001(I002:),FMT='(F10.6)') XXEM(I)
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
  512 CH001=' '
      END IF
      GNOMSOFAR=GNOM
!-----
!=====Gradient search for minimum. (Cauchy with anti-zig-zag)
!-----step 1 and 2
      L1=1
      IF (TRY.GT.1.AND.GTOT.LE.GTEST) NSTEP=1500
      DO 516,STEP=1,MIN0(2,NSTEP)
      DO 514,I001=1,NEND(IS)
  514 AR002(I001)=XXX(STEP,I001)
      CALL MUECAL(IS,AR002,MUE)
      CALL STEEP(IS,AR002,MUE)
      L1=STEP+1
      CALL ETC(IS,AR002,GGG(STEP),AR001,GGG(L1))
      DO 515,I001=1,NEND(IS)
  515 XXX(L1,I001)=AR001(I001)
  516 CONTINUE
!-----step 3 to n
      DO 525,STEP=3,NSTEP
      IF (DELTAX.LE.DXMIN) GO TO 15
      L3=MOD(STEP-1,3)+1
      L1=MOD(STEP,3)+1
      DO 517,I001=1,NEND(IS)
      AR001(I001)=XXX(L1,I001)
  517 AR002(I001)=XXX(L3,I001)
      CALL MUECAL(IS,AR002,MUE)
      GSC=0.0D0
      DO 518,I=1,NEND(IS)
  518 GSC=GSC+MUE(I)*XXX(L1,I)
      CALL DISTAN(IS,AR001,AR002)
      DELTAX=DISTA
      IF (DELTAX.GT.DXMIN) THEN
      IF (GSC.LT.GGG(L3)) THEN
      CALL STEEP(IS,AR002,MUE)
      DELTAX=DELTAX/2.0D0
      ELSE
      SUMME=0.0D0
      DO 519,I=1,NEND(IS)
      VEKTOR(I)=XXX(L3,I)-XXX(L1,I)
  519 SUMME=SUMME+VEKTOR(I)*VEKTOR(I)
      SUMME=DSQRT(SUMME)
      IF (SUMME.GT.0.0D0) THEN
      DO 520,I=1,NEND(IS)
  520 VEKTOR(I)=VEKTOR(I)/SUMME
      END IF
      END IF
      CALL ETC(IS,AR002,GGG(L3),AR001,GGG(L1))
      DO 521,I001=1,NEND(IS)
  521 XXX(L1,I001)=AR001(I001)
      END IF
  525 CONTINUE
!=====
!
!     XXEM(i): still starting values (global)
!     GSOL: still G for XXEM(i) (global)
!     DELTAX: final value of stepsize
!     AR001(i): final xem after Gauchy
!
!-----direction of endmembers, stepsize=0.001
      DO I001=1,NEND(IS)
        AR003(I001)=XXEM(I001)
      END DO
   15 CALL DISTAN(IS,AR001,AR003)
!
!!      GOTO 876
!     check if em is zero
      DO I001=1,NEND(IS)
       IF (XXEM(I001).GT.0.0D0.AND.DABS(AR001(I001)).LT.1D-9) THEN
        ZEND(IS,I001)=ZEND(IS,I001)+1
!
        IF (TEST.LT.0.0D0) THEN
        WRITE (scr,120) NAME(EM(IS,I001)),XXEM(I001),AR001(I001)
        WRITE (out,120) NAME(EM(IS,I001)),XXEM(I001),AR001(I001)
  120 FORMAT(' ----- ',A,' is 0 ',2(2X,1PE11.4))
        END IF
!
       END IF

       IF (ZEND(IS,I001).GT.0) THEN
       IF (XXEM(I001).GT.0.0D0.AND.DABS(AR001(I001)).GT.1D-9) THEN
        ZEND(IS,I001)=0
!
        IF (TEST.LT.0.0D0) THEN
        WRITE (scr,121) NAME(EM(IS,I001)),XXEM(I001),AR001(I001)
        WRITE (out,121) NAME(EM(IS,I001)),XXEM(I001),AR001(I001)
  121 FORMAT(' ----- ',A,' is not 0 ',2(2X,1PE11.4))
        END IF
!
       END IF
       END IF

      END DO
!
!!  876 CONTINUE


      STEP2=0
      IF (DISTA.LT.DXMIN) THEN
      STEP=2-STEP
      DO 526,I=1,NEND(IS)
  526 VEKTOR(I)=-AR001(I)
      DELTAX=DXSTAR
      F001=0.001D0
      DO 530,STEP2=1,NEND(IS)
      IF (STEP2.NE.1) VEKTOR(STEP2-1)=-AR001(STEP2-1)
      VEKTOR(STEP2)=1.0D0-AR001(STEP2)
      CALL VECADD(IS,AR001,F001,XXSC)
      CALL GNONID(IS,XXSC,GSC)
      IF (GSC.LT.GGG(L1)) THEN
      MINKOM='endm.'
      DELTAX=F001
      CALL ETC(IS,XXSC,GSC,AR001,GGG(L1))
      END IF
  530 CONTINUE
!=====
!-----direction of structural endmembers, stepsize=0.001
      DO 570,II=1,NSTREM(IS)
      F001=0.001D0
      DO 571,I=1,NEND(IS)
  571 VEKTOR(I)=STREM(IS,II,I)-AR001(I)
      CALL VECADD(IS,AR001,F001,XXSC)
      CALL GNONID(IS,XXSC,GSC)
      IF (GSC.LT.GGG(L1)) THEN
      MINKOM='str.em'
      DELTAX=F001
      CALL ETC(IS,XXSC,GSC,AR001,GGG(L1))
      END IF
  570 CONTINUE
!--   (end if dista.lt.dxmin)
      END IF
!=====
!
!     XXEM(i): still starting values (global)
!     GSOL: still G for XXEM(i) (global)
!     AR001(i): final xem after directional searches
!
!-----make vektor = end-start
      DO I001=1,NEND(IS)
        AR003(I001)=XXEM(I001)
      END DO
      CALL DISTAN(IS,AR001,AR003)
      IF (DISTA.GT.DISTAMAX.AND.GGG(L1).LT.0.0D0.AND.START.GT.0) &
       DISTAMAX=DISTA
      DXEND=DMAX1(DISTA,DXMIN)
      DO 531,I=1,NEND(IS)
      VEKTOR(I)=AR001(I)-XXEM(I)
  531 XXEM(I)=AR001(I)
      GSOL=GGG(L1)
!
!     AR001(i): final xem after directional searches
!     XXEM(i): also final xem (global) = AR001(i)
!     GSOL: G for XXEM(i) (global)
!
!-----
      IF (TEST.LT.0.0D0) THEN
      I001=STEP-1
      WRITE (UNIT=CH001,FMT=105) SOLNAM(IS),MINKOM,I001
  105 FORMAT (1X,A16,2X,A6,2X,'STEP =',I4)
      DO 533,I001=1,NEND(IS),10
      DO 532,I=I001,MIN0(I001+9,NEND(IS))
      I002=42+MOD(I-1,10)*10
  532 WRITE (UNIT=CH001(I002:),FMT='(F10.6)') XXEM(I)
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
  533 CH001=' '
      CH001=' '
!-----
      WRITE (UNIT=scr,FMT=106) DELTAX,DISTA,GNOM,GNOM-GNOMSOFAR
      WRITE (UNIT=out,FMT=106) DELTAX,DISTA,GNOM,GNOM-GNOMSOFAR
  106 FORMAT (8X,'DELTAX = ',1PE12.5,4X,'DISTA = ',1PE12.5, &
      4X,'GNOM = ',I8,4X,'GCALC = ',I8)
      END IF
      GNOMSOFAR=GNOM
!==
      IF (MINKOM.EQ.'normal') SUGCOD(0)(6:9)='norm'
      IF (MINKOM.EQ.'endm.') SUGCOD(0)(6:9)='endm'
      IF (MINKOM.EQ.'str.em') SUGCOD(0)(6:9)='dirs'
!==
!== minimum is in AR001. for newph: XXEM=AR001
!== G of minimum is in GSOL for newph
!== XXEM and GSOL are global
!==
      IF (TEST.LT.0.0D0) THEN
        WRITE (scr,536) DISTA,GSOL-REFG0,GNOM-GNOMSOFAR
        WRITE (out,536) DISTA,GSOL-REFG0,GNOM-GNOMSOFAR
  536 FORMAT (' delta x :',1PE12.5,'  delta G: ',1PE12.5, &
      4X,'GCALC = ',I8)
      END IF
      DO I001=1,NEND(IS)
        REFX(I001)=XXEM(I001)
      END DO
      REFG=GSOL
!+++++
!+++++ if DISTA = 0 and start = em, try again 
!!----- DDX away from endmember
      IF (DISTA.EQ.0.0D0.AND.VERSUCH.EQ.1 &
      .AND.START.LT.0.AND.START.GE.-NEND(IS)) THEN
      VERSUCH=2
      II=-START
      DDX=1D-3
      DO I=1,NEND(IS)
        XXEM(I)=DDX/(DBLE(NEND(IS)-1))
        XXSC(I)=XXEM(I)
      END DO
      XXEM(II)=1.0D0-DDX
      XXSC(II)=XXEM(II)
      CALL GNONID(IS,XXSC,GSC)
      GSOL=GSC
      DELTAX=DXSTAR
      NSTEP=STPSTA
      SUGCOD(0)='end2'
      GOTO 22
      END IF
!+++++
!==
!== normal minimum search is finished. Following is refinement
!== minimum is in AR001. for newph: XXEM=AR001
!== G of minimum is in GSOL for newph
!== XXEM and GSOL are global
!==
!+++++
      IF (DISTA.GT.0.0D0) THEN
        I1=NMAX
        CALL NEWPH(IS)
        IF (NMAX.GT.I1) THEN
          IF (GSOL.LT.BESTG) THEN
            BESTN=NMAX
            BESTS=SUGNR
            BESTG=GSOL
          END IF
        END IF
!-----
!----- use as starting point for MINN
!-----
        DO 310,I=1,NEND(IS)
  310   XXSC(I)=XXEM(I)
        CALL MINN(IS,XXSC,GSC,I001,GAIN)
        DO I=1,NEND(IS)
          XXEM(I)=XXSC(I)
          AR001(I)=XXSC(I)
        END DO
        SUGCOD(0)(11:14)='minn'
!-- new: call to NEWPH here, not in MINN
        I1=NMAX
        IF (GAIN.LT.-1D-10) CALL NEWPH(IS)
        IF (NMAX.GT.I1) THEN
          IF (GSOL.LT.BESTG) THEN
            BESTN=NMAX
            BESTS=SUGNR
            BESTG=GSOL
          END IF
        END IF
        IF (I001.GT.0) GOTO 999
      END IF
!+++++
!
!     XXEM(i): final xem (global)
!     AR001(i): also final xem = XXEM(i)
!     GSOL: G for XXEM(i) (calculated in NEWPH) (global)
!
      DO I001=1,NEND(IS)
        REFX(I001)=XXEM(I001)
      END DO
      REFG=GSOL
!=====add more to pool (only for NEND(IS)>2
      IF (NEND(IS).LE.2) RETURN
!*****
!*****check towards and away from endmembers
      DO IPM=1,2
!*****
      IF (IPM.EQ.1) PLUMI=1.0D0
      IF (IPM.EQ.2) PLUMI=-1.0D0
      IF (TEST.LT.0.0D0) THEN
      IF (IPM.EQ.1) THEN
      WRITE (UNIT=scr,FMT='(1X,''direction: + endmembers:'')')
      WRITE (UNIT=out,FMT='(1X,''direction: + endmembers:'')')
      ELSE
      WRITE (UNIT=scr,FMT='(1X,''direction: - endmembers:'')')
      WRITE (UNIT=out,FMT='(1X,''direction: - endmembers:'')')
      END IF
      END IF
      DXEND=DMAX1(DISTA,0.1D0)
      DO 650,I=1,NEND(IS)
  650 VEKTOR(I)=-AR001(I)*PLUMI
      DO 700,II=1,NEND(IS)
      IF (II.NE.1) VEKTOR(II-1)=-AR001(II-1)*PLUMI
      VEKTOR(II)=(1.0D0-AR001(II))*PLUMI
      CALL GNONID(IS,AR001,GSC)
      DELTAX=1D-6
      CALL ETC(IS,AR001,GSC,XXSC,F001)
!cdcFeb2011
      IF (F001.LT.GSC) THEN
      DO 710,I=1,NEND(IS)
  710 XXEM(I)=XXSC(I)
!==
      IF (DABS(F001-GSC).LT.1D-8) GOTO 700
!==
      IF (TEST.LT.0.0D0) THEN
        CALL DISTAN(IS,REFX,XXSC)
        CALL GNONID(IS,XXSC,F001)
        WRITE (scr,715) DISTA,F001-REFG,GNOM-GNOMSOFAR
        WRITE (out,715) DISTA,F001-REFG,GNOM-GNOMSOFAR
  715 FORMAT (' delta x :',1PE12.5,'  delta G: ',1PE12.5, &
      4X,'GCALC = ',I8)
      END IF
      GNOMSOFAR=GNOM
!==
      IF (IPM.EQ.1) THEN
      SUGCOD(0)(11:14)='end+'
      ELSE
      SUGCOD(0)(11:14)='end-'
      END IF
      I1=NMAX
      CALL NEWPH(IS)
      IF (NMAX.GT.I1) THEN
        IF (GSOL.LT.BESTG) THEN
          BESTN=NMAX
          BESTS=SUGNR
          BESTG=GSOL
        END IF
      END IF
      END IF
  700 CONTINUE
!*****
      END DO
!*****
!*****
!-----use above to calculate fictive ideal solution ?
!*****
!*****
!===
!===  check towards and away from str.em.
      IF (NSTREM(IS).GT.0) THEN
!===
      DO IPM=1,2
!===
      IF (IPM.EQ.1) PLUMI=1.0D0
      IF (IPM.EQ.2) PLUMI=-1.0D0
      IF (TEST.LT.0.0D0) THEN
      IF (IPM.EQ.1) THEN
      WRITE (UNIT=scr,FMT='(1X,''direction: + str. endmembers:'')')
      WRITE (UNIT=out,FMT='(1X,''direction: + str. endmembers:'')')
      ELSE
      WRITE (UNIT=scr,FMT='(1X,''direction: - str. endmembers:'')')
      WRITE (UNIT=out,FMT='(1X,''direction: - str. endmembers:'')')
      END IF
      END IF
      DO 740,II=1,NSTREM(IS)
      DXEND=DMAX1(DISTA,0.1D0)
      DO 750,I=1,NEND(IS)
  750 VEKTOR(I)=(STREM(IS,II,I)-AR001(I))*PLUMI
      CALL GNONID(IS,AR001,GSC)
      DELTAX=1D-6
      CALL ETC(IS,AR001,GSC,XXSC,F001)
!cdcFeb2011
      IF (F001.LT.GSC) THEN
      DO 760,I=1,NEND(IS)
  760 XXEM(I)=XXSC(I)
!==
      IF (DABS(F001-GSC).LT.1D-8) GOTO 740
!==
      IF (TEST.LT.0.0D0) THEN
        CALL DISTAN(IS,REFX,XXSC)
        CALL GNONID(IS,XXSC,F001)
        WRITE (scr,716) DISTA,F001-REFG,GNOM-GNOMSOFAR
        WRITE (out,716) DISTA,F001-REFG,GNOM-GNOMSOFAR
  716 FORMAT (' delta x :',1PE12.5,'  delta G: ',1PE12.5, &
      4X,'GCALC = ',I8)
      END IF
      GNOMSOFAR=GNOM
!==
      IF (IPM.EQ.1) THEN
      SUGCOD(0)(11:14)='str+'
      ELSE
      SUGCOD(0)(11:14)='str-'
      END IF
      I1=NMAX
      CALL NEWPH(IS)
      IF (NMAX.GT.I1) THEN
        IF (GSOL.LT.BESTG) THEN
          BESTN=NMAX
          BESTS=SUGNR
          BESTG=GSOL
        END IF
      END IF
      END IF
  740 CONTINUE
!===
      END DO
!===
      END IF
!===
  999 CONTINUE
!-
      IF (BESTN.GT.0) THEN
!
      SUGUSE(BESTN)=1
!
      IF (TEST.LT.0.0D0) THEN
        WRITE (scr,718) BESTS,BESTG
        WRITE (out,718) BESTS,BESTG
  718   FORMAT (' minimum for this step: ',I7,8X,'G = ',1PE12.5)
      END IF
!******     
! 1 search dominant endmember = DOMEM
      DOMEM=1
      FF=XEM(BESTN,1)
      DO I1=2,NEND(IS)
       IF (XEM(BESTN,I1).GT.XEM(BESTN,DOMEM)) THEN
        DOMEM=I1
        FF=XEM(BESTN,I1)
       END IF
      END DO
! 2 if BESTG < GTRAC(IS,DOMEM)
!   then make TRAC(IS,DOMEM,..) = XEM(BESTN,..)
      IF (BESTG.LT.GTRAC(IS,DOMEM)) THEN
        GTRAC(IS,DOMEM)=BESTG
        DO I1=1,NEND(IS)
          TRAC(IS,DOMEM,I1)=XEM(BESTN,I1)
        END DO
        IF (TEST.LT.0.0D0) THEN
          WRITE (scr,719) BESTS,DOMEM,SOLNAM(IS),NAME(EM(IS,DOMEM))
          WRITE (out,719) BESTS,DOMEM,SOLNAM(IS),NAME(EM(IS,DOMEM))
  719 FORMAT (1X,'---> ',I6,' is newTRAC for ',1X,I4,2X,A16,2X,A)
        END IF
      END IF
!-------
!      IF (START.LT.0.AND.START.GE.-NEND(IS)) THEN
!        I001=-START
!        IF (GTRAC(IS,I001).EQ.1D30.OR.BESTG.LT.GTRAC(IS,I001)) THEN
!          GTRAC(IS,I001)=BESTG
!          DO I1=1,NEND(IS)
!            TRAC(IS,I001,I1)=XEM(BESTN,I1)
!          END DO
!          IF (TEST.LT.0.0D0) THEN
!            WRITE (scr,720) BESTS,I001,SOLNAM(IS)
!            WRITE (out,720) BESTS,I001,SOLNAM(IS)
!  720 FORMAT (1X,'---> ',I6,' is TRAC for ',1X,I4,2X,A16)
!          END IF
!        END IF
!      END IF
!
!
!===== why is following not so good ?? (skipped with 777)
      IF (START.EQ.777) THEN
      I001=ANUMBER
        IF (BESTG.LT.GTRAC(IS,ANUMBER)) THEN
          GTRAC(IS,I001)=BESTG
          DO I1=1,NEND(IS)
            TRAC(IS,I001,I1)=XEM(BESTN,I1)
          END DO
          IF (TEST.LT.0.0D0) THEN
            WRITE (scr,721) BESTS,ANUMBER,SOLNAM(IS)
            WRITE (out,721) BESTS,ANUMBER,SOLNAM(IS)
  721 FORMAT (1X,'---> ',I6,' is TRAC for ',1X,I4,2X,A16)
          END IF
        END IF
      END IF
!******
      END IF
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE GOFMUE(IS,XX0,MUE0,GG0)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IS,I
      REAL*8 XX0(EMAX),GG0,MUE0(EMAX)
!-----
      GG0=0.0D0
      DO 500,I=1,NEND(IS)
      GG0=GG0+XX0(I)*MUE0(I)
  500 CONTINUE
!===
      RETURN
      END
!-----
!******************************
      SUBROUTINE MINN(IS,XX0,GG0,NOREF,GAIN)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IS,I,II,N0,IE,IMEGA,NRLOOP,ERRC,N1,CODE,BCOD, &
      PINDEX(COMAX),MINDEX(COMAX),IP,IM,I0,NOREF
      REAL*8 XX0(EMAX),GG0,MUE0(EMAX),XX1(EMAX),GG1,MUE1(EMAX), &
      DX0(COMAX),DX,FFX,JACOB(COMAX,COMAX),FF,F1,F2,CC0(COMAX),FUN2, &
      FUN1,BREMS,XX2(EMAX),GG2,MUE2(EMAX),DXP(COMAX),DXM(COMAX), &
      MINXV(COMAX),XX0N(EMAX),GLAST,GPROG,GPROGTOT,RTEST,GAIN
! only needed for GAUSSEL:
!      INTEGER*4 J
!      REAL*8 MAT(45,45)
!      LOGICAL*4 SING
!-----
      NOREF=0
      ERRC=0
      RTEST=0.0D0
      N0=NEND(IS)
      DX=1.0D-7
      NRLOOP=20
      GPROG=0.0D0
      GPROGTOT=0.0D0
      CALL MUECAL(IS,XX0,MUE0)
      CALL GOFMUE(IS,XX0,MUE0,GG0)
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (UNIT=scr,FMT='('' refine, initial G :  '',1PE15.8)') GG0
      WRITE (UNIT=out,FMT='('' refine, initial G :  '',1PE15.8)') GG0
      WRITE (scr,1000) (XX0(I),I=1,N0)
      WRITE (out,1000) (XX0(I),I=1,N0)
 1000 FORMAT (' initial X(i)      :',100(2X,1PE15.8))
      WRITE (scr,1002) (MUE0(I)-GG0,I=1,N0)
      WRITE (out,1002) (MUE0(I)-GG0,I=1,N0)
 1002 FORMAT (' initial MUE(i)-GG :',100(2X,1PE15.8))
      END IF
!---- test convergence
      GAIN=0.0D0
      FF=DABS(MUE0(1)-GG0)
      DO 300,I=2,N0
      FF=FF+DABS(MUE0(I)-GG0)
  300 CONTINUE
      FF=FF/DBLE(N0)
      IF (FF.LT.1D-2) THEN
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (scr,1005)
      WRITE (out,1005)
 1005 FORMAT (' no refinement necessary')
      END IF
!==
      NOREF=1
      RETURN
      END IF
!==
      GLAST=GG0
      N1=N0-1
      DO 310,I=1,N0
  310 MINXV(I)=0.0D0
      IF (NNEGEM(IS).GT.0) THEN
      DO 320,I=1,NNEGEM(IS)
  320 MINXV(NEGEM(IS,I))=-1.0D0
      END IF
!----- make PINDEX and MINDEX
      I0=1
      FF=XX0(1)
      DO 324,I=1,N0
      IF (XX0(I).GT.FF) THEN
      I0=I
      FF=XX0(I)
      END IF
  324 CONTINUE
!CCCC      I0=2
      DO 325, I=1,N1
      PINDEX(I)=MOD(I+I0-2,N0)+1
      MINDEX(I)=MOD(I+I0-1,N0)+1
  325 CONTINUE
!====
!==== start the megaloop
      FF=1.0D0
      DO 900,IMEGA=1,NRLOOP
!----- stop if FF < 1D-3
      IF (FF.LT.1D-3) GOTO 901
      BREMS=1.0D0
!---- make CC0
      DO 330,I=1,N1
  330 CC0(I)=-(MUE0(PINDEX(I))-MUE0(MINDEX(I)))
!---- find dxp and dxm
      DO 335,I=1,N1
      IP=PINDEX(I)
      IM=MINDEX(I)
      F1=DMIN1(1.0D0-XX0(IP),XX0(IM)-MINXV(IM))
      DXP(I)=DMIN1(DX,DABS(F1/2.0D0))
      F2=DMIN1(XX0(IP)-MINXV(IP),1.0D0-XX0(IM))
      DXM(I)=DMIN1(DX,DABS(F2/2.0D0))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!JUL2011 why is the following better??? 
      DXM(I)=0.0D0
  335 CONTINUE
!---- make jacobian
      DO 500,IE=1,N1
      DO 510,I=1,N0
      XX1(I)=XX0(I)
      XX2(I)=XX0(I)
  510 CONTINUE
      IP=PINDEX(IE)
      IM=MINDEX(IE)
      XX1(IP)=XX0(IP)+DXP(IE)
      XX1(IM)=XX0(IM)-DXP(IE)
      XX2(IP)=XX0(IP)-DXM(IE)
      XX2(IM)=XX0(IM)+DXM(IE)
      FFX=XX1(IP)-XX2(IP)
!====
!==== versuch Jul 2011
  567 CONTINUE
      IF (XX0(IP).GT.XX0(IM)) THEN
!      DXP(IE)=DMIN1(DX,DABS(XX0(IP)/2.0D0))
      XX1(IP)=XX0(IP)-DXP(IE)
      XX1(IM)=XX0(IM)+DXP(IE)
      FFX=XX0(IP)-XX1(IP)
      ELSE
!      DXP(IE)=DMIN1(DX,DABS(XX0(IM)/2.0D0))
      XX1(IP)=XX0(IP)+DXP(IE)
      XX1(IM)=XX0(IM)-DXP(IE)
      FFX=XX1(IP)-XX0(IP)
      END IF
!====
!====
!2:
      CODE=1
      CALL SPACETEST(IS,XX1,CODE)
!4      DO 512,I=1,N0
!4      IF (XX1(I).GT.1.0D0.OR.XX1(I).LT.MINXV(I)) CODE=0
!4  512 CONTINUE
      IF (CODE.EQ.0) THEN
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (scr,1015) DXP(IE),(XX1(I),I=1,N0)
      WRITE (out,1015) DXP(IE),(XX1(I),I=1,N0)
 1015 FORMAT (' spacetest failed for XX1',2X,1PE15.8/100(2X,1PE15.8))
      END IF
!=====
!==== versuch Jul 2011
      IF (DXP(IE).GT.1D-9) THEN
      DXP(IE)=DXP(IE)/2.0D0
      GOTO 567
      END IF
!=====
!=====
!==
      RETURN
      END IF
      CODE=1
      CALL SPACETEST(IS,XX2,CODE)
!4      DO 513,I=1,N0
!4      IF (XX2(I).GT.1.0D0.OR.XX2(I).LT.MINXV(I)) CODE=0
!4  513 CONTINUE
      IF (CODE.EQ.0) THEN
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (scr,1016) (XX2(I),I=1,N0)
      WRITE (out,1016) (XX2(I),I=1,N0)
 1016 FORMAT (' spacetest failed for XX2',/100(2X,1PE15.8))
      END IF
!==
      RETURN
      END IF
      CALL MUECAL(IS,XX1,MUE1)
      CALL GOFMUE(IS,XX1,MUE1,GG1)
      CALL MUECAL(IS,XX2,MUE2)
      CALL GOFMUE(IS,XX2,MUE2,GG2)
      DO 530,I=1,N1
      IP=PINDEX(I)
      IM=MINDEX(I)
      FUN2=(MUE2(IP)-MUE2(IM))
      FUN1=(MUE1(IP)-MUE1(IM))
      IF (DABS(FFX).LT.1D-50) FFX=1D-50
      JACOB(IE,I)=(FUN1-FUN2)/(FFX)
  530 CONTINUE
  500 CONTINUE
!---- solve |J| * DX0 = -(MUE-GG)
      ERRC=0
!CCCC      CALL LINSOFT(N1,JACOB,CC0,DX0,ERRC)
      CALL LINCOMP(N1,JACOB,CC0,DX0,RTEST,ERRC)
!------------------------
! GAUSSEL and LINCOMP seem to give quite identical results
! decide later which one to use (7. Feb 2015)
! only needed for GAUSSEL:
!      WRITE (UNIT=scr,FMT='('' dx0LC'',I4,100(2X,1PE15.8))') &
!       IS,(DX0(J),J=1,N1)
!      WRITE (UNIT=out,FMT='('' dx0LC'',I4,100(2X,1PE15.8))') &
!       IS,(DX0(J),J=1,N1)
!!
!      SING=.FALSE.
!      ERRC=0
!      DO I=1,N1
!       DO J=1,N1
!        MAT(I,J)=JACOB(I,J)
!       END DO
!       MAT(I,N1+1)=CC0(I)
!      END DO
!
!      CALL GAUSSEL(MAT,I,J,N1,DX0,SING)
!      IF (SING) ERRC=1
!      IF (SING) PRINT *, 'Matrix is (nearly) singular'
!      IF (SING) WRITE (UNIT=out,FMT='('' Matrix is (nearly) singular'')')
!      WRITE (UNIT=scr,FMT='('' dx0GA'',I4,100(2X,1PE15.8))') &
!       IS,(DX0(J),J=1,N1)
!      WRITE (UNIT=out,FMT='('' dx0GA'',I4,100(2X,1PE15.8))') &
!       IS,(DX0(J),J=1,N1)
!------------------------
      IF (ERRC.NE.0) THEN
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (scr,1025) N1
      WRITE (out,1025) N1
 1025 FORMAT (' rank not ',I4)
      END IF
!==
      RETURN
      END IF
!----
!---- make new guess
      DO 810,II=1,5
      DO 550,I=1,N0
  550 XX0N(I)=XX0(I)
      DO 800,I=1,N1
      IP=PINDEX(I)
      IM=MINDEX(I)
      XX0N(IP)=XX0N(IP)+DX0(I)*BREMS
      XX0N(IM)=XX0N(IM)-DX0(I)*BREMS
  800 CONTINUE
!2:
      BCOD=0
      CODE=1
      CALL SPACETEST(IS,XX0N,CODE)
!4      DO 514,I=1,N0
!4      IF (XX0N(I).GT.1.0D0.OR.XX0N(I).LT.MINXV(I)) CODE=0
!4  514 CONTINUE
      IF (CODE.EQ.0) THEN
      BCOD=BCOD+1
      ELSE
      CALL MUECAL(IS,XX0N,MUE0)
      CALL GOFMUE(IS,XX0N,MUE0,GG0)
      GPROG=GG0-GLAST
      IF (GPROG.GE.0.0D0) BCOD=BCOD+2
      END IF
      IF (BCOD.GT.0) THEN
      BREMS=BREMS/10.0D0
!==
!      IF (TEST.LT.0.0D0) THEN
!      WRITE (scr,1035) SOLNAM(IS),BCOD,(XX0N(I),I=1,N0)
!      WRITE (out,1035) SOLNAM(IS),BCOD,(XX0N(I),I=1,N0)
! 1035 FORMAT (A12,' brems ',I2,100(2X,1PE15.8))
!      END IF
!==
      ELSE
      GOTO 811
      END IF
  810 CONTINUE
  811 CONTINUE
!----
      IF (BCOD.GT.0) THEN
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (scr,1030) IMEGA,BCOD,SOLNAM(IS)
      WRITE (out,1030) IMEGA,BCOD,SOLNAM(IS)
 1030 FORMAT (' loop =',I4,': outside space(1), no progress(2):' &
      ,I3,2X,A)
      WRITE (scr,1031) IMEGA,FFX,BREMS,(DX0(I),I=1,N1)
      WRITE (out,1031) IMEGA,FFX,BREMS,(DX0(I),I=1,N1)
 1031 FORMAT (1X,I4,' FFX,BREMS,DX0:',6(2X,1PE15.8), &
      :,100(/,54X,4(2X,1PE15.8)))
      WRITE (scr,1032) IMEGA,GG0,GPROG,(XX0N(I),I=1,N0)
      WRITE (out,1032) IMEGA,GG0,GPROG,(XX0N(I),I=1,N0)
 1032 FORMAT (1X,I4,' G,DG,last Xi :',6(2X,1PE15.8), &
      :,100(/,54X,4(2X,1PE15.8)))
      END IF
!==
      IF (IMEGA.EQ.1) RETURN
!1      RETURN
      ELSE
      DO 820,I=1,N0
  820 XX0(I)=XX0N(I)
      END IF
      IF (BCOD.GT.0) THEN
      IF (BCOD.GT.1) THEN
      CALL MUECAL(IS,XX0,MUE0)
      CALL GOFMUE(IS,XX0,MUE0,GG0)
      END IF
      GOTO 901
      END IF
!---- test convergence
      FF=DABS(MUE0(1)-GG0)
      DO 400,I=2,N0
      FF=FF+DABS(MUE0(I)-GG0)
  400 CONTINUE
      FF=FF/DBLE(N0)
      GPROGTOT=GPROGTOT+GPROG
!==
!      IF (TEST.LT.0.0D0) THEN
!      WRITE (scr,1020) IMEGA,GG0,GPROG,(XX0(I),I=1,N0)
!      WRITE (out,1020) IMEGA,GG0,GPROG,(XX0(I),I=1,N0)
! 1020 FORMAT (/,1X,I4,' new X(i)     :',100(2X,1PE15.8))
!      WRITE (scr,1022) IMEGA,FFX,BREMS,(MUE0(I)-GG0,I=1,N0)
!      WRITE (out,1022) IMEGA,FFX,BREMS,(MUE0(I)-GG0,I=1,N0)
! 1022 FORMAT (1X,I4,' new MUE(i)-GG:',100(2X,1PE15.8))
!C      WRITE (out,1023) IMEGA,(DX0(I),I=1,N1)
!C 1023 FORMAT (1X,I4,' DX0          :',51X,100(2X,1PE15.8))
!      END IF
!==
      GLAST=GG0
  900 CONTINUE
!---- end of megaloop
!====
  901 CONTINUE
      GAIN=GPROGTOT
!==
      IF (TEST.LT.0.0D0) THEN
      WRITE (scr,1050) IMEGA,GG0,GPROGTOT
      WRITE (out,1050) IMEGA,GG0,GPROGTOT
 1050 FORMAT (1X,I4,' new G and dG :',2(2X,1PE15.8))
      WRITE (scr,1052) IMEGA,(XX0(I),I=1,N0)
      WRITE (out,1052) IMEGA,(XX0(I),I=1,N0)
 1052 FORMAT (1X,I4,' new X(i)     :',100(2X,1PE15.8))
      WRITE (scr,1055) IMEGA,(MUE0(I)-GG0,I=1,N0)
      WRITE (out,1055) IMEGA,(MUE0(I)-GG0,I=1,N0)
 1055 FORMAT (1X,I4,' new MUE(i)-GG:',100(2X,1PE15.8))
!      WRITE (out,1013) IMEGA,(DX0(I),I=1,N1)
! 1013 FORMAT (1X,I4,' DX0          :',51X,100(2X,1PE15.8))
      END IF
!==
!---- add phase: moved to MARMIN
!      DO 700,I=1,N0
!  700 XXEM(I)=XX0(I)
!      SUGCOD(0)(11:14)='minn'
!      CALL NEWPH(IS)
!===
      RETURN
      END
!-----
!******************************
      SUBROUTINE STEEP(IS,ARA,MUE)
!-----find direction of steepest descent.
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IS,I,BING
      REAL*8 DELMUE,SUMME,ARA(EMAX),MUE(EMAX)
!-----
      DELMUE=0.0D0
      SUMME=0.0D0
      BING=0
!=====
      DO I=1,NEND(IS)
        DELMUE=DELMUE+MUE(I)
      END DO
      DELMUE=DELMUE/DBLE(NEND(IS))
      DO 502,I=1,NEND(IS)
      VEKTOR(I)=DELMUE-MUE(I)
!cdcjun09
!      IF (G(EM(IS,I)).GT.1D5) VEKTOR(I)=0.0D0
!cdc
!      IF (ARA(I).LE.1D-20.AND.VEKTOR(I).LT.0.0D0) THEN
      IF (ARA(I).LE.1D-50.AND.VEKTOR(I).LT.0.0D0 &
      .AND.NNEGEM(IS).EQ.0) THEN
!-
       VEKTOR(I)=0.0D0
       BING=BING+1
      END IF
!
!---- check for zero em
      IF (DABS(ARA(I)).LT.1D-9.AND.ZEND(IS,I).GT.0) THEN
       VEKTOR(I)=0.0D0
      END IF
!
  502 SUMME=SUMME+VEKTOR(I)*VEKTOR(I)
!cdc
      IF (BING.GT.0.AND.BING.LT.NEND(IS)) THEN
!      WRITE (6,1000) SOLNAM(IS),(ARA(I),I=1,NEND(IS))
! 1000 FORMAT (A10,
!     >'  negative gradient for x(i)<10D-20: x ='
!     >,20E13.4)
      SUMME=0.0D0
      DELMUE=0.0D0
      DO 601,I=1,NEND(IS)
  601 DELMUE=DELMUE+VEKTOR(I)
      DELMUE=DELMUE/DBLE(NEND(IS)-BING)
      DO 602,I=1,NEND(IS)
      IF (VEKTOR(I).NE.0.0D0) VEKTOR(I)=DELMUE-VEKTOR(I)
  602 SUMME=SUMME+VEKTOR(I)*VEKTOR(I)
      END IF
!-----
      SUMME=DSQRT(SUMME)
      IF (SUMME.GT.0.0D0) THEN
      DO 504,I=1,NEND(IS)
  504 VEKTOR(I)=VEKTOR(I)/SUMME
      END IF
      RETURN
      END
!-----
!******************************
      SUBROUTINE DISTAN(IS,X1,X2)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IS,I
      REAL*8 X1(EMAX),X2(EMAX),DIFF
!-----
      DISTA=0.0D0
      DO 501,I=1,NEND(IS)
      DIFF=X2(I)-X1(I)
  501 DISTA=DISTA+DIFF*DIFF
      DISTA=DSQRT(DISTA)
      RETURN
      END
!-----
!******************************
      SUBROUTINE ETC(IS,XSTART,GSTART,XNEW,GNEW)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IS,I,II,I001
      REAL*8 XSTART(EMAX),GSTART,XOLD(EMAX),GOLD,XNEW(EMAX), &
      GNEW,DELIX
!-----
      DELIX=DELTAX
      GOLD=GSTART
      DO 500,I001=1,NEND(IS)
  500 XOLD(I001)=XSTART(I001)
      CALL VECADD(IS,XSTART,DELIX,XNEW)
      CALL GNONID(IS,XNEW,GNEW)
      DO 502,II=1,GCMAX
      IF (GNEW.GE.GOLD) GO TO 1
      DELIX=DELIX+DELTAX
      GOLD=GNEW
      DO 501,I001=1,NEND(IS)
  501 XOLD(I001)=XNEW(I001)
      CALL VECADD(IS,XOLD,DELIX,XNEW)
  502 CALL GNONID(IS,XNEW,GNEW)
    1 GNEW=GOLD
      DO 503,I001=1,NEND(IS)
  503 XNEW(I001)=XOLD(I001)
      DO 504,I=1,GCMAX
      IF (GNEW.LT.GSTART.OR.DELIX.LE.DXMIN) GO TO 2
      DELIX=DELIX/2.0D0
      CALL VECADD(IS,XSTART,DELIX,XNEW)
  504 CALL GNONID(IS,XNEW,GNEW)
    2 IF (GNEW.GT.GSTART) THEN
      GNEW=GSTART
      DO 505,I001=1,NEND(IS)
  505 XNEW(I001)=XSTART(I001)
      END IF
      RETURN
      END
!-----
!******************************
      SUBROUTINE VECADD(IS,X1,DELIX,X2)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IS,I,CODE
      REAL*8 X1(EMAX),X2(EMAX),DELIX,SUMME,FF
!-----
      CODE=1
      CALL SPACETEST(IS,X1,CODE)
      IF (CODE.EQ.0) THEN
!      WRITE (6,1000) SOLNAM(IS),(X1(I),I=1,NEND(IS))
! 1000 FORMAT (1X,A16,' x1 in VECADD is outside space ',20(1PE12.5))
      DO 200,I=1,NEND(IS)
      X2(I)=1.0D0/DBLE(NEND(IS))
  200 CONTINUE
      RETURN
      END IF
      FF=DMAX1(DELIX,DXMIN)
      IF (DELIX.LT.0.0D0) FF=DMIN1(DELIX,-DXMIN)
      SUMME=0.0D0
      DO 400,I=1,NEND(IS)
      X2(I)=X1(I)+VEKTOR(I)*FF
!      IF (X2(I).GT.1.0D0) X2(I)=1.0D0-1D-6
  400 SUMME=SUMME+X2(I)
!-----
    1 CODE=2
      CALL SPACETEST(IS,X2,CODE)
      IF (CODE.EQ.0) THEN
      IF (DABS(FF).LT.DXMIN) THEN
      DO 250,I=1,NEND(IS)
  250 X2(I)=X1(I)
      RETURN
      END IF
      FF=FF/10.0D0
      DO 260,I=1,NEND(IS)
  260 X2(I)=X1(I)+VEKTOR(I)*FF
!DC
      IF (NNEGEM(IS).GT.0) THEN
      CODE=2
      CALL SPACETEST(IS,X2,CODE)
      IF (CODE.EQ.0) THEN
      DO 261,I=1,NEND(IS)
      IF (X2(I).GT.1D-2) THEN
      X2(I)=X2(I)-1D-3
      ELSE
      X2(I)=X2(I)+1D-2
      END IF
  261 CONTINUE
      END IF
      END IF
!DC
      GOTO 1
      END IF
!---
!-----
      SUMME=0.0D0
      DO 600,I=1,NEND(IS)
  600 SUMME=SUMME+X2(I)
      IF (DABS(SUMME).GT.1.0D-20) THEN
      DO 610,I=1,NEND(IS)
  610 X2(I)=X2(I)/(SUMME)
      END IF
      RETURN
      END
!-----
!******************************
      SUBROUTINE SPACETEST(IS,X1,CODE)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IS,ILX,IE,IK,I001,CODE
      REAL*8 X1(EMAX),XTEST,TOLER
!-----
!----- if test OK, CODE remains unchanged, else CODE=0
!      TOLER=1.0D-8
      TOLER=0.0D0
      I001=0
!-
      IF (NNEGEM(IS).NE.0) THEN
      DO 500,ILX=1,NSIEL(IS)
      XTEST=0.0D0
      DO 510,IK=1,NEMQQ(IS,ILX)
      IE=EMQQ(IS,ILX,IK)
      XTEST=XTEST+EMXX(IS,ILX,IE)*X1(IE)
  510 CONTINUE
      IF (XTEST.LT.(0.0D0-TOLER).OR.XTEST.GT.(1.0D0+TOLER)) I001=1
  500 CONTINUE
!-
      ELSE
      DO 520,IE=1,NEND(IS)
      IF (X1(IE).LT.(0.0D0-TOLER).OR.X1(IE).GT.(1.0D0+TOLER)) I001=1
  520 CONTINUE
      END IF
!-
      IF (I001.NE.0) CODE=0
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE FULLRES
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 K,II,J,IS,N,I
      REAL*8 XXSC(EMAX),GSC
!-----
      DO 500,K=1,NMAX
      IF (EMCODE(K).GT.0) THEN
      IS=EMCODE(K)
!---- the following is XSOL
      DO 502,N=1,NUN
      X(K,N)=0.0D0
      DO 502,I=1,NEND(IS)
  502 X(K,N)=X(K,N)+XEM(K,I)*XX(EM(IS,I),N)
!---- end XSOL
      DO 510,J=1,NEND(IS)
  510 XXSC(J)=XEM(K,J)
      CALL GNONID(IS,XXSC,GSC)
      G(K)=GSC
      ELSE
      DO 512,II=1,NUN
  512 X(K,II)=XX(NUMMER(K),II)
      G(K)=GG(NUMMER(K))
      END IF
  500 CONTINUE
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE MAPRI
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 I,J
!-----
      WRITE (6,1000)
      WRITE (out,1000)
 1000 FORMAT (/' MAPRI')
      WRITE (6,1002) (SUGG(J),EMCODE(J),J=1,NUN)
      WRITE (out,1002) (SUGG(J),EMCODE(J),J=1,NUN)
 1002 FORMAT (6X,100(2X,2I6))
      DO 500,I=1,NUN
      WRITE (6,1004) NAME(I),(X(J,I),J=1,NUN)
      WRITE (out,1004) NAME(I),(X(J,I),J=1,NUN)
 1004 FORMAT (A6,100(2X,1PE12.5))
  500 CONTINUE
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE FULLRED_OLD
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 II,IIX,I001,PIV,IC,IR
      REAL*8 COLSUM(COMAX),FX
!-----
      DO 500,PIV=1,NUN
!----- mach colsum
      DO 510,IC=PIV,NUN
      COLSUM(IC)=0.0D0
      DO 510,IR=1,NUN
      COLSUM(IC)=COLSUM(IC)+DABS(X(IC,IR))
  510 CONTINUE
!----- end mach colsum
      IIX=PIV
      FX=DABS(X(PIV,PIV)/COLSUM(PIV))
!--- suche max
      DO 512,II=PIV+1,NUN
      IF (DABS(X(II,PIV)/COLSUM(II)).GT.FX) THEN
      IIX=II
      FX=DABS(X(II,PIV)/COLSUM(II))
      END IF
  512 CONTINUE
!--- end suche max
      IF (FX.EQ.0.0D0) THEN
      END IF
      I001=PIV
      IF (IIX.NE.PIV) CALL COLCHG(I001,IIX)
  500 CALL REDUCE(I001)
!+++++
!-----
      RETURN
      END
!-----
!******************************
!----- (caused at some times problems e.g. with fo_qz_melt) ???
      SUBROUTINE FULLRED
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 IIX,IIY,I001,PIV,IC,IR
      REAL*8 COLSUM(COMAX),FX,F1,FF(CALMAX)
!-----
      DO 500,PIV=1,NUN
!----- mach colsum
      DO 510,IC=PIV,NUN
      COLSUM(IC)=0.0D0
      DO 510,IR=1,NUN
      COLSUM(IC)=COLSUM(IC)+DABS(X(IC,IR))
  510 CONTINUE
!----- end mach colsum
      IIY=0
      IIX=0
      FX=0
!--- suche max
      DO 512,IR=PIV,NUN
      DO 514,IC=PIV,NUN
      F1=DABS(X(IC,IR)/COLSUM(IC))
      IF (F1.GT.FX) THEN
      IIX=IC
      IIY=IR
      FX=F1
      END IF
  514 CONTINUE
  512 CONTINUE
!--- end suche max (warum konnte TTY=0 werden?)
      IF (IIY.NE.PIV.AND.IIY.NE.0) THEN
!---- switch rows
      DO 310,IC=1,NMAX
  310 FF(IC)=X(IC,IIY)
      DO 312,IC=1,NMAX
  312 X(IC,IIY)=X(IC,PIV)
      DO 314,IC=1,NMAX
  314 X(IC,PIV)=FF(IC)
!---- end switch rows
      END IF
      I001=PIV
      IF (IIX.NE.PIV) CALL COLCHG(I001,IIX)
  500 CALL REDUCE(I001)
!+++++
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE REDUCE(K)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 K,I,II
      REAL*8 F,AR(COMAX)
!-----
      F=X(K,K)
! maybe here IF (DABS(F).LT.1.0D-10) THEN ??? sept2009
      IF (F.EQ.0.0D0) THEN
!==
      IF (TEST.LT.0.0D0) CALL MAPRI
!==
      DO 601,II=1,NMAX
  601 X(II,K)=0.0D0
      ELSE
      DO 602,I=1,NMAX
      IF (X(I,K).NE.0.0D0) X(I,K)=X(I,K)/F
  602 CONTINUE
      DO 603,I=1,NUN
  603 AR(I)=X(K,I)
      DO 605,I=1,NUN
      IF (I.NE.K.AND.AR(I).NE.0.0D0) THEN
      DO 604,II=1,NMAX
      X(II,I)=X(II,I)-X(II,K)*AR(I)
!CCCCC      IF (DABS(X(II,I)).LT.1D-16) X(II,I)=0.0D0
!CCCCC      IF (DABS(X(II,I)).LT.1D-40) X(II,I)=0.0D0
  604 CONTINUE
      END IF
  605 CONTINUE
      END IF
      DO 606,I=1,NUN
  606 X(K,I)=0.0D0
      X(K,K)=1.0D0
      RETURN
      END
!-----
!******************************
      SUBROUTINE COLCHG(K,I)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER*4 K,I,I001
!-----
      DO 501,I001=1,COMAX
  501 X(0,I001)=X(K,I001)
      NN(0)=NN(K)
      G(0)=G(K)
      NUMMER(0)=NUMMER(K)
      EMCODE(0)=EMCODE(K)
      SUGG(0)=SUGG(K)
      SUGCOD(0)=SUGCOD(K)
      ISTAB(0)=ISTAB(K)
      DELXXX(0)=DELXXX(K)
      SUGUSE(0)=SUGUSE(K)
      DO 502,I001=1,EMAX
  502 XEM(0,I001)=XEM(K,I001)
!-
      DO 503,I001=1,COMAX
  503 X(K,I001)=X(I,I001)
      NN(K)=NN(I)
      G(K)=G(I)
      NUMMER(K)=NUMMER(I)
      EMCODE(K)=EMCODE(I)
      SUGG(K)=SUGG(I)
      SUGCOD(K)=SUGCOD(I)
      ISTAB(K)=ISTAB(I)
      DELXXX(K)=DELXXX(I)
      SUGUSE(K)=SUGUSE(I)
      DO 504,I001=1,EMAX
  504 XEM(K,I001)=XEM(I,I001)
!-
      DO 505,I001=1,COMAX
  505 X(I,I001)=X(0,I001)
      NN(I)=NN(0)
      G(I)=G(0)
      NUMMER(I)=NUMMER(0)
      EMCODE(I)=EMCODE(0)
      SUGG(I)=SUGG(0)
      SUGCOD(I)=SUGCOD(0)
      ISTAB(I)=ISTAB(0)
      DELXXX(I)=DELXXX(0)
      SUGUSE(I)=SUGUSE(0)
      DO 506,I001=1,EMAX
  506 XEM(I,I001)=XEM(0,I001)
      RETURN
      END
!-----
!********************************
      SUBROUTINE LINCOMP(N0,ARR0,CRR0,DX0,RTEST,ERRC)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!.....solves a linear system of n equations with n unknowns
!.....
!.....          x(1)*A(1,1) + x(2)*A(1,2) + ...   = C(1)
!.....          x(1)*A(2,1) + x(2)*A(2,2) + ...   = C(2)
!.....          ...
!.....          x(1)*A(n,1) + x(2)*A(n,2) + ...   = C(n)
!.....
!.....N0,N: number of equations
!.....ARR0,ARR: coefficients
!.....CRR0,CRR: constants (right hand side)
!.....DX0: solution
!.....ERRC: 0:OK, 1:rank not N0
      REAL*8 ARR0(COMAX,COMAX),CRR0(COMAX),DX0(COMAX),COLSUM(COMAX), &
      FF(COMAX),WERT,F1,FC,RTEST
      INTEGER*4 N0,IC,IR,ERRC,IIX,IIY,PIV,II,I
!-----common blocks
      REAL*8 ARR(COMAX,COMAX),CRR(COMAX)
      INTEGER*4 N,COLNR(COMAX),RANG
!-----end of common
      ERRC=0
      N=N0
      DO 300,IR=1,N
      COLNR(IR)=IR
      CRR(IR)=CRR0(IR)
      DO 300,IC=1,N
      ARR(IR,IC)=ARR0(IR,IC)
  300 CONTINUE
!-----
!=====
!===== reduc
      RANG=0
      DO 600,PIV=1,N
!----- mach colsum
      DO 400,IC=PIV,N
      COLSUM(IC)=0.0D0
      DO 400,IR=1,N
  400 COLSUM(IC)=COLSUM(IC)+DABS(ARR(IR,IC))
!----- end mach colsum
      IIY=0
      IIX=0
      WERT=0.0D0
!--- suche max
      DO 610,IR=PIV,N
      DO 620,IC=PIV,N
      IF (COLSUM(IC).LE.0.0D0) GOTO 609
      F1=DABS(ARR(IR,IC)/COLSUM(IC))
      IF (F1.GT.WERT) THEN
      WERT=F1
      IIX=IR
      IIY=IC
      END IF
  609 CONTINUE
  620 CONTINUE
  610 CONTINUE
!--- end suche max
      IF (IIX.EQ.0.OR.IIY.EQ.0) GOTO 601
      IF (IIX.NE.PIV) THEN
!---- switch rows
      FC=CRR(IIX)
      DO 310,I=1,N
  310 FF(I)=ARR(IIX,I)
      CRR(IIX)=CRR(PIV)
      DO 312,I=1,N
  312 ARR(IIX,I)=ARR(PIV,I)
      CRR(PIV)=FC
      DO 314,I=1,N
  314 ARR(PIV,I)=FF(I)
!---- end switch rows
      END IF
      IF (IIY.NE.PIV) THEN
!---- switch columns
      II=COLNR(IIY)
      DO 320,I=1,N
  320 FF(I)=ARR(I,IIY)
      COLNR(IIY)=COLNR(PIV)
      DO 322,I=1,N
  322 ARR(I,IIY)=ARR(I,PIV)
      COLNR(PIV)=II
      DO 324,I=1,N
  324 ARR(I,PIV)=FF(I)
!---- end switch columns
      END IF
      F1=ARR(PIV,PIV)
      RANG=PIV
!---- lindiv
      CRR(PIV)=CRR(PIV)/F1
      DO 330,I=1,N
  330 ARR(PIV,I)=ARR(PIV,I)/F1
!---- end lindiv
      DO 500,II=1,N
      IF (II.NE.PIV) THEN
      F1=ARR(II,PIV)
!---- linsub
      CRR(II)=CRR(II)-CRR(PIV)*F1
      DO 340,I=1,N
      ARR(II,I)=ARR(II,I)-ARR(PIV,I)*F1
      IF (RTEST.NE.0.0D0) THEN
      IF (DABS(ARR(II,I)).LE.RTEST) ARR(II,I)=0.0D0
      END IF
  340 CONTINUE
!---- end linsub
      END IF
  500 CONTINUE
  600 CONTINUE
!===== end reduc
  601 CONTINUE
!=====
      IF (RANG.NE.N.AND.TEST.LT.0.0D0) THEN
      WRITE (6,1000) RANG,N
      WRITE (out,1000) RANG,N
 1000 FORMAT (' LINCOMP : rank =',I4,'  N =',I4)
      ERRC=1
      RETURN
      END IF
!---- write result into DX0
      DO 700,IC=1,N
  700 DX0(COLNR(IC))=CRR(IC)
!=====
!     DO 800,IR=1,N
!     WRITE (6,2000) IR,DX0(IR)
!     WRITE (out,2000) IR,DX0(IR)
!2000 FORMAT ('lincomp: X =',I5,F20.8)
! 800 CONTINUE
!=====
      RETURN
      END
