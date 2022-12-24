SUBROUTINE K_coef_PT(VSTART) ! 12 April,2002.
USE INITIAL
!* --------------------------------------------------------------------- *
!* Automatic taking into account number of atmospheric levels
!* H = CONST, CKD-2.4 etc.											*
!* --------------------------------------------------------------------- *
	PARAMETER ( NT0=10,					&
	NT1=NT0*2,NT2=NT1*2,NT3=NT2*2,NT4=NT3*2,NT5= NT4*2,	&
	NT6=NT5*2,NT7=NT6*2,NT8=NT7*2,NT9=NT8*2,NT= NT9*4+1,	&
				OBR = 10.0,OBR25=250.0,	&
!################           ===========          ############!
				STEP = 1.0, NINT = 10)
!*
	REAL*8 VSTART, VFINISH, VS
!*
	CHARACTER COMPONENT*5, JMET*3, DIR_NAME*4, LINE_PATH*8 !#### 12
	COMMON/GASES/ COMPONENT(NCOMP)
	COMMON/AMOL/ AMOLI(NCOMP)
	COMMON/PHIPAR/T_T,P_P,RO_RO
!*
	COMMON/MESH/DELTA,H,H0,H1,H2,H3,H4,H5,H6,H7,H8,H9,RK(NT)	&
	,RK0(NT0),RK0L(NT0),RK0P(NT0),RK1(NT1),RK1L(NT1),RK1P(NT1)	&
	,RK2(NT2),RK2L(NT2),RK2P(NT2),RK3(NT3),RK3L(NT3),RK3P(NT3)	&
	,RK4(NT4),RK4L(NT4),RK4P(NT4),RK5(NT5),RK5L(NT5),RK5P(NT5)	&
	,RK6(NT6),RK6L(NT6),RK6P(NT6),RK7(NT7),RK7L(NT7),RK7P(NT7)	&
	,RK8(NT8),RK8L(NT8),RK8P(NT8),RK9(NT9),RK9L(NT9),RK9P(NT9)
	DIMENSION ICODE(NCOMP),DIR_NAME(200)
	DATA ISTART/0/
!*
	IF(ISTART == 0)THEN
			ISTART=1
! *** Metka for Zj-Level  ('1__.','2__.',...,'100.',...) ***
DIR_NAME='___.'
DO JN=1,200
IF(JN<10)THEN
WRITE(DIR_NAME(JN)(1:1),'(I1)')JN   
ELSE
     IF(JN<100)THEN
WRITE(DIR_NAME(JN)(1:2),'(I2)')JN
	 ELSE
WRITE(DIR_NAME(JN)(1:3),'(I3)')JN
	 END IF
END IF
END DO

!*						DELTA=OBR
						DELTA=STEP*NT0
						H0=STEP
						H1=H0/2.
						H2=H1/2.
						H3=H2/2.
						H4=H3/2.
						H5=H4/2.
						H6=H5/2.
						H7=H6/2.
						H8=H7/2.
						H9=H8/2.
						H=H9/4.
!**********************************************************
		VFINISH=VSTART+10.D0
!*............................Search for accont molecules papameters:
 LINE_PATH='./HITRAN/' !#### 'C:\HITRAN16\'
 !####               CALL DBASE_2016(LINE_PATH) 
  		NMISTAKE = 0
		IPRESENT = 0
	DO 11 M = 1, NGAS
			DO 1 I = 1, NCOMP
	IF(MOLECULE(M) == COMPONENT(I))THEN
	IPRESENT		= IPRESENT + 1
	ICODE(IPRESENT)	= I
	MOLECULE(IPRESENT) = MOLECULE(M)
			GO TO 11
	END IF
!*
1			CONTINUE
!*............................Mismatches of component initialization:
	WRITE(*,*)'Component identificator misprinting!'
	PAUSE 'Hit RETURN key to take up the problem......'
	NMISTAKE = NMISTAKE + 1
	WRITE(*,100)
	WRITE(*,*)'You can not take account of ',MOLECULE(M),' lines,'
	WRITE(*,*)'since there is no such a component in HITRAN '
	WRITE(*,*)'The number of account components is reduced for 1'
	WRITE(*,*)'Enter (1) to continue calculation'
	WRITE(*,*)'	or (0) to terminate a programm:'
	READ(*,*)ITERMINATE
			IF(ITERMINATE == 0)STOP
!*
11	CONTINUE
!*............................There are no these gases in HITRAN:
	NGAS = NGAS - NMISTAKE
	IF(IPRESENT == 0)THEN
	WRITE(*,100)
	WRITE(*,*)' There are no account components in HITRAN database'
					PAUSE
					RETURN
				END IF
!*
!*......................................................................
!*			START OF TAU CALCULATION
!*......................................................................
!*---------------------------------------------------------------------
!###	WRITE(*,100)
!###	WRITE(*,*)'	***** LINE-BY-LINE CALCULATIONS ******	'
!###	WRITE(*,100)
		END IF
!*
		VS=VSTART
!###	WRITE(*,*)'<<< L-by-L :',VS,' cm**(-1) >>>'
OBRR=OBR
CALL CATAL2016(VS,MOLECULE,NGAS,OBRR,OBR25)

!*....................Cycle on atmosphere MAC calc, levels:

NW=(VS+1.D0)/10.D0
				DO 3 JLEV=1,JMAX
!*				******************
!### JL=JLEV-1 ; IF(JL/10*10==JL)WRITE(*,*)JLEV,JMAX,' NW ',NW        !#######################
N_4=NT*4
OPEN(491,ACCESS='DIRECT', FORM='UNFORMATTED',	&
 RECL = N_4, FILE='./PT_CALC/'//DIR_NAME(JLEV)//'PT') ! NT*4 for LINUX FORTRANs
RK=0.  ; RK0P=0. ; RK0L=0. ; RK0=0. ; RK1P=0. ; RK1L=0. ;RK1=0. ; RK2P=0. ; RK2L=0. ;RK2=0. ; RK3P=0. ; RK3L=0. ; RK3=0.
               RK4P=0. ; RK4L=0. ; RK4=0. ; RK5P=0. ; RK5L=0. ;RK5=0. ; RK6P=0. ; RK6L=0. ;RK6=0. ; RK7P=0. ; RK7L=0. ; RK7=0.
               RK8P=0. ; RK8L=0. ; RK8=0. ; RK9P=0. ; RK9L=0. ;RK9=0.    
	P_P	= P1(JLEV)
	T_T	= T1(JLEV)
		DO 4 IG=1,NGAS
	RO_RO=RO1(IG,JLEV) !/2.!
	CALL LBL2016( MOLECULE(IG),IG,VS)
 4	CONTINUE
            VR4=VS

!*						*** SUMMARISING		***
			DO J = 1,NT0
			I=J*2-1
			RK1P(I)=RK1P(I)+RK0P(J)
			RK1(I) =RK1(I)+RK0P(J)*0.375+RK0(J)*0.75-RK0L(J)*0.125
			RK1L(I)=RK1L(I)+RK0(J)
			M=I+1
			RK1P(M)=RK1P(M)+RK0(J)
			RK1(M) =RK1(M)+RK0L(J)*0.375+RK0(J)*0.75-RK0P(J)*0.125
			RK1L(M)=RK1L(M)+RK0L(J)
			END DO
!*
			DO J = 1,NT1
			I=J*2-1
			RK2P(I)=RK2P(I)+RK1P(J)
			RK2(I) =RK2(I)+RK1P(J)*0.375+RK1(J)*0.75-RK1L(J)*0.125
			RK2L(I)=RK2L(I)+RK1(J)
			M=I+1
			RK2P(M)=RK2P(M)+RK1(J)
			RK2(M) =RK2(M)+RK1L(J)*0.375+RK1(J)*0.75-RK1P(J)*0.125
			RK2L(M)=RK2L(M)+RK1L(J)
				END DO
!*
			DO J = 1,NT2
			I=J*2-1
			RK3P(I)=RK3P(I)+RK2P(J)
			RK3(I) =RK3(I)+RK2P(J)*0.375+RK2(J)*0.75-RK2L(J)*0.125
			RK3L(I)=RK3L(I)+RK2(J)
			M=I+1
			RK3P(M)=RK3P(M)+RK2(J)
			RK3(M) =RK3(M)+RK2L(J)*0.375+RK2(J)*0.75-RK2P(J)*0.125
			RK3L(M)=RK3L(M)+RK2L(J)
				END DO
!*
			DO J = 1,NT3
			I=J*2-1
			RK4P(I)=RK4P(I)+RK3P(J)
			RK4(I) =RK4(I)+RK3P(J)*0.375+RK3(J)*0.75-RK3L(J)*0.125
			RK4L(I)=RK4L(I)+RK3(J)
			M=I+1
			RK4P(M)=RK4P(M)+RK3(J)
			RK4(M) =RK4(M)+RK3L(J)*0.375+RK3(J)*0.75-RK3P(J)*0.125
			RK4L(M)=RK4L(M)+RK3L(J)
				END DO
!*
			DO J = 1,NT4
			I=J*2-1
			RK5P(I)=RK5P(I)+RK4P(J)
			RK5(I) =RK5(I)+RK4P(J)*0.375+RK4(J)*0.75-RK4L(J)*0.125
			RK5L(I)=RK5L(I)+RK4(J)
			M=I+1
			RK5P(M)=RK5P(M)+RK4(J)
			RK5(M) =RK5(M)+RK4L(J)*0.375+RK4(J)*0.75-RK4P(J)*0.125
			RK5L(M)=RK5L(M)+RK4L(J)
				END DO
!*
			DO J = 1,NT5
			I=J*2-1
			RK6P(I)=RK6P(I)+RK5P(J)
			RK6(I) =RK6(I)+RK5P(J)*0.375+RK5(J)*0.75-RK5L(J)*0.125
			RK6L(I)=RK6L(I)+RK5(J)
			M=I+1
			RK6P(M)=RK6P(M)+RK5(J)
			RK6(M) =RK6(M)+RK5L(J)*0.375+RK5(J)*0.75-RK5P(J)*0.125
			RK6L(M)=RK6L(M)+RK5L(J)
				END DO
			DO J = 1,NT6
			I=J*2-1
			RK7P(I)=RK7P(I)+RK6P(J)
			RK7(I) =RK7(I)+RK6P(J)*0.375+RK6(J)*0.75-RK6L(J)*0.125
			RK7L(I)=RK7L(I)+RK6(J)
			M=I+1
			RK7P(M)=RK7P(M)+RK6(J)
			RK7(M) =RK7(M)+RK6L(J)*0.375+RK6(J)*0.75-RK6P(J)*0.125
			RK7L(M)=RK7L(M)+RK6L(J)
				END DO
!*
			DO J = 1,NT7
			I=J*2-1
			RK8P(I)=RK8P(I)+RK7P(J)
			RK8(I) =RK8(I)+RK7P(J)*0.375+RK7(J)*0.75-RK7L(J)*0.125
			RK8L(I)=RK8L(I)+RK7(J)
			M=I+1
			RK8P(M)=RK8P(M)+RK7(J)
			RK8(M) =RK8(M)+RK7L(J)*0.375+RK7(J)*0.75-RK7P(J)*0.125
			RK8L(M)=RK8L(M)+RK7L(J)
				END DO
!*
			DO J = 1,NT8
			I=J*2-1
			RK9P(I)=RK9P(I)+RK8P(J)
			RK9(I) =RK9(I)+RK8P(J)*0.375+RK8(J)*0.75-RK8L(J)*0.125
			RK9L(I)=RK9L(I)+RK8(J)
			M=I+1
			RK9P(M)=RK9P(M)+RK8(J)
			RK9(M) =RK9(M)+RK8L(J)*0.375+RK8(J)*0.75-RK8P(J)*0.125
			RK9L(M)=RK9L(M)+RK8L(J)
				END DO
!*
			I=1
			DO J = 1,NT9
			I=I+1
	RK(I) =RK(I)+(RK9P(J)*0.375+RK9(J)*0.75-RK9L(J)*0.125)
			I=I+1
	RK(I)=RK(I)+RK9(J)
			I=I+1
	RK(I) =RK(I)+(RK9L(J)*0.375+RK9(J)*0.75-RK9P(J)*0.125)
			I=I+1
	RK(I)=RK(I)+RK9L(J)
			END DO

!*		van Vleck-Weisskopf-  Huber factor !!!	*

IF(VS>=2000.) THEN
				JM1=0 
			    DO J =1,NT
                        FACTV=VR4+H*JM1
                                    RK(J)=RK(J)*FACTV 
					if(rk(j) < 0.)rk(j)=0.
					JM1=JM1+1
				END DO
ELSE
				JM1=0 
				DO J =1,NT
                                        VIVI=VS+H*JM1
EVV_=VIVI*1.438786/T_T
IF(VIVI<0.1)THEN
FACTV=VIVI*EVV_/(2.-EVV_)
ELSE
EVV=EXP(-EVV_)
FACTV=VIVI *(1.-EVV)/(1.+EVV)
END IF
                                    RK(J)=RK(J)*FACTV 
					if(rk(j) < 0.)rk(j)=0.
					JM1=JM1+1
				  END DO
END IF
WRITE(491,REC=NW)RK
CLOSE(491)
!*......................END OF JLEV-CYCLE ON MAC-LEVELS:
3	CONTINUE
!*
 100 FORMAT(/80(1H-))
	END


