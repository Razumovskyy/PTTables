!******************************************************************
!* This program creates PT-Tables for any Atmospheric Model  *
!******************************************************************
PROGRAM PT_main_HITRAN !  03-08-2022.
USE INITIAL
REAL*8 VSTART,VFINISH,DIAP,V1,V2
PARAMETER(DIAP=10.D0,iout=47) 
CHARACTER FI*20
!********** CALC SETTINGS **********!
OPEN(2001,FILE='CONTROL.PT')
READ(2001,*)V1,V2   ;  write(*,*)V1,V2
READ(2001,2003)FI   ;  write(2001,*)fi
2003 FORMAT(A20)
CLOSE(2001)
!*********** END SETTINGS **************************************************
! ----------------------------------------------------------------- !
 CALL ATM_PROF_READING(FI)
Nc = (V2 - V1 + 1.0) / 10.D0
write(*,*) 'Nc=', Nc

VSTART=V1  
DO nnc=1, Nc 
 VFINISH=VSTART+10.D0
WRITE(*,*)VSTART,VFINISH,V2 
CALL K_COEF_PT(VSTART)
        VSTART = VSTART + DIAP
END DO
!* ---------------------------------- *
    write(*,*) '*** PT ->Congratulations! **** '
	END
