module INITIAL ! Information from atmospheric profile
    parameter (NTH = 20481, NCOMP = 49)
	character*80 TITLE
	character*5 MOLECULE(NCOMP)
    integer*4 :: NGAS, JMAX
    real*4 Z(200), P1(200), RO1(10,200), T1(200)
	save Z, P1, RO1, T1
  	contains
  	SUBROUTINE ATM_PROF_READING(NATT)
  		character NATT*20
       OPEN(66,FILE='CONTROL.CHK')
   		OPEN(55, file='./Atmospheres/'//NATT)
		READ(55,'(A)')TITLE
		WRITE(66,*)TITLE
		READ(55,*)NGAS,JMAX
		WRITE(66,*)'The number of gases (NGAS) : ',NGAS
		WRITE(66,*)'The number of levels (JMAX) : ',JMAX
		DO I=1,NGAS
			READ(55,'(A)')MOLECULE(I)
		END DO
		WRITE(66,*)'Account atmosphere gases		: ',	&
				(MOLECULE(I),I=1,NGAS)
Z=0. ; P1=0. ; T1=0. ; RO1=0.
		DO J=1,JMAX
			READ(55,*)Z(J),P1(J),T1(J),( RO1(I,J), I = 1, NGAS )
			WRITE(66,*)Z(J),P1(J),T1(J),( RO1(I,J), I = 1, NGAS )
!***				WRITE(*,*)Z(J),P1(J),T1(J),( RO1(I,J), I = 1, NGAS )
		END DO
		CLOSE(55)
        CLOSE(66)
    end subroutine ATM_PROF_READING
end module INITIAL