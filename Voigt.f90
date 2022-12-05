!****************************************************************************
				FUNCTION VV_LOR(X)
!*---------------------------------------------*
!* Lorentz or Van Vleck + Huber line shape. *
!* If line cut off = 25 and line position less *
!* than 125 (in cm**-1) the VVH is recommended *
!*---------------------------------------------*
	IMPLICIT INTEGER*4 (I-N)
	REAL*8 VI
	COMMON/SHAPE/ SL,AL,ADD,ALAD,VI,MOTYPE,TT,CTF2,SLL,ALAL
!* --------------------------------------------------------------------*
!*		For VV_LOR are used :										*
!* X	- distance from line center VI ( X = V - VI cm**-1 ),	*
!* ALAL - (Lorentz half width)**2,								*
!* SLL	- (intensity*density*half_width*VVH_factor)/pi (see LBL93), *
!* MOTYPE - to define type of the molecule : 2 -CO2, 1 -H2O, 0 -other,*
!* CTF2 - (line cut off)**2 = (25 cm**-1)**2,					*
!* --------------------------------------------------------------------*
	DATA VVH/0./
!* Line cut off *
	VV_LOR=0.
	XX=X*X
!#######			IF(XX > 100..AND.MOTYPE /= 1)RETURN
!#######			IF(XX >= 624.9.AND.MOTYPE == 1)RETURN
!*
!* Lorentz *
	VV_LOR=SLL/(XX+ALAL)
	IF(MOTYPE == 0)RETURN ! LORENTZ for H2O
!*
!* Far wing correction *
	IF(MOTYPE == 2) THEN
!* CO2 - (Kunde et al.) *
	IF(VI > 1000. .OR. XX <= 12.25)RETURN
	IF(VI < 500.)RETURN
	Y=ABS(X)
	VV_LOR=VV_LOR   !#### *EXP(-1.4*SQRT(Y-3.5))
	RETURN
									ELSE

!* H2O - (Clough et al.) *
		VV_LOR=(VV_LOR -SLL/625.)
!*------------------------------------------------------------*
!* Correction by Clough et al. for H2O continuum (foreign) *
!*------------------------------------------------------------*
		IF(XX > 9.)VV_LOR=VV_LOR*(1.-(1.-6.65*EXP(-XX/5625.))*XX/625.)
			RETURN
	END IF
	END
