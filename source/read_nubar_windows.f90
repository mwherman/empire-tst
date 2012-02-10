!cc   * $Rev: 2542 $
!cc   * $Author: rcapote $
!cc   * $Date: 2012-02-10 17:20:47 +0100 (Fr, 10 Feb 2012) $

module endf_nubars

    implicit none

    private

    real*8, parameter :: eniu(20) = (/1.D-11, 1.D0, 3.D0, 4.D0, 5.7D0, 7.D0, 10.D0, 14.7D0, &
        20.D0, 22.D0, 24.D0, 26.D0, 28.D0, 30.D0, 35.D0, 40.D0,45.D0, 50.D0, 55.D0, 60.D0/)

    real*8, parameter :: vniu(20) = (/2.05D0, 2.127D0, 2.263D0, 2.4023D0, 2.64D0, 2.996D0, 3.37D0, 3.97D0, 4.79D0, &
      5.052D0, 5.2731D0, 5.5143D0, 5.7053D0, 5.9263D0, 6.4284D0, 6.8801D0, 7.3217D0, 7.7434D0, 8.1242D0, 8.5053D0/)

    public read_nubar, fniu_nubar_eval

    contains

!--------------------------------------------------------------------------------------------

    subroutine read_nubar(infile, nin, a, z, ierr)
 
    implicit none

    character*(*), intent(in)  :: infile        ! input ENDF file containing nubars (MF1/456)
    integer*4,     intent(in)  :: nin           ! # chars in infile
    real*8,        intent(in)  :: a,z           ! A,Z for MAT to read in
    integer*4,     intent(out) :: ierr          ! output status flag: 0=success, 1= no nubars with A,Z found

    integer*4 i
    character*1 chr

    ! dummy routine here

    chr = infile(1:1)
    i = nin
    i = nint(1000.D0*z + a)
    ierr = 0
 
    return
    end subroutine read_nubar

!--------------------------------------------------------------------------------------------

    real*8 function fniu_nubar_eval(en)

    implicit none

    real*8, intent(in) :: en

    integer*4 i

      if(en < 1.0D-11) then
        fniu_nubar_eval = vniu(1)
        return
      endif

      IF(En > 60.D0) THEN
        WRITE(8,*)' ERROR: Einc > 60 MeV in NUBAR calculation'
        STOP ' ERROR: Einc > 60 MeV in NUBAR calculation'
      ENDIF
 
      DO i = 2, 20
        IF(eniu(i) >= En) EXIT
      ENDDO

      FNIU_NUBAR_EVAL = vniu(i-1) + (vniu(i) - vniu(i-1))*(En - eniu(i-1))/(eniu(i) - eniu(i-1))
 
    return
    end function fniu_nubar_eval

end module endf_nubars
