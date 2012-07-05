    real*8 function mu_bar(awr, npt, scl, cmp, crs)

    implicit none

    integer*4, intent(in) :: npt             ! number of theta points
    real*8, intent(in)    :: awr             ! mass of target/neutron mass
    real*8, intent(in)    :: scl             ! scale factor for shape elastic diff crs (ELAred)
    real*8, intent(in)    :: cmp             ! compound elastic cross section (ELCncs)
    real*8, intent(in)    :: crs(*)          ! shape elastic cross sections (npt) (elada in main.f)

    real*8, parameter :: pi = 3.1415926535897932384626433

    ! it is assumed that these shape elastic diff crs sections are from
    ! empire where they are in the CM frame uniformly spaced from 0 -180 deg.
    ! Here we transform them back to lab using the formalism from Andrej's
    ! mu-bar code, which uses non-relativistic formalism. Here I have
    ! simplified the formulas for the case of Q=0. In this case beta
    ! becomes energy independent and simply awr. In empire, the shape elastic
    ! is scaled by ELAred and the compound elastic is flat. For convenience
    ! for calling from empire, these parameters are passed separately.

    integer*4 i
    real*8 dan,bet,acm,sbt,qbt,s0,s1,p0j,p0k,p1j,p1k,dj,jak
    real*8, allocatable :: labcos(:), labcrs(:)

    allocate(labcos(npt),labcrs(npt))

    dan = pi/real(npt-1)
    bet = awr

    ! first convert from cm to lab

    do i = 1,npt
         acm = cos(real(i-1)*dan)
         sbt = bet*(bet + 2.D0*acm) + 1.D0
         qbt = sqrt(sbt)
         jak = (sbt*qbt)/(bet*bet*(bet+acm))
         labcos(i) = (1.D0 + bet*acm)/qbt
         labcrs(i) = jak*(scl*crs(i) + cmp)
    end do

    ! now get mu-bar, the ave cos of reaction in lab
    ! this assumes linear interp in crs vs cos_theta

    s0 = 0.D0
    s1 = 0.D0
    p0j = labcrs(1)
    p1j = labcrs(1)*labcos(1)
    do i = 2,npt
        p0k = p0j
        p1k = p1j
        dj = labcos(i) - labcos(i-1)
        p0j = labcrs(i)
        p1j = labcrs(i)*labcos(i)
        s0 = s0 + (p0k+p0j)*dj/2.D0
        s1 = s1 + (p1k+p1j)*dj/2.D0
    end do

    deallocate(labcos,labcrs)

    if(s0 /= 0.D0) then
        mu_bar = s1/s0
    else
        mu_bar = 0.D0
    endif

    return
    end 
