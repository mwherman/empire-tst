module ENDF_IO

    ! author: Sam Hoblit, NNDC, BNL
    ! this module provides the interface for I/O to ENDF
    ! files. It collects the basic I/O for all MF and
    ! assembles the endf_mat and endf_file data types.
    ! As all arrays are dynamically allocated there are
    ! no preset limits on any arrays - any number of materials
    ! may be in an endf file which may contain any MF/MT.
    ! Once a file is read in the endf_file type contains the
    ! information contained in the entire file and is available
    ! for further processing and/or writing out to a new file.
    ! The main public interface for this module is just the
    ! routines read_endf_file and write_endf_file. All the
    ! type definitions for the various MF files are made available.

    use base_endf_io
    use endf_cov_io
    use endf_mf1_io
    use endf_mf2_io
    use endf_mf3_io
    use endf_mf4_io
    use endf_mf5_io
    use endf_mf6_io
    use endf_mf7_io
    use endf_mf8_io
    use endf_mf9_io
    use endf_mf10_io
    use endf_mf12_io
    use endf_mf13_io
    use endf_mf14_io
    use endf_mf15_io
    use endf_mf23_io
    use endf_mf26_io
    use endf_mf27_io
    use endf_mf28_io
    use endf_mf31_io
    use endf_mf32_io
    use endf_mf33_io
    use endf_mf34_io
    use endf_mf35_io
    use endf_mf40_io

    implicit none

    public

    type endf_mat
        integer mat
        type (endf_mat), pointer :: next
        type (MF_1),  pointer :: mf1
        type (MF_2),  pointer :: mf2
        type (MF_3),  pointer :: mf3
        type (MF_4),  pointer :: mf4
        type (MF_5),  pointer :: mf5
        type (MF_6),  pointer :: mf6
        type (MF_7),  pointer :: mf7
        type (MF_8),  pointer :: mf8
        type (MF_9),  pointer :: mf9
        type (MF_10), pointer :: mf10
        type (MF_12), pointer :: mf12
        type (MF_13), pointer :: mf13
        type (MF_14), pointer :: mf14
        type (MF_15), pointer :: mf15
        type (MF_23), pointer :: mf23
        type (MF_26), pointer :: mf26
        type (MF_27), pointer :: mf27
        type (MF_28), pointer :: mf28
        type (MF_31), pointer :: mf31
        type (MF_32), pointer :: mf32
        type (MF_33), pointer :: mf33
        type (MF_34), pointer :: mf34
        type (MF_35), pointer :: mf35
        type (MF_40), pointer :: mf40
    end type

    type endf_file
        character*80 hdline            ! header line (line 0)
        type (endf_mat) mat            ! materials
    end type

    ! define generic interfaces for read/write MF routines

    interface read_mf
        module procedure read_mf1
        module procedure read_mf2
        module procedure read_mf3
        module procedure read_mf4
        module procedure read_mf5
        module procedure read_mf6
        module procedure read_mf7
        module procedure read_mf8
        module procedure read_mf9
        module procedure read_mf10
        module procedure read_mf12
        module procedure read_mf13
        module procedure read_mf14
        module procedure read_mf15
        module procedure read_mf23
        module procedure read_mf26
        module procedure read_mf27
        module procedure read_mf28
        module procedure read_mf31
        module procedure read_mf32
        module procedure read_mf33
        module procedure read_mf34
        module procedure read_mf35
        module procedure read_mf40
    end interface

    interface write_mf
        module procedure write_mf1
        module procedure write_mf2
        module procedure write_mf3
        module procedure write_mf4
        module procedure write_mf5
        module procedure write_mf6
        module procedure write_mf7
        module procedure write_mf8
        module procedure write_mf9
        module procedure write_mf10
        module procedure write_mf12
        module procedure write_mf13
        module procedure write_mf14
        module procedure write_mf15
        module procedure write_mf23
        module procedure write_mf26
        module procedure write_mf27
        module procedure write_mf28
        module procedure write_mf31
        module procedure write_mf32
        module procedure write_mf33
        module procedure write_mf34
        module procedure write_mf35
        module procedure write_mf40
    end interface

    interface lc_mf
        module procedure lc_mf1
        module procedure lc_mf2
        module procedure lc_mf3
        module procedure lc_mf4
        module procedure lc_mf5
        module procedure lc_mf6
        module procedure lc_mf7
        module procedure lc_mf8
        module procedure lc_mf9
        module procedure lc_mf10
        module procedure lc_mf12
        module procedure lc_mf13
        module procedure lc_mf14
        module procedure lc_mf15
        module procedure lc_mf23
        module procedure lc_mf26
        module procedure lc_mf27
        module procedure lc_mf28
        module procedure lc_mf31
        module procedure lc_mf32
        module procedure lc_mf33
        module procedure lc_mf34
        module procedure lc_mf35
        module procedure lc_mf40
    end interface

    private read_mf1,read_mf2,read_mf3,read_mf4,read_mf5,read_mf6,read_mf7,read_mf8
    private read_mf9,read_mf10,read_mf12,read_mf13,read_mf14,read_mf15,read_mf23,read_mf26
    private read_mf27,read_mf28,read_mf31,read_mf32,read_mf33,read_mf34,read_mf35,read_mf40
    private write_mf1,write_mf2,write_mf3,write_mf4,write_mf5,write_mf6,write_mf7,write_mf8
    private write_mf9,write_mf10,write_mf12,write_mf13,write_mf14,write_mf15,write_mf23,write_mf26
    private write_mf27,write_mf28,write_mf31,write_mf32,write_mf33,write_mf34,write_mf35,write_mf40
    private lc_mf1,lc_mf2,lc_mf3,lc_mf4,lc_mf5,lc_mf6,lc_mf7,lc_mf8,lc_mf9,lc_mf10
    private lc_mf12,lc_mf13,lc_mf14,lc_mf15,lc_mf23,lc_mf26,lc_mf27,lc_mf28,lc_mf31
    private lc_mf32,lc_mf33,lc_mf34,lc_mf35,lc_mf40
    private get_mat, get_mf, get_mt, set_mat, set_mf, set_mt, next_mt, endf_error
    private read_endf, get_endf, write_endf, put_endf, get_endline, put_endline, endline, ipos
    private clear_mat, mtmod, process_material

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_endf_file(filename,endf)

    implicit none

    character*(*), intent(in) :: filename
    type (endf_file), intent(out), target :: endf

    character*80, parameter :: hdlin = ' $Rev::          $  $Date::            $                             1 0  0    0'

    integer mat,status
    type (endf_mat), pointer :: mx

    call open_endfile(filename,.false.)

    ! first line, or "header" line, usually contains SVN tags
    ! and ends with MAT,MF,MT all 0. Look for this and if found,
    ! save this line as the header

    call get_endline

    if(endline(71:80) .eq. hdlin(71:80)) then
        endf%hdline = endline
        endf%hdline(67:80) = hdlin(67:80)    ! reset these fields
        call get_endline
    else
        write(6,*) ' No header line found in ',filename
        write(6,*) ' Header will be set to standard header line with SVN tags'
        endf%hdline = hdlin
    endif

    mx => endf%mat
    mat = get_mat()
    if(mat .lt. 0) then
        write(6,*) ' No materials found in ',filename
        call close_endfile
        return
    endif

    do
        call clear_mat(mx)
        mx%mat = mat
        call set_mat(mat)

        call process_material(mx)

        call get_endline(status)
        if(status .ne. 0) then
            if(status .eq. -1) then
		! hit EOF. Just tell user and close.
		write(6,*) ' Hit EOF when expecting TEND record'
                call close_endfile
                return
            endif
            write(6,*) ' Error returned from READ:',status
            call endf_error
        endif
        mat = get_mat()
        if(mat .eq. -1) then
            ! end-of-tape marker. All done.
            call close_endfile
            return
        else if(mat .gt. 0) then
            ! new material.
            allocate(mx%next)
            mx => mx%next
        else
            write(6,*) ' Undefined MAT number encountered in file:',mat
            call endf_error
        endif
    end do

    end subroutine read_endf_file

!------------------------------------------------------------------------------

    subroutine process_material(mx)

    implicit none

    type (endf_mat), intent(out) :: mx
    integer mf

    ! process material

    do
        mf = get_mf()
        select case(mf)
        case(0)
            return
        case(1)
            allocate(mx%mf1)
            call read_mf1(mx%mf1)
        case(2)
            allocate(mx%mf2)
            call read_mf2(mx%mf2)
        case(3)
            allocate(mx%mf3)
            call read_mf3(mx%mf3)
        case(4)
            allocate(mx%mf4)
            call read_mf4(mx%mf4)
        case(5)
            allocate(mx%mf5)
            call read_mf5(mx%mf5)
        case(6)
            allocate(mx%mf6)
            call read_mf6(mx%mf6)
        case(7)
            allocate(mx%mf7)
            call read_mf7(mx%mf7)
        case(8)
            allocate(mx%mf8)
            call read_mf8(mx%mf8)
        case(9)
            allocate(mx%mf9)
            call read_mf9(mx%mf9)
        case(10)
            allocate(mx%mf10)
            call read_mf10(mx%mf10)
        case(12)
            allocate(mx%mf12)
            call read_mf12(mx%mf12)
        case(13)
            allocate(mx%mf13)
            call read_mf13(mx%mf13)
        case(14)
            allocate(mx%mf14)
            call read_mf14(mx%mf14)
        case(15)
            allocate(mx%mf15)
            call read_mf15(mx%mf15)
        case(23)
            allocate(mx%mf23)
            call read_mf23(mx%mf23)
        case(26)
            allocate(mx%mf26)
            call read_mf26(mx%mf26)
        case(27)
            allocate(mx%mf27)
            call read_mf27(mx%mf27)
        case(28)
            allocate(mx%mf28)
            call read_mf28(mx%mf28)
        case(31)
            allocate(mx%mf31)
            call read_mf31(mx%mf31)
        case(32)
            allocate(mx%mf32)
            call read_mf32(mx%mf32)
        case(33)
            allocate(mx%mf33)
            call read_mf33(mx%mf33)
        case(34)
            allocate(mx%mf34)
            call read_mf34(mx%mf34)
        case(35)
            allocate(mx%mf35)
            call read_mf35(mx%mf35)
        case(40)
            allocate(mx%mf40)
            call read_mf40(mx%mf40)
        case default
            ! unknown MF
            write(6,*) ' Undefined MF encountered:',mf
        end select

        call get_endline
    end do

    end subroutine process_material

!------------------------------------------------------------------------------

    subroutine clear_mat(mr)

    implicit none

    type (endf_mat), intent(out) :: mr

    nullify(mr%next,mr%mf1,mr%mf2,mr%mf3,mr%mf4,mr%mf5,mr%mf6,mr%mf7,mr%mf8)
    nullify(mr%mf9,mr%mf10,mr%mf12,mr%mf13,mr%mf14,mr%mf15,mr%mf23,mr%mf26)
    nullify(mr%mf27,mr%mf28,mr%mf31,mr%mf32,mr%mf33,mr%mf34,mr%mf35,mr%mf40)

    return
    end subroutine clear_mat

!------------------------------------------------------------------------------

    subroutine write_endf_file(filename,endf)

    implicit none

    character*(*), intent(in) :: filename
    type (endf_file), intent(in), target :: endf
    type (endf_mat), pointer :: mx

    call open_endfile(filename,.true.)

    endline = endf%hdline
    call put_endline

    mx => endf%mat

    do while(associated(mx))

        call set_mat(mx%mat)
        call set_mf1_directory(mx)

        if(associated(mx%mf1))  call write_mf(mx%mf1)
        if(associated(mx%mf2))  call write_mf(mx%mf2)
        if(associated(mx%mf3))  call write_mf(mx%mf3)
        if(associated(mx%mf4))  call write_mf(mx%mf4)
        if(associated(mx%mf5))  call write_mf(mx%mf5)
        if(associated(mx%mf6))  call write_mf(mx%mf6)
        if(associated(mx%mf7))  call write_mf(mx%mf7)
        if(associated(mx%mf8))  call write_mf(mx%mf8)
        if(associated(mx%mf9))  call write_mf(mx%mf9)
        if(associated(mx%mf10)) call write_mf(mx%mf10)
        if(associated(mx%mf12)) call write_mf(mx%mf12)
        if(associated(mx%mf13)) call write_mf(mx%mf13)
        if(associated(mx%mf14)) call write_mf(mx%mf14)
        if(associated(mx%mf15)) call write_mf(mx%mf15)
        if(associated(mx%mf23)) call write_mf(mx%mf23)
        if(associated(mx%mf26)) call write_mf(mx%mf26)
        if(associated(mx%mf27)) call write_mf(mx%mf27)
        if(associated(mx%mf28)) call write_mf(mx%mf28)
        if(associated(mx%mf31)) call write_mf(mx%mf31)
        if(associated(mx%mf32)) call write_mf(mx%mf32)
        if(associated(mx%mf33)) call write_mf(mx%mf33)
        if(associated(mx%mf34)) call write_mf(mx%mf34)
        if(associated(mx%mf35)) call write_mf(mx%mf35)
        if(associated(mx%mf40)) call write_mf(mx%mf40)

        call set_mat(0)
        call write_endf(0, 0, 0, 0)

        mx => mx%next

    end do

    call set_mat(-1)
    call write_endf(0, 0, 0, 0)

    call close_endfile

    return
    end subroutine write_endf_file

!------------------------------------------------------------------------------

    subroutine set_mf1_directory(mx)

    implicit none

    type (endf_mat), intent(inout), target :: mx

    integer i,mtc,nxc

    type (mf_1),  pointer :: r1
    type (mf_2),  pointer :: r2
    type (mf_3),  pointer :: r3
    type (mf_4),  pointer :: r4
    type (mf_5),  pointer :: r5
    type (mf_6),  pointer :: r6
    type (mf_7),  pointer :: r7
    type (mf_8),  pointer :: r8
    type (mf_9),  pointer :: r9
    type (mf_10), pointer :: r10
    type (mf_12), pointer :: r12
    type (mf_13), pointer :: r13
    type (mf_14), pointer :: r14
    type (mf_15), pointer :: r15
    type (mf_23), pointer :: r23
    type (mf_26), pointer :: r26
    type (mf_27), pointer :: r27
    type (mf_28), pointer :: r28
    type (mf_31), pointer :: r31
    type (mf_32), pointer :: r32
    type (mf_33), pointer :: r33
    type (mf_34), pointer :: r34
    type (mf_35), pointer :: r35
    type (mf_40), pointer :: r40

    type (MF1_sect_list), pointer :: drc(:), sc(:)

    nxc =  mx%mf1%mt451%nxc
    drc => mx%mf1%mt451%sct

    mtc = 0
    mtc = mtc + lc_mf(mx%mf1)
    mtc = mtc + lc_mf(mx%mf2)
    mtc = mtc + lc_mf(mx%mf3)
    mtc = mtc + lc_mf(mx%mf4)
    mtc = mtc + lc_mf(mx%mf5)
    mtc = mtc + lc_mf(mx%mf6)
    mtc = mtc + lc_mf(mx%mf7)
    mtc = mtc + lc_mf(mx%mf8)
    mtc = mtc + lc_mf(mx%mf9)
    mtc = mtc + lc_mf(mx%mf10)
    mtc = mtc + lc_mf(mx%mf12)
    mtc = mtc + lc_mf(mx%mf13)
    mtc = mtc + lc_mf(mx%mf14)
    mtc = mtc + lc_mf(mx%mf15)
    mtc = mtc + lc_mf(mx%mf23)
    mtc = mtc + lc_mf(mx%mf26)
    mtc = mtc + lc_mf(mx%mf27)
    mtc = mtc + lc_mf(mx%mf28)
    mtc = mtc + lc_mf(mx%mf31)
    mtc = mtc + lc_mf(mx%mf32)
    mtc = mtc + lc_mf(mx%mf33)
    mtc = mtc + lc_mf(mx%mf34)
    mtc = mtc + lc_mf(mx%mf35)
    mtc = mtc + lc_mf(mx%mf40)

    ! make new directory

    allocate(mx%mf1%mt451%sct(mtc))
    sc => mx%mf1%mt451%sct

    ! now we have to step through each MF
    ! this is where polymorphism would come in handy!

    i = 0

    r1 => mx%mf1
    do while(associated(r1))
        i = i + 1
        sc(i)%mf = 1
        sc(i)%mt = r1%mt
        sc(i)%nc = r1%lc
        sc(i)%mod = mtmod(drc,nxc,1,r1%mt)
        r1 => r1%next
    end do

    r2 => mx%mf2
    if(associated(r2)) then
        i = i + 1
        sc(i)%mf = 2
        sc(i)%mt = 151
        sc(i)%nc = r2%lc
        sc(i)%mod = mtmod(drc,nxc,2,151)
    endif

    r3 => mx%mf3
    do while(associated(r3))
        i = i + 1
        sc(i)%mf = 3
        sc(i)%mt = r3%mt
        sc(i)%nc = r3%lc
        sc(i)%mod = mtmod(drc,nxc,3,r3%mt)
        r3 => r3%next
    end do

    r4 => mx%mf4
    do while(associated(r4))
        i = i + 1
        sc(i)%mf = 4
        sc(i)%mt = r4%mt
        sc(i)%nc = r4%lc
        sc(i)%mod = mtmod(drc,nxc,4,r4%mt)
        r4 => r4%next
    end do

    r5 => mx%mf5
    do while(associated(r5))
        i = i + 1
        sc(i)%mf = 5
        sc(i)%mt = r5%mt
        sc(i)%nc = r5%lc
        sc(i)%mod = mtmod(drc,nxc,5,r5%mt)
        r5 => r5%next
    end do

    r6 => mx%mf6
    do while(associated(r6))
        i = i + 1
        sc(i)%mf = 6
        sc(i)%mt = r6%mt
        sc(i)%nc = r6%lc
        sc(i)%mod = mtmod(drc,nxc,6,r6%mt)
        r6 => r6%next
    end do

    r7 => mx%mf7
    do while(associated(r7))
        i = i + 1
        sc(i)%mf = 7
        sc(i)%mt = r7%mt
        sc(i)%nc = r7%lc
        sc(i)%mod = mtmod(drc,nxc,7,r7%mt)
        r7 => r7%next
    end do

    r8 => mx%mf8
    do while(associated(r8))
        i = i + 1
        sc(i)%mf = 8
        sc(i)%mt = r8%mt
        sc(i)%nc = r8%lc
        sc(i)%mod = mtmod(drc,nxc,8,r8%mt)
        r8 => r8%next
    end do

    r9 => mx%mf9
    do while(associated(r9))
        i = i + 1
        sc(i)%mf = 9
        sc(i)%mt = r9%mt
        sc(i)%nc = r9%lc
        sc(i)%mod = mtmod(drc,nxc,9,r9%mt)
        r9 => r9%next
    end do

    r10 => mx%mf10
    do while(associated(r10))
        i = i + 1
        sc(i)%mf = 10
        sc(i)%mt = r10%mt
        sc(i)%nc = r10%lc
        sc(i)%mod = mtmod(drc,nxc,10,r10%mt)
        r10 => r10%next
    end do

    r12 => mx%mf12
    do while(associated(r12))
        i = i + 1
        sc(i)%mf = 12
        sc(i)%mt = r12%mt
        sc(i)%nc = r12%lc
        sc(i)%mod = mtmod(drc,nxc,12,r12%mt)
        r12 => r12%next
    end do

    r13 => mx%mf13
    do while(associated(r13))
        i = i + 1
        sc(i)%mf = 13
        sc(i)%mt = r13%mt
        sc(i)%nc = r13%lc
        sc(i)%mod = mtmod(drc,nxc,13,r13%mt)
        r13 => r13%next
    end do

    r14 => mx%mf14
    do while(associated(r14))
        i = i + 1
        sc(i)%mf = 14
        sc(i)%mt = r14%mt
        sc(i)%nc = r14%lc
        sc(i)%mod = mtmod(drc,nxc,14,r14%mt)
        r14 => r14%next
    end do

    r15 => mx%mf15
    do while(associated(r15))
        i = i + 1
        sc(i)%mf = 15
        sc(i)%mt = r15%mt
        sc(i)%nc = r15%lc
        sc(i)%mod = mtmod(drc,nxc,15,r15%mt)
        r15 => r15%next
    end do

    r23 => mx%mf23
    do while(associated(r23))
        i = i + 1
        sc(i)%mf = 23
        sc(i)%mt = r23%mt
        sc(i)%nc = r23%lc
        sc(i)%mod = mtmod(drc,nxc,23,r23%mt)
        r23 => r23%next
    end do

    r26 => mx%mf26
    do while(associated(r26))
        i = i + 1
        sc(i)%mf = 26
        sc(i)%mt = r26%mt
        sc(i)%nc = r26%lc
        sc(i)%mod = mtmod(drc,nxc,26,r26%mt)
        r26 => r26%next
    end do

    r27 => mx%mf27
    do while(associated(r27))
        i = i + 1
        sc(i)%mf = 27
        sc(i)%mt = r27%mt
        sc(i)%nc = r27%lc
        sc(i)%mod = mtmod(drc,nxc,27,r27%mt)
        r27 => r27%next
    end do

    r28 => mx%mf28
    do while(associated(r28))
        i = i + 1
        sc(i)%mf = 28
        sc(i)%mt = r28%mt
        sc(i)%nc = r28%lc
        sc(i)%mod = mtmod(drc,nxc,28,r28%mt)
        r28 => r28%next
    end do

    r31 => mx%mf31
    do while(associated(r31))
        i = i + 1
        sc(i)%mf = 31
        sc(i)%mt = r31%mt
        sc(i)%nc = r31%lc
        sc(i)%mod = mtmod(drc,nxc,31,r31%mt)
        r31 => r31%next
    end do

    r32 => mx%mf32
    if(associated(r32)) then
        i = i + 1
        sc(i)%mf = 32
        sc(i)%mt = 151
        sc(i)%nc = r32%lc
        sc(i)%mod = mtmod(drc,nxc,32,151)
    endif

    r33 => mx%mf33
    do while(associated(r33))
        i = i + 1
        sc(i)%mf = 33
        sc(i)%mt = r33%mt
        sc(i)%nc = r33%lc
        sc(i)%mod = mtmod(drc,nxc,33,r33%mt)
        r33 => r33%next
    end do

    r34 => mx%mf34
    do while(associated(r34))
        i = i + 1
        sc(i)%mf = 34
        sc(i)%mt = r34%mt
        sc(i)%nc = r34%lc
        sc(i)%mod = mtmod(drc,nxc,34,r34%mt)
        r34 => r34%next
    end do

    r35 => mx%mf35
    do while(associated(r35))
        i = i + 1
        sc(i)%mf = 35
        sc(i)%mt = r35%mt
        sc(i)%nc = r35%lc
        sc(i)%mod = mtmod(drc,nxc,35,r35%mt)
        r35 => r35%next
    end do

    r40 => mx%mf40
    do while(associated(r40))
        i = i + 1
        sc(i)%mf = 40
        sc(i)%mt = r40%mt
        sc(i)%nc = r40%lc
        sc(i)%mod = mtmod(drc,nxc,40,r40%mt)
        r40 => r40%next
    end do

    if(i .ne. mtc) write(6,*) ' ERROR COUNTING DIRECTORIES'

    ! set line count & add to first section, MF1/451

    mx%mf1%mt451%nxc = mtc
    sc(1)%nc = sc(1)%nc + mtc

    ! let go of old directory

    deallocate(drc)

    return
    end subroutine set_mf1_directory

!------------------------------------------------------------------------------

    integer function mtmod(drc,nx,mf,mt)

    implicit none

    type (MF1_sect_list), intent(in) :: drc(*)
    integer, intent(in) :: nx,mf,mt

    integer i, imod

    imod = 1
    do i = 1,nx
        if(mf .ne. drc(i)%mf) cycle
        if(mt .ne. drc(i)%mt) cycle
        imod = drc(i)%mod
        exit
    end do

    mtmod = imod

    return
    end function mtmod

end module ENDF_IO
