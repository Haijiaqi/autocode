module appsub

    !����ṹ�嶨��
    type chromosome
    integer(kind=1)gene(8)
    integer(kind=1)::length,L
    real(8)memo,R,memo2
    character*16 information
    logical changed
    end type
contains

!�õ��������
subroutine gettestrand(iind,inum)
    double precision genrand_real128
    integer(1)inum
    real(4) iind(inum)
    do j=1,inum,1
        iind(j)=genrand_real128()
    end do
end subroutine

!�õ���������������У�������ʼ������
subroutine getrandarray(igene,imask)
    integer(4)temp,genrand_int32
    integer(1)igene(8)
    integer(1)::imask
    temp=genrand_int32()
    do j=1,8,1
        igene(j)=ishft(temp,4-4*j)
        igene(j)=iand(igene(j),imask)
    end do
end subroutine

!�õ�����������򳤶ȣ�������ʼ������
subroutine setlength(iparents,ipopulation,ilmask)
    integer genrand_int32
    integer(kind=1)ilmask,ipopulation
    type(chromosome) :: iparents(ipopulation)
    integer(kind=8)::temp,i
    logical ::logic=.true.
    do while(logic)
        i=0
        temp=genrand_int32()
        do j=1,8,1
            iparents(j)%length=ishft(temp,4-4*j)
            iparents(j)%length=iand(iparents(j)%length,ilmask)
            i=i+1
            if(i>=ipopulation)then
                logic=.false.
                exit
            end if
        end do
    end do
end subroutine

!�����������
subroutine change(igene,irate,imask,ichanged,information)
    integer genrand_int32
    double precision genrand_real1
    integer(1)igene,imask
    real(8)irate,rands
    integer(4)genechen
    logical ichanged
    character*8 information
    rands=genrand_real1()
    if(rands<=irate) then
        genechen=genrand_int32()
        igene=genechen
        igene=iand(igene,imask)
        ichanged=.true.
        information=trim(information)//"T"
    end if
end subroutine

!ȷ����Ӧ�Ժ����з�ĸ�����
subroutine nozero(idep,inum)
    integer(kind=1)inum
    real(4)idep(inum)
    where (idep==0)
        idep=idep+0.00000001
    end where
endsubroutine

!���򳤶ȱ�������
subroutine lengthchan(ilength,ilrate,ichanged,information)
    double precision genrand_real1
    integer(1)ilength
    real(8)ilrate,lrand
    logical ichanged
    character*8 information
    lrand=genrand_real1()
    if (lrand>=1-ilrate) then
        if(ilength==8) then
            ilength=ilength-1
            information=trim(information)//"���ȼ�1"
        else
            ilength=ilength+1
        endif
        ichanged=.true.
        information=trim(information)//"���ȼ�1"
    end if
    if (lrand<=ilrate)then
        if(ilength==0) then
            ilength=ilength+1
            information=trim(information)//"���ȼ�1"
        else
            ilength=ilength-1
            information=trim(information)//"���ȼ�1"
        endif
        ichanged=.true.
    end if
endsubroutine

!Ⱦɫ������������
subroutine memochan(imrate,imemo,information)
    double precision genrand_real1,genrand_real_8210128
    real(8)imrate,imemo,imrand
    character*8 information,cm
    imrand=genrand_real1()
    if (imrand<=imrate)then
        imemo=genrand_real_8210128()
        write(cm,'(f6.2)')imemo
        information=trim(information)//"����"//cm
    endif
endsubroutine

!��Ӧ�Ժ������ó��������۳ɼ�
subroutine fitness(idep,ires,inum,iRESU,iparents,iscore,igeneration)
    integer(kind=1)inum
    real(4)idep(inum),ires(inum)
    real(8)iRESU,iscore
    type(chromosome) :: iparents
    logical igeneration
    !׼����Ӧ������
    call nozero(idep,inum)
    !������Ӧ������
    ires=ires/idep-1
    !������Ӧ������
    iRESU=dot_product(ires,ires)+iparents%L/1000.0
    if (iRESU<=((iparents%L+0.001)/1000.0))igeneration=.true.
    iparents%R=iRESU
    iscore=iRESU
endsubroutine

!��ֳ����
subroutine produce(iparents,iscore,ipopulation)
    integer(1)ipopulation,i
    type(chromosome) :: iparents(ipopulation),tparents
    real(8)::iscore(ipopulation)
    iparents(maxloc(iscore))=iparents(minloc(iscore))
    do i=1,ipopulation,1
        iparents(i)%information=" "
    end do
endsubroutine

!�����趨����׼����
subroutine environment(iind,idep,inum)
    integer(1)inum
    real(4)::iind(inum),idep(inum)
    f(x)=x**2+103
    call gettestrand(iind,inum)
    forall(i=1:inum)
        idep(i)=f(iind(i))
    end forall
endsubroutine

!�趨�����ģ������
subroutine getsize(inum,ipopulation,irate,ilrate,imrate,im)
    integer(kind=1)::inum,ipopulation
    real(8)::irate,ilrate,imrate
    print*,"���������������"//"num = 5"
    !read*,inum
    inum = 5
    print*,"��������Ⱥ������"//"population = 8"
    !read*,ipopulation
    ipopulation = 8
    print*,"�������������ʣ�"//"irate = 0.02"
    !read*,irate
    irate = 0.2
    print*,"�����뵥�򳤶ȱ����ʣ�"//"ilrate = 0.2"
    !read*,ilrate
    ilrate = 0.125
    print*,"�������������ʣ�"//"mrate = 0.4"
    !read*,imrate
    imrate = 0.4
    print*,"������������������"//"m = 100000"
    !read*,inum
    im = 100000
endsubroutine

!���ڳ�ʼ��Ⱦɫ��
subroutine initfirst(iparents,ipopulation,imask)
    double precision genrand_real_8210128
    integer(1)ipopulation,imask
    type(chromosome) :: iparents(ipopulation)
    do j=1,ipopulation,1
        call getrandarray(iparents(j)%gene,imask)
        iparents(j)%changed=.false.
        iparents(j)%information=" "
        iparents(j)%memo=genrand_real_8210128()
    end do
endsubroutine

!�����������
subroutine parentsprint(iparents)
    type(chromosome) :: iparents
    character(len=64)::c=" ",brackets=" ",cm,ct
    integer B
    c=" "
    brackets=" "
    ct=" "
    write(cm,'(f6.2)')iparents%memo

    do i=1,iparents%length,1
            B=iparents%gene(i)
            !��������
            select case (B)
            case (0)
                if (k==num) L=L-1
                cycle
            case (1)
!                R=R+IO
!                T=R
                c=trim(c)//"+x)"
                brackets=trim(brackets)//"("
            case (2)
!                R=R*IO
!                T=R
                c=trim(c)//"*x)"
                brackets=trim(brackets)//"("
            case (3)
!                R=R+parents(j)%memo
!                T=R
                c=trim(c)//"+"//trim(cm)//")"
                brackets=trim(brackets)//"("
            case (4)
!                R=R*parents(j)%memo
!                T=R
                c=trim(c)//"*"//trim(cm)//")"
                brackets=trim(brackets)//"("
            case (5)
!                R=R+R
!                T=R
                c=trim(c)//"*2)"
                brackets=trim(brackets)//"("
            case (6)
!                R=R*R
!                T=R
                c=trim(c)//"^2)"
                brackets=trim(brackets)//"("
            case (7)
                !T=IO+parents(j)%memo
                ct="(x+"//trim(cm)//")"
            case (8)
                !T=IO*parents(j)%memo
                ct="(x*"//trim(cm)//")"
            case (9)
                !T=IO+IO
                ct="(x*2)"
            case (10)
                !T=IO*IO
                ct="(x^2)"
            case (11)
                !T=parents(j)%memo+parents(j)%memo
                ct="("//trim(cm)//"*2)"
            case (12)
                !T=parents(j)%memo*parents(j)%memo
                ct="("//trim(cm)//"^2)"
            case (13)
                !R=T
                c=ct
            case (14)
                !parents(j)%memo=T
                cm=ct
            case (15)
                !call memochan(mrate,parents(j)%memo)
                write(cm,'(f10.2)')iparents%memo
            case default
                print*,"!!!The base is unable to express::",B
            end select
        end do
        if(iparents%gene(1)<=6.and.iparents%gene(1)/=0)then
            print*,"chromosome: f(x)=",trim(brackets),"0",c
        else
            print*,"chromosome: f(x)=",trim(brackets),c
        endif
        print*," G:",iparents%gene," leng:",iparents%length," Memo:",iparents%memo,"change",iparents%information," C:",iparents%R
end subroutine

!�����õ���Ⱦɫ���ʼ��
subroutine setone(iparents,igene,imemo,ilength,iL,imask)
    type(chromosome) :: iparents
    integer(4)igene,temp
    integer(kind=1)::ilength,iL
    real(8)imemo
    integer(1)::imask
    iparents%length=ilength
    iparents%memo=imemo
    iparents%L=iL
    temp=igene
    do j=1,8,1
        iparents%gene(j)=ishft(temp,4-4*j)
        iparents%gene(j)=iand(iparents%gene(j),imask)
    end do
end subroutine




!����Ϊ����ͬ���������
subroutine init_random_seed()
    integer :: n,clock
    integer,dimension(:),allocatable :: seed
    call random_seed(size=n)
    allocate(seed(n))
    call system_clock(count=clock)
    call lcg(seed,clock,n)
    call random_seed(put=seed)
    deallocate(seed)
end subroutine
subroutine lcg(iseed,iclock,n)
    integer(4) ::i,iseed(n)
    do j=1,n,1
        iclock=mod((iclock * 16807) , huge(i))
        iseed(j)=iclock
    end do
end subroutine

!����ΪMT�������
subroutine init_random()
    !use mtrandom
    integer(4):: clock
    integer(4):: seed(4)
    data seed/4*0/
    call system_clock(count=clock)
    call ini(seed,clock)
    call init_by_array(seed,4)
end subroutine
subroutine ini(iseed,iclock)
    !use mtrandom
    integer(1)::temp
    integer(4) iseed(4)

    do j=1,4,1
        do i=4,4-j+1,-1
            temp=ishft(iclock,8*j-8)
            temp=iand(temp,127)
            iseed(i)=iseed(i)+temp
        end do
    end do
end subroutine


endmodule
