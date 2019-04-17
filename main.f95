!-------------------------------------------------
!���ó��ú���ģ��
use appsub
implicit none
!_______________________�������____________________________
!ѭ������:i���ƻ���j���Ƹ��壬k���ƻ�����m���ƴ��� |
!generations:���ƴ�������                                                 |
!B:base,��ʹ���                                                                |
!num:������Ŀ��                                                                |
!population:��Ⱥ����                                                         |
!mask=15:���ü��������������                                       |
!lmask=7:����Ⱦɫ�峤�ȵ�����                                       |
!L:���Ի�����                                                                     |
!rate:��������ʣ�lrate:���ȱ����ʣ�mrate:���������     |
!genrand_int32:32bits�з������������                             |
!genrand_int31:32bits�޷������������                             |
!genrand_real1:[0,1]�޷������������                               |
!genrand_real2:[0,1)�޷������������                                |
!genrand_real3:(0,1)�޷������������                                |
!genrand_real128(0,128)�޷������������                         |
!genrand_real_8210128(-128,128)�з������������           |
!generation:�ж������Ŀ����߼�����                                |
!ifcontinue:�����û����ƵĿ����߼�����                           |
!IO,RESU,T,R:�����ﳣ������                                          |
!-------------------------------------------------
integer(kind=1)::i,j,k,B,num,population,mask=15,lmask=7,L
integer(kind=4)::generations,m
real(8)::rate,lrate,mrate
integer genrand_int32,genrand_int31
double precision genrand_real1,genrand_real2,genrand_real3,genrand_real128,genrand_real_8210128
logical::generation=.false.,ifcontinue=.true.
real(8)::IO,RESU,T,R


!����ʼ������������

!-------------------------------------------------
!��������
real(4),allocatable::ind(:),dep(:),res(:)

!-------------------------------------------------
!����Ⱦɫ��
type(chromosome),allocatable:: parents(:)

!-------------------------------------------------
!����������Ӧֵ�б�
real(8),allocatable::score(:)

!-------------------------------------------------
!��ʼ�����������
call init_random()

!-------------------------------------------------
!���������ģ
call getsize(num,population,rate,lrate,mrate,m)

!-------------------------------------------------
!��������
allocate(ind(num),dep(num),res(num))
!call environment(ind,dep,num)

!-------------------------------------------------
!����Ⱦɫ����Ⱥ
allocate(parents(population),score(population))

!-------------------------------------------------
!��ʼ����һ����Ⱥ����
call initfirst(parents,population,mask)

!-------------------------------------------------
!��ʼ����һ��������򳤶�
call setlength(parents,population,lmask)

!-------------------------------------------------
!��ʼ����������������100000
do generations=1,m,1

    !__________________________________________________________________
    !ÿ��������������
    do j=1,population,1
        L=parents(j)%length!��ʼ����������
        call environment(ind,dep,num)!������Ӧ��ָ��
        RESU=0!�����������

        !_________________________________________________________________
        !���ֻ���ɸѡ
        do k=1,num,1
            IO=ind(k)!
            R=0        !�����������
            T=0        !

            !_____________________________________________________________________
            !����
            do i=1,parents(j)%length,1
                B=parents(j)%gene(i)!��ʹ���ת¼

                !�������ӣ�������
                select case (B)
                case (0)
                    if (k==num) L=L-1
                    cycle
                case (1)
                    R=R+IO
                    T=R
                case (2)
                    R=R*IO
                    T=R
                case (3)
                    R=R+parents(j)%memo
                    T=R
                case (4)
                    R=R*parents(j)%memo
                    T=R
                case (5)
                    R=R+R
                    T=R
                case (6)
                    R=R*R
                    T=R
                case (7)
                    T=IO+parents(j)%memo
                case (8)
                    T=IO*parents(j)%memo
                case (9)
                    T=IO+IO
                case (10)
                    T=IO*IO
                case (11)
                    T=parents(j)%memo+parents(j)%memo
                case (12)
                    T=parents(j)%memo*parents(j)%memo
                case (13)
                    R=T
                case (14)
                    parents(j)%memo=T
                case (15)
                    call memochan(mrate,parents(j)%memo,parents(j)%information)
                case default
                    print*,"!!!The base is unable to express::",B
                end select


            end do
            !��¼���ֻ�������
            res(k)=R


        end do
        !�����ﳤ�ȼ�¼
        parents(j)%L=L
        !�ۺ����۸�����Ӧ�Ȳ�ȷ���Ƿ�����
        call fitness(dep,res,num,RESU,parents(j),score(j),generation)


    end do
    !����������˳�����
    if (generation)exit
    !����ֳ
    call produce(parents,score,population)
    !�������
    do j=1,population,1
        do i=1,parents(j)%length,1
            !�������
            call change(parents(j)%gene(i),rate,mask,parents(j)%changed,parents(j)%information)
        end do
        !Ⱦɫ�峤�ȱ���
        call lengthchan(parents(j)%length,lrate,parents(j)%changed,parents(j)%information)
    end do


enddo
!���˵�������������������

!-------------------------------------------------------------
!��������һ�����
do j=1,population,1
    print*
    print*,j,"�Ÿ���"
    call parentsprint(parents(j))
end do

!-------------------------------------------------------------
!�����������
print*,ind
print*,dep

!-------------------------------------------------------------
!������մ���
print*,"������",generations

!-------------------------------------------------------------
!�������ɸѡ���
print*,"���������"
print*,"���Ÿ��壺","(",minloc(score),")"
print*,"���Ӹ��壺","(",maxloc(score),")"

!-------------------------------------------------------------
!�������״̬��Ϣ
print*,"״̬�ο���Ϣ���Ƿ�������������",generation


end!�������
