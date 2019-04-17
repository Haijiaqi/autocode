!-------------------------------------------------
!引用常用函数模块
use appsub
implicit none
!_______________________声明板块____________________________
!循环变量:i控制基因，j控制个体，k控制环境，m控制代数 |
!generations:控制代数变量                                                 |
!B:base,信使碱基                                                                |
!num:测试项目数                                                                |
!population:种群数量                                                         |
!mask=15:设置碱基种类数的掩码                                       |
!lmask=7:设置染色体长度的掩码                                       |
!L:显性基因数                                                                     |
!rate:基因变异率；lrate:长度变异率；mrate:记忆变异率     |
!genrand_int32:32bits有符号随机数变量                             |
!genrand_int31:32bits无符号随机数变量                             |
!genrand_real1:[0,1]无符号随机数变量                               |
!genrand_real2:[0,1)无符号随机数变量                                |
!genrand_real3:(0,1)无符号随机数变量                                |
!genrand_real128(0,128)无符号随机数变量                         |
!genrand_real_8210128(-128,128)有符号随机数变量           |
!generation:判断收敛的控制逻辑变量                                |
!ifcontinue:接受用户控制的控制逻辑变量                           |
!IO,RESU,T,R:基因表达常量定义                                          |
!-------------------------------------------------
integer(kind=1)::i,j,k,B,num,population,mask=15,lmask=7,L
integer(kind=4)::generations,m
real(8)::rate,lrate,mrate
integer genrand_int32,genrand_int31
double precision genrand_real1,genrand_real2,genrand_real3,genrand_real128,genrand_real_8210128
logical::generation=.false.,ifcontinue=.true.
real(8)::IO,RESU,T,R


!程序开始。。。。。。

!-------------------------------------------------
!创建环境
real(4),allocatable::ind(:),dep(:),res(:)

!-------------------------------------------------
!创建染色体
type(chromosome),allocatable:: parents(:)

!-------------------------------------------------
!创建个体适应值列表
real(8),allocatable::score(:)

!-------------------------------------------------
!初始化随机数种子
call init_random()

!-------------------------------------------------
!设置问题规模
call getsize(num,population,rate,lrate,mrate,m)

!-------------------------------------------------
!创建环境
allocate(ind(num),dep(num),res(num))
!call environment(ind,dep,num)

!-------------------------------------------------
!创建染色体种群
allocate(parents(population),score(population))

!-------------------------------------------------
!初始化第一代种群基因
call initfirst(parents,population,mask)

!-------------------------------------------------
!初始化第一代个体基因长度
call setlength(parents,population,lmask)

!-------------------------------------------------
!开始迭代，最大代数暂设100000
do generations=1,m,1

    !__________________________________________________________________
    !每代个体基因表达、评估
    do j=1,population,1
        L=parents(j)%length!初始化基因表达数
        call environment(ind,dep,num)!创建适应度指标
        RESU=0!运算变量归零

        !_________________________________________________________________
        !多种环境筛选
        do k=1,num,1
            IO=ind(k)!
            R=0        !运算变量归零
            T=0        !

            !_____________________________________________________________________
            !翻译
            do i=1,parents(j)%length,1
                B=parents(j)%gene(i)!信使碱基转录

                !功能连接，基因表达
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
            !记录单种环境表现
            res(k)=R


        end do
        !基因表达长度记录
        parents(j)%L=L
        !综合评价个体适应度并确定是否收敛
        call fitness(dep,res,num,RESU,parents(j),score(j),generation)


    end do
    !如果收敛则退出迭代
    if (generation)exit
    !基因繁殖
    call produce(parents,score,population)
    !个体变异
    do j=1,population,1
        do i=1,parents(j)%length,1
            !基因变异
            call change(parents(j)%gene(i),rate,mask,parents(j)%changed,parents(j)%information)
        end do
        !染色体长度变异
        call lengthchan(parents(j)%length,lrate,parents(j)%changed,parents(j)%information)
    end do


enddo
!至此迭代结束，可以输出结果

!-------------------------------------------------------------
!迭代最优一代输出
do j=1,population,1
    print*
    print*,j,"号个体"
    call parentsprint(parents(j))
end do

!-------------------------------------------------------------
!环境变量输出
print*,ind
print*,dep

!-------------------------------------------------------------
!输出最终代数
print*,"代数：",generations

!-------------------------------------------------------------
!输出评估筛选结果
print*,"评估结果："
print*,"最优个体：","(",minloc(score),")"
print*,"最劣个体：","(",maxloc(score),")"

!-------------------------------------------------------------
!输出最终状态信息
print*,"状态参考信息（是否正常收敛）：",generation


end!程序结束
