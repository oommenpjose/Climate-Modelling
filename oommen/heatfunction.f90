module boundary
    implicit none
    contains
    real function f_initial(x)
        real,intent(in) :: x
        f_initial=sin(x)
    end function f_initial
end module boundary
module heateqn
    use boundary
    implicit none
    contains
    function solution(n,m,x0,xa,t,k)
        integer,intent(in) :: n,m
        real,intent(in) :: x0,xa,t,k
        real,dimension(n+1,m+1) :: solution
        real,dimension(n+1):: initial,current,result
        real :: dt,dx,s
        real :: p
        integer :: i,j
        dx=(xa-x0)/n
        dt=t/m
        s=k*dt/(dx**2)
        p=x0
        do i=1, n+1
            initial(i)=f_initial(p)
            solution(i,1)=initial(i)
            p=p+dx
        end do
        current=initial
        do i=2, m+1
            solution(1,i)=(1-s)*current(1)+s*current(2)
            do j=2, n
                solution(j,i)=(1-2*s)*current(j)+s*(current(j-1)+current(j+1))
            end do
            solution(n+1,i)=(1-s)*current(n+1)+s*current(n)
            current(:)=solution(:,i)
        end do
    end function solution
end module heateqn
program pde
    use heateqn
    implicit none
    integer:: i,j
    integer,parameter :: n=100,m=3600
    real,parameter :: pi=4.*atan(1.), x0=0, xa=pi, k=0.041, t=120
    real,dimension(n+1,m+1) :: sol
    sol=solution(n,m,x0,xa,t,k)
    open(1, file = 'data1.dat', status = 'new')
    do i=1,101
        write(1,*) sol(:,i)
    end do
    close(1) 
end program pde