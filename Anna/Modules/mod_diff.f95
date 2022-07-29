module mod_diff  !defines the module definition and specifies its name
    
    use iso_fortran_env, only: int32,real32
    implicit none

contains

!!Function to compute the finite diffrerence of an input array

    

    !!!!!FORWARD SCHEME 1D!!!!!
    pure function diff_forw(x) result(dx)
        real(real32),intent(in)::x(:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x))  !The result would be real array of the sam
        integer(int32)::im

        im=size(x)
        dx(im)=x(1)-x(im)   !Calculate the boundary value
        dx(1:im-1)=x(2:im)-x(1:im-1)  !Calculate the finite difference for all other elements of x
    end function diff_forw



    !!!!!UPWIND SCHEME  1D!!!!!
    pure function diff_upwind(x) result(dx)
        real(real32),intent(in)::x(:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x))  !The result would be real array of the sam
        integer(int32)::im

        im=size(x)
        dx(1)=x(1)-x(im)   !Calculate the boundary value
        dx(2:im)=x(2:im)-x(1:im-1)  !Calculate the finite difference for all other elements of x
    end function diff_upwind


    
    !!!!!CENTERED DIFFERENCE SCHEME  1D!!!!!
    pure function diff_centered(x) result(dx)
        real(real32),intent(in)::x(:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x))  !The result would be real array of the sam
        integer(int32)::im

        im=size(x)
        dx(1)=(x(2)-x(im))  !Calculate the boundary value on the left
        dx(im)=(x(1)-x(im-1)) !Calculate the boundary value on the right
        dx(2:im-1)=(x(3:im)-x(1:im-2))  !Calculate the finite difference for all other elements of x
        dx=dx*0.5 !Divides all element by 2
    end function diff_centered



    !!!!!CENTERED DIFFERENCE SCHEME  1D(for double derivative)!!!!!
    pure function diff_cent(x) result(dx)
        real(real32),intent(in)::x(:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x))  !The result would be real array of the sam
        integer(int32)::im

        im=size(x)
        dx(1)=(x(2)-2*x(1)+x(im))  !Calculate the boundary value on the left
        dx(im)=(x(1)-2*x(im)+x(im-1)) !Calculate the boundary value on the right
        dx(2:im-1)=(x(3:im)-2*x(2:im-1)+x(1:im-2))  !Calculate the finite difference for all other elements of x

    end function diff_cent



    !!!!!CENTERED DIFFERENCE SCHEME  2D x-direction!!!!!
    pure function diffx_centered(x) result(dx)
        real(real32),intent(in)::x(:,:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x,dim=1),size(x,dim=2))  !The result would be real array of the sam
        integer(int32)::i,im

        im=size(x,dim=1)
        dx(1,:)=(x(2,:)-x(im,:))  !Calculate the boundary value on the left
        dx(im,:)=(x(1,:)-x(im-1,:)) !Calculate the boundary value on the right
        dx(2:im-1,:)=(x(3:im,:)-x(1:im-2,:))  !Calculate the finite difference for all other elements of x
        dx=0.5*dx
    end function diffx_centered

    !!!!!FORWARD SCHEME 2D x-direction!!!!!
    pure function diffx_forw(x) result(dx)
        real(real32),intent(in)::x(:,:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x,dim=1),size(x,dim=2))  !The result would be real array of the sam
        integer(int32)::i,im

        im=size(x,dim=1)
        dx(im,:)=x(1,:)-x(im,:)   !Calculate the boundary value
        dx(1:im-1,:)=x(2:im,:)-x(1:im-1,:)  !Calculate the finite difference for all other elements of x
    end function diffx_forw



    !!!!!UPWIND SCHEME  2D x-direction!!!!!
    pure function diffx_upwind(x) result(dx)
        real(real32),intent(in)::x(:,:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x,dim=1),size(x,dim=2))  !The result would be real array of the sam
        integer(int32)::im

        im=size(x,dim=1)
        dx(1,:)=x(1,:)-x(im,:)   !Calculate the boundary value
        dx(2:im,:)=x(2:im,:)-x(1:im-1,:)  !Calculate the finite difference for all other elements of x
    end function diffx_upwind

    !!!!!CENTERED DIFFERENCE SCHEME  2D x-direction(for double derivative)!!!!!
    pure function diffx_cent(x) result(dx)
        real(real32),intent(in)::x(:,:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x,dim=1),size(x,dim=2))  !The result would be real array of the sam
        integer(int32)::im

        im=size(x,dim=1)
        dx(1,:)=(x(2,:)-2*x(1,:)+x(im,:))  !Calculate the boundary value on the left
        dx(im,:)=(x(1,:)-2*x(im,:)+x(im-1,:)) !Calculate the boundary value on the right
        dx(2:im-1,:)=(x(3:im,:)-2*x(2:im-1,:)+x(1:im-2,:))  !Calculate the finite difference for all other elements of x

    end function diffx_cent

    
!!!!!CENTERED DIFFERENCE SCHEME  2D y-direction!!!!!
    pure function diffy_centered(x) result(dx)
        real(real32),intent(in)::x(:,:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x,dim=1),size(x,dim=2))  !The result would be real array of the sam
        integer(int32)::j,jm

        jm=size(x,dim=2)
        dx(:,1)=(x(:,2)-x(:,jm))  !Calculate the boundary value on the left
        dx(:,jm)=(x(:,1)-x(:,jm-1)) !Calculate the boundary value on the right
        dx(:,2:jm-1)=(x(:,3:jm)-x(:,1:jm-2))  !Calculate the finite difference for all other elements of x
        dx=0.5*dx
    end function diffy_centered

    !!!!!FORWARD SCHEME 2D y-direction!!!!!
    pure function diffy_forw(x) result(dx)
        real(real32),intent(in)::x(:,:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x,dim=1),size(x,dim=2))  !The result would be real array of the sam
        integer(int32)::j,jm

        jm=size(x,dim=2)
        dx(:,jm)=x(:,1)-x(:,jm)   !Calculate the boundary value
        dx(:,1:jm-1)=x(:,2:jm)-x(:,1:jm-1)  !Calculate the finite difference for all other elements of x
    end function diffy_forw



    !!!!!UPWIND SCHEME  2D y-direction!!!!!
    pure function diffy_upwind(x) result(dx)
        real(real32),intent(in)::x(:,:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x,dim=1),size(x,dim=2))  !The result would be real array of the sam
        integer(int32)::jm

        jm=size(x,dim=2)
        dx(:,1)=x(:,1)-x(:,jm)   !Calculate the boundary value
        dx(:,2:jm)=x(:,2:jm)-x(:,1:jm-1)  !Calculate the finite difference for all other elements of x
    end function diffy_upwind

    !!!!!CENTERED DIFFERENCE SCHEME  2D y-direction(for double derivative)!!!!!
    pure function diffy_cent(x) result(dx)
        real(real32),intent(in)::x(:,:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x,dim=1),size(x,dim=2))  !The result would be real array of the sam
        integer(int32)::jm

        jm=size(x,dim=2)
        dx(:,1)=(x(:,2)-2*x(:,1)+x(:,jm))  !Calculate the boundary value on the left
        dx(:,jm)=(x(:,1)-2*x(:,jm)+x(:,jm-1)) !Calculate the boundary value on the right
        dx(:,2:jm-1)=(x(:,3:jm)-2*x(:,2:jm-1)+x(:,1:jm-2))  !Calculate the finite difference for all other elements of x

    end function diffy_cent


end module mod_diff  !Ends the module definition
