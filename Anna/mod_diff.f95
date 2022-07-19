module mod_diff  !defines the module definition and specifies its name
    
    use iso_fortran_env, only: int32,real32
    implicit none

contains

!!Function to compute the finite diffrerence of an input array

    !!!!!UPWIND SCHEME!!!!!
    pure function diff_upwind(x) result(dx)
        real(real32),intent(in)::x(:)  !Assumed-shape real array as input argument
        real(real32)::dx(size(x))  !The result would be real array of the sam
        integer(int32)::im

        im=size(x)
        dx(1)=x(1)-x(im)   !Calculate the boundary value
        dx(2:im)=x(2:im)-x(1:im-1)  !Calculate the finite difference for all other elements of x
    end function diff_upwind
    
    !!!!!CENTERED DIFFERENCE SCHEME!!!!!
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

end module mod_diff  !Ends the module definition
