module mod_initial

    use iso_fortran_env, only: int32,real32
    implicit none

contains
    !! Subroutine to initialize the input array to a Gaussian shape
    pure subroutine set_gaussian(x,icenter,decay)
        real(real32),intent(in out)::x(:) !1D array as input(and output)argument
        !!input parameters for pertubation psition and shape
        integer(int32) ,intent(in)::icenter
        real(real32),intent(in)::decay
        integer(int32)::i
        do concurrent( i=1:size(x) )        !loops over all elements,from index1 to grid_size
            x(i)=exp(-decay*(i-icenter)**2)
        end do 
    end subroutine set_gaussian

     !! Subroutine to initialize the input array to a Gaussian shape
    pure subroutine set_gaussian_2d(x,icenter,jcenter,decay)
        real(real32),intent(in out)::x(:,:) !1D array as input(and output)argument
        !!input parameters for pertubation psition and shape
        integer(int32) ,intent(in)::icenter,jcenter
        real(real32),intent(in)::decay
        integer(int32)::i,j
        integer(int32)::im,in
        im,in=shape(x)
        do concurrent( i=1:im )        !loops over all elements,from index1 to grid_size
            do concurrent( j=1:in )
                x(i,j)=exp(-decay*((i-icenter)+(j-jcenter))**2)
            end do
        end do 
    end subroutine set_gaussian_2d

end module mod_initial