!!!!!!!An Attempt to solve 1-D Transcient Heat Conduction(THC)!!!!!!!

!! A Parabolic Equation of the form dT/dt=k d2T/dx2

program Transcient_Heat_Conduction

    use iso_fortran_env, only : int32,real32
    
    implicit none

    real(real32),parameter::k=2.17e-4   !Constant k
    real(real32),parameter::h=0.04     !in meters 
    real(real32),parameter::dy=0.001    !y discretization(in m)
    real(real32),parameter::dt=0.002   !time dicretization(in s)
    real(real32),parameter::d=((k*dt)/(dy*dy)) !diffusion number
    integer(int32),parameter::N=h/dy+1   !1st time step 

    real(real32)::error
    real(real32),parameter::epsilon1=1e-3
    real(real32)::iter
    integer(int32)::i
    real(real32)::u(N)

    real(real32)::u_new(N)=0
    u_new(1)=0   !bottom plate boundary condition
    u_new(N)=40   !upper plate boundary condition
    
    error=1
    
    
    !!Loop
    
    

    do while (error>epsilon1) 
        iter=iter+1
        u=u_new

        do i=2,N-1
            u_new(i)=u(i) + d*(u(i+1) -2*u(i) + u(i-1))
            error=max(abs(u(i)-u_new(i)),abs(u(i-1)-u_new(i-1)))
            
        end do
        print *,u_new
        

    end do


    

end program Transcient_Heat_Conduction

