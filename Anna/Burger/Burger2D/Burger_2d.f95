!!An attempt to solve the 2D Burger's equation of the form  dC/dt=-u*dC/dx + K*(d2C/dx2 + d2C/dy2) +q

program Burger_2D

    use iso_fortran_env, only : int32,real32
    use mod_diff,only : diff_upwind, diff_cent
    use mod_initial,only : set_gaussian_2d

    implicit none
    
    integer(int32)::n,i,j
    integer(int32),parameter::grid_size_x=21
    integer(int32),parameter::grid_size_y=21
    integer(int32),parameter::num_time_steps=100
    
    
    !dt =time step[s],dx =grid spacing[m],c =phase (background flow)speed[m/s]
    
    real(real32),parameter::dt =0.5,dx =1,c=1 ,dy=1
        
    !Declares h as a real array with the number of elements equal to grid size
    real(real32)::h(grid_size_x,grid_size_y)
    real(real32)::K(grid_size_x,grid_size_y)=1
    
    
    !!!SETTING THE INITIAL WATER HEIGHT VALUES
    !central index and decay factor of the shape
    integer(int32),parameter::icenter=10
    integer(int32),parameter::jcenter=10
    real(real32),parameter::decay=0.02
    
    if(grid_size_x<=0) stop 'grid_size must be > 0'
    if(grid_size_y<=0) stop 'grid_size must be > 0'
    if(dt<=0) stop 'time step dt must be > 0'
    if(dx<=0) stop 'grid spacing dx must be > 0'
    if(c<=0) stop 'background flow speed c must be > 0'
    
    
    call set_gaussian_2d(h,icenter,jcenter,decay)  !Calls the subroutine to initialize the water height
    print *,0,h
    

    !!!PREDICTING THE MOVEMENT OF THE OBJECT
    Time_loop: do n=1,num_time_steps  !iterates over num_time_steps time steps
        do i=1,grid_size_x-1
            h(i,:)=h(i,:)-c*diff_forw(h(i,:))/dy*dt + K(i,:)*diff_cent(h(i,:))/dy/dy*dt
        end do
        do j=1,grid_size_y-1
            h(:,j)=h(:,j)-c*diff_forw(h(:,j))/dx*dt + K(:,j)*diff_cent(h(:,j))/dx/dx*dt
        end do
        !prints values n and h to the terminal using default formatting
        print *,n,h
        
    end do Time_loop 
    
        


    
end program Burger_2D