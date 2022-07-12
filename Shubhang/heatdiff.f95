program heatdiff
  implicit none
  integer :: i, j, k
  integer :: m, n
  integer :: L, H
  integer :: T1, T2, T3, T4
  real :: A(5,5), B(5,5), C(5,5)
  T1=300
  T2=100
  T3=50
  T4=75
  m=5
  n=5
  do 10 j=1,n
   do 20 i=1,m 
    C(i,j)=0
    B(i,j)=0
    A(i,j)=0
    if (j==1) then
      A(i,j)=50
     else if (j==n) then
      A(i,j)=300
     else if (i==1) then
      A(i,j)=75
     else if (i==m) then
      A(i,j)=100
     else
      A(i,j)=0
    end if
   20 end do
  10 end do
  do 70 k=1,10
   do 50 j=2,n-1
    do 60 i=2,m-1
     A(i,j)=(A(i+1,j)+A(i-1,j)+A(i,j+1)+A(i,j-1))*(0.25)
     C(i,j)=B(i,j)/A(i,j)
     C(i,j)=1-C(i,j)
    60 end do
   50 end do
   open(1, file = 'data1.dat', status = 'new')
   do 30 j=1,n
    do 40 i=1,m
     write(1,*) i,j,A(i,j)
     B(i,j)=A(i,j)
    40 end do
   30 end do
  70 end do  
end program heatdiff  
  