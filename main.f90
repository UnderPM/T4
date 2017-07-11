program main
  implicit none
  integer :: npx
  integer :: npy
  integer :: nvx
  integer :: nvy
  integer :: i,i2
  real :: k
  real :: q
  real :: x
  real :: y
  real :: dx
  real :: dy
  real :: dym
  real :: dxm
  real, dimension(10000,10000) :: t
  call reader(x,y,nvx,nvy,k,q)
  npx = nvx + 2
  npy = nvy + 2
  dx = x / nvx
  dy = y / nvy
  dxm = dx / 2
  dym = dy / 2
  call calc(t,dx,dy,dxm,dym,npx,npy,k,q)
  !call export(dx,dxm,dy,dym,nvx,nvy,t)
  open (unit=3 , file ="gmsh.pos")

write(3,*)'View "Temperature" {'
  do i=1,npy
    !print*, "enter"
    do i2=1,npx
        write(3,*)'SQ(',(i2*dx-dxm)-dxm,',',(i*dy-dym)-dym,',0,',(i2*dx-dxm)+dxm,',',(i*dy-dym)-dym,',0,'
        write(3,*) (i2*dx-dxm)+dxm,',',(i*dy-dym)+dym,',0,',(i2*dx-dxm)-dxm,',',(i*dy-dym)+dym,',0)'
        write(3,*)'{',t(i2,i),',',t(i2,i),',',t(i2,i),',',t(i2,i),'};'
    end do
end do
write(3,*)'};'
end program
