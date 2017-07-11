subroutine reader(x,y,nvx,nvy,k,q)
    implicit none
    integer :: nvx
    integer :: nvy
    real :: k
    real :: q
    real :: x
    real :: y
    open(file="dados.dat",unit=10)
    read(10,*)x
    read(10,*)y
    read(10,*)nvx
    read(10,*)nvy
    read(10,*)k
    read(10,*)q
end subroutine
