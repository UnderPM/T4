subroutine calc(t,dx,dy,dxm,dym,npx,npy,k,q)
  implicit none
  real, dimension(10000,10000) :: t0
  real, dimension(10000,10000), intent(out) :: t
  real, dimension(10000,10000) :: erro
  real, intent(in) :: dx
  real, intent(in) :: dxm
  real, intent(in) :: dy
  real, intent(in) :: dym
  real, intent(in) :: q
  real, intent(in) :: k
  integer, intent(in) :: npx
  integer, intent(in) :: npy
  real :: an
  real :: as
  real :: ae
  real :: aw
  real :: ap
  real :: b
  integer :: c1 = 1
  integer :: c2 = 1

  do c1=1,npy
    do c2=1, npx
        t(c2,c1) = 0
    end do
  end do
  do c1=1,npx
    t(c1,1) = 400
    t(c1,npy) = 100
  end do
  101 continue
    do c1=2,npy-1
      do c2=1,npx
        aw = k * dy/dxm
        ae = k/dxm
        an = k*dx/dym
        as = k*dx/dym
        b = 0
        if(c2 == 1)then
            aw = 0
            b = dy * q
        elseif(c2 == npx) then
            ae = 0
            b = 0
        end if
        ap = ae + aw + as + an
        t0(c2,c1) = t(c2,c1)
        t(c2,c1) = ((ae*t(c2+1,c1))+(aw*t(c2-1,c1))+(as*t(c2,c1-1))+(an*t(c2,c1+1)) + b)/ap
        erro(c2,c1) = abs((t(c2,c1)-t0(c2,c1))/t(c2,c1))
      end do
    end do
    do c1=2,npy-1
        do c2=1,npx
            if(erro(c2,c1) >= 1e-6)then
                goto 101
            end if
        end do
    end do
  do c1=2,npy-1
    t(1,c1) = t(2,c1) - (q*dxm/k)
    t(npx,c1) = t(npx-1,c1) + (q*dxm/k)
  end do
end subroutine
