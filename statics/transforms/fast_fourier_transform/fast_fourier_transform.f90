!http://rosettacode.org/wiki/Fast_Fourier_transform#Fortran
module fast_fourier_transform

    ! Module to calculate the fast fourier transform 

    implicit none

    private 

    public :: fft

    integer,       parameter :: dp = selected_real_kind(15, 300)
    real(kind=dp), parameter :: pi1 = 3.141592653589793238460_dp

  contains

    !
    ! Subroutine fft to calculate the fast fourier transform
    !
    ! Inputs: 
    !   --- x1 : array, with values to transform
    !
    ! Return:
    !   --- x1 : array, with values tranformed
    
   
    ! in place Cooley-Tukey FFT
    recursive subroutine fft(x1)

      complex(kind=dp), dimension(:), intent(inout) :: x1
      complex(kind=dp) :: t1
      integer :: N1
      integer :: i1
      complex(kind=dp), dimension(:), allocatable :: even, odd
   
      N1 = size(x1)
   
      if(N1 .le. 1) return
   
      allocate(odd((N1 + 1) / 2))
      allocate(even(N1 / 2))
   
      ! divide
      odd = x1(1 : N1 : 2)
      even = x1(2 : N1 : 2)
   
      ! conquer
      call fft(odd)
      call fft(even)
   
      ! combine
      do i1 = 1, N1 / 2
         t1 = exp(cmplx(0.0_dp, -2.0_dp*pi1*real(i1 - 1, dp) / real(N1, dp), kind = dp))*even(i1)
         x1(i1) = odd(i1) + t1
         x1(i1 + N1 / 2) = odd(i1) - t1
      end do
   
      deallocate(odd)
      deallocate(even)
   
    end subroutine fft
   
  end module fast_fourier_transform