program heat_expliciteuler
      implicit none
     
      real(kind=8) :: dx, dt, alpha,a,b
      real(kind=8), allocatable :: u(:), uold(:)
      integer :: i,j,n, t
      real :: s
      
      
      
      write(*,*) ' entre com n, dt,t, alpha'
      read (*,*) n, dt,t, alpha
      
   
      a = 0 
      b = 1
      dx = (b-a)/n
      s = (dt*alpha)/(dx*dx)
      if (s > 0.5) then
      write(*,*)'warning: s  = ',s,'> 0.5'
      endif
      
        
      write(*,*) dt
      
      
      allocate (u(0:n))
      allocate (uold(0:n))
      
      do i=0,n-1
      uold(i) = 0
      enddo
      uold(n) = 100
     
      
      do j=1,t
      u(0) = 0
      u(n) = 100
      do i=1,n-1
      u(i) = uold(i)+s*(uold(i-1) +2*uold(i)+uold(i+1))
      enddo
      write(*,*) u
      write(*,*) ' '
      uold = u
      enddo
    
      end program heat_expliciteuler
