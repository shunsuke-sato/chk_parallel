program main
  implicit none
  include 'mpif.h'
  integer :: Myrank,Nprocs,ierr
  real(8) :: Time_start,Time_now
  integer :: nsize, nloop, ns, ne, nsize_per_process
  integer :: iloop,i
  real(8),allocatable :: avec(:), bvec(:), cvec(:), avec_l(:)
  real(8) :: ss

  call MPI_init(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,Nprocs,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,Myrank,ierr)
  Time_start=MPI_WTIME()

  nloop = 10000
  nsize = 256
  allocate(avec(nsize),bvec(nsize),cvec(nsize),avec_l(nsize))

  avec = 0d0
  bvec = 1d0
  cvec = 2d0

!  if(myrank == 0)then
!    write(*,*)'nprocs=',nprocs
!  end if
!  write(*,*)'hello',myrank

  if(mod(nsize,nprocs)/=0)stop 'error'
  nsize_per_process = nsize/nprocs
  ns = nsize_per_process*myrank+1
  ne = nsize_per_process*(myrank + 1)

  write(*,*)ns,ne

  do iloop =1, nloop
    do i = ns, ne
      avec(i) = bvec(i) + cvec(i)
    end do
  end do
  
  avec_l = 0d0
  call MPI_Allreduce(avec, avec_l, nsize, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)

  if(myrank == 0)then
    ss = 0d0
    do i = 1, nsize
      ss = ss + abs(avec_l(i)-3d0)
    end do
    write(*,*)'chk',ss
  end if

  call MPI_FINALIZE(ierr)
  
end program main
