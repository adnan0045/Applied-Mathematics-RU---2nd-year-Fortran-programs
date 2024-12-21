program fibonacci
implicit none
  
  integer::i,n,si
  integer,allocatable,dimension(:)::fibo

  do
    print*,"Enter the serial number:"
	read*,si
	if(si==0)exit

	print*,"Enter the value of n:"
	read*,n
	allocate(fibo(n))

	fibo(1)=0
	fibo(2)=1

	print*,"The fibonacci series is:"

	  do i=3,n
	    fibo(i)=fibo(i-2)+fibo(i-1)
	  end do
	
	  do i=1,n
	    print*,fibo(i)
	  end do
	
	deallocate(fibo)
	
  end do
  
end program fibonacci    	    
