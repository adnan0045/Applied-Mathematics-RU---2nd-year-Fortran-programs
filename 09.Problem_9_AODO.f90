program order
implicit none

  real,allocatable,dimension(:)::a
  integer::si,n,i,j,t

  do
    print*,"Enter the serial number(Input 0 to exit):"
	read*,si
	if(si==0)exit

	print*,"Enter the number of value:"
	read*,n
	allocate(a(n))

	print*,"Enter the values:"
	read*,(a(i),i=1,n)

	do i=1,n-1
	  do j=i+1,n
	    
		if(a(i)>a(j))then
		  
		  t=a(i)
		  a(i)=a(j)
		  a(j)=t
		
		end if
	  
	  end do
	end do

	print*,"The ascending order is:"
	!print*,(a(i),i=1,n)	          !this print statement also can be used to print horizontally
	do i=1,n
	  print*,a(i)
	end do

	do i=1,n-1
	  do j=i+1,n

	    if(a(i)<a(j))then

		  t=a(i)
		  a(i)=a(j)
		  a(j)=t

		end if
	  
	  end do
	end do
	
	print*,"The descending order is"
	!print*,(a(i),i=1,n)			   !this print statement also can be used to print horizontally
	do i=1,n
	  print*,a(i)
	end do
	
  end do
  
end program order    	   
	  
