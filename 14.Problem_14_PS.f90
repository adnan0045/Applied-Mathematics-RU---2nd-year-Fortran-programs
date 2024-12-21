program product_series
implicit none
  
  integer::si,n,i
  real::pro

  do
    print*,"Enter the serial number:"
	read*,si
	if(si==0)exit

	print*,"Enter the value of n:"
	read*,n
	pro=1

	  do i=1,n
	    pro=pro*((2.0*i-1)/(2.0*i))
	  end do

	print*,"Product=",pro

  end do

end program product_series
