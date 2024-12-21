program series_cosx
implicit none

  real::sum,fact,x
  real,parameter::pi=3.1416
  integer::si,i,n

    do
    print*,"Enter the serial number:"
	read*,si
	if(si==0)exit

	print*,"Enter the value of x:"
	read*,x
	x=(pi*x)/180.0
	
	print*,"Enter the value of n:"
	read*,n
	n=n-1
	sum=1
	fact=1
	  
	  do i=1,n
	    fact=fact*(2*i)*(2*i-1)
		sum=sum+((-1)**i*x**(2*i))/fact
	  end do

	print*,"The sum of the series is",sum

  end do
end program series_cosx

