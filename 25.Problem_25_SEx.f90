program seriesEx
implicit none
  
  real::sum,fact,x
  integer::si,i,n

  do
    print*,"Enter the serial number:"
	read*,si
	if(si==0)exit

	print*,"Enter the value of x:"
	read*,x
	
	print*,"Enter the value of n:"
	read*,n
	if(n==0)then
	  print*,"Sum=0"
	else

	sum=1
	fact=1
	  
	  do i=1,n-1
	    fact=fact*i
		sum=sum+(x**i)/fact
	  end do

	print*,"The sum of the series is",sum
	end if
  end do
end program seriesEx
