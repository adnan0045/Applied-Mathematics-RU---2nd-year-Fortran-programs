program serieslog
implicit none
  
  real::sum,x
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

	sum=0
	  
	  do i=1,n
		sum=sum+((-1)**(i+1)*x**i)/i
	  end do

	print*,"The sum of the series is",sum
	end if
  end do
end program serieslog