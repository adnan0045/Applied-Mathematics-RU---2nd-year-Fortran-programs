program prime
implicit none

  integer::m,n,i,j,p,si,flag
  
  do
    print*,"Enter the serial number:"
	read*,si
	if(si==0)exit

	p=0
	print*,"Enter the range m and n (m<n):"
	read*,m,n

	print*,"The required prime numbers are:"

	do i=m,n
	if(i<=1)cycle
	flag=1

	  do j=2,i-1
	    if(mod(i,j)==0)flag=0
	  end do

	  if(flag==1)then
	    print*,j
		p=p+1
	  end if

	end do

	print*,"The number of prime number is:",p

  end do

end program prime
