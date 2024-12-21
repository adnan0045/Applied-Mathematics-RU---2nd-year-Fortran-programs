program sum_reverse
implicit none

  integer::si,sum,rev,n,x

  do
    print*,"Enter the serial number:"
	read*,si
	if(si==0)exit

	print*,"Enter the number:"
	read*,n
	
	sum=0
	rev=0

	do
	  if(n==0)exit
	  x=mod(n,10)
	  rev=rev*10+x
	  n=n/10
	  sum=sum+x
	end do

	print*,"Sum of the digits of the number is:",sum
	print*,"Reverse of the number is:",rev

  end do

end program sum_reverse
