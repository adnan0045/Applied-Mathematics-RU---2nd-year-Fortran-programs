program prob_21
implicit none
  
  integer::si,i,n
  real::sum,x

  do 
  print*,"Enter the serial number:"
  read*,si
  if(si==0)exit

  print*,"Enter the value of x and n:"
  read*,x,n

    if(abs(x)>0.5)then
	  print*,"This exceeds the condition,it must be 0.5 or less"
	else
	  sum=0.0
	  do i=1,n
	    sum=sum+(2*x)**(i-1)
	  end do
	  print*,"The sum is",sum
	end if
  end do
end program prob_21  