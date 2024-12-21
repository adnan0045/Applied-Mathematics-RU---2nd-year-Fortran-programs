program quadratic_normal
implicit none

  real::a,b,c,d,e,x1,x2,im1,im2,r
  integer::si

  do
    print*,"Enter the serial number(input 0 to exit):"
	read*,si
	if(si==0)exit

	print*,"Enter the value of a,b,c,d:"
	read*,a,b,c,d

	if((a+b)==0)then
	  print*,"This is not a quadratic equation"

	else
	  e = c**2-4*(a+b)*d		   !discriminant

	  if(e>0)then
	    x1 = (-c+sqrt(e))/2.0*(a+b)
		x2 = (-c-sqrt(e))/2.0*(a+b)
		print*,"Two roots are real and unequal"
		print*,"x1=",x1," x2=",x2

	  else if(e==0)then
	    x1 = -c/(2.0*(a+b))
		x2 = x1
		print*,"Two roots are real and equal"
		print*,"x1=",x1," x2=",x2

	  else
	    r = -c/(2.0*(a+b))
		im1 = sqrt(abs(e))/(2.0*(a+b))
		im2 = im1
		print*,"Two roots are complex"
		print*,"x1=",r,"+i",im1," x2=",r,"-i",im2

	  end if

	end if

  end do

end program quadratic_normal
	