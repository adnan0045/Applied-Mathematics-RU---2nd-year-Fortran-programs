program quadsub
implicit none

  real::a,b,c,d,e,x,x1,x2,r,im
  integer::si

  do 
    print*,"Enter the serial number:"
	read*,si
	if(si==0)exit

	print*,"Enter the value of a,b,c,d:"
	read*,a,b,c,d

	e=c**2-4*(a+b)*d

	if((a+b)==0)then
	  x=-d/c
	  print*,"This is not a quadratic equation and x=",x

	else
	    call f(a,b,c,e,x1,x2,r,im)

		if(e>0)then
		  print*,"Two roots are real and unequal"
		  print*,"x1=",x1,"x2=",x2
		else if(e==0)then
		  print*,"Two roots are real and equal"
		  print*,"x1=",x1,"x2=",x2
		else
		  print*,"Two roots are complex"
		  print*,"x1=",r," + i",im
		  print*,"x2=",r," - i",im
		end if

	 end if
  
  end do

end program

subroutine f(a,b,c,e,x1,x2,r,im)
implicit none
real,intent(in)::a,b,c,e
real,intent(out)::x1,x2,r,im

if(e>0)then
  x1=-c/(2.0*(a+b))+sqrt(e)/(2.0*(a+b))
  x2=-c/(2.0*(a+b))-sqrt(e)/(2.0*(a+b))
else if(e==0)then
  x1=-c/(2.0*(a+b))
  x2=x1
else
  r=-c/(2.0*(a+b))
  im=sqrt(abs(e))/(2.0*(a+b))
end if

end subroutine