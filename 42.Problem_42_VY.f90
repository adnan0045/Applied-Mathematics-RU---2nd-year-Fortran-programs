program valuey
implicit none
  
  integer::si
  real::y,x

  do 
    print*,"Enter the serial number:"
	read*,si
	if(si==0)exit

	print*,"Enter the value of x:"
	read*,x
	print*,"The value of y is",y(x)

  end do
end program valuey

real function y(x)
implicit none
real::x

  if(x<2.0)then
    y=2*x**2+3*x+4
  else if(x==2.0)then
    y=0.0
  else
    y=2*x**2+3*x-4
  end if
end function

