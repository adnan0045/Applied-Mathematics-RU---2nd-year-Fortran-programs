program area_peri_angle
implicit none

real::a,b,c,theta,radian_theta,area,perimeter,s
real,parameter:: pi = 3.1416
integer::si
  
  do
 	print*,"Enter the serial number(Enter 0 to exit):"
	read*,si
	if(si==0)exit

	print*, "Enter the first side of the triangle:"
	read*,a

	print*, "Enter the second side of the triangle:"
	read*,b

	print*, "Enter the angle between the sides of the triangle:"
	read*,theta
	print*,"Angle:",theta

	radian_theta = theta*pi/180.0

	c = sqrt(a**2+b**2-2.0*a*b*cos(radian_theta))	!Third side of the triangle

	  if ((a+b>c)	.and. (b+c>a) .and. (c+a>b)) then

	  perimeter = a+b+c
	  s = perimeter/2.0
	  area = sqrt(s*(s-a)*(s-b)*(s-c))

	  print 10,area,perimeter
	  10 format (2X,"Area of the triangle is:",f8.3/2X,"Perimeter of the triangle is:",f8.3)
	  
	  else
		  print*,"Doesn't form a triangle"
	  
	  end if
  
  end do

end program area_peri_angle