program outer_circle
implicit none

real::x1,x2,x3,y1,y2,y3,a,b,c,area,perimeter,s,radius,circumference,circle_area
real,parameter::pi=3.1416
integer::si

  do
  print*,"Enter the serial number(input 0 to exit):"
  read*,si
  if(si==0)exit

    Print*,"Enter the first vertex:"
	read*,x1,y1

	Print*,"Enter the second vertex:"
	read*,x2,y2

    Print*,"Enter the third vertex:"
	read*,x3,y3

	a = sqrt((x1-x2)**2+(y1-y2)**2)		  !first side of the triangle
	b = sqrt((x2-x3)**2+(y2-y3)**2)		  !second side of the triangle
	c = sqrt((x3-x1)**2+(y3-y1)**2)		  !third side of the triangle

	if((a+b>c).and.(b+c>a).and.(c+a>b))then
	  
	  perimeter = a+b+c					  !perimeter of the triangle
	  s = perimeter/2.0
	  area = sqrt(s*(s-a)*(s-b)*(s-c))	  !area of the triangle

	  radius = (a*b*c)/(4.0*area)			  !radius of the outer circle
	  circumference = 2.0*pi*radius		  !circumference of the outer circle
	  circle_area = pi*(radius**2)		  !area of the outer circle 

	  print 40,circumference,circle_area
	    40 format(2X,"The circumference of the outer circle is:",f8.3/2X,"The area of the outer circle is:",f8.3)

	else
	    print*,"Doesn't form a triangle"

	end if

  end do

end program outer_circle