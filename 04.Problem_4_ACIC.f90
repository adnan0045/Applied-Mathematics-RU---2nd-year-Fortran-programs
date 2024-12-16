program inner_circle
implicit none

real::x1,x2,x3,y1,y2,y3,a,b,c,radius,area,perimeter,s,circumference,circle_area
real,parameter::pi=3.1416
integer::si
 
  do
    print*,"Enter the serial number(input 0 to exit):"
	read*,si
	if(si==0)exit

	  print*,"Enter the first vertex:"
	  read*,x1,y1

	  print*,"Enter the second vertex:"
	  read*,x2,y2

	  print*,"Enter the third vertex:"
	  read*,x3,y3

	  a = sqrt((x1-x2)**2+(y1-y2)**2)
	  b = sqrt((x2-x3)**2+(y2-y3)**2)
	  c = sqrt((x3-x1)**2+(y3-y1)**2)

	  if((a+b>c).and.(b+c>a).and.(c+a>b))then
	    
		perimeter = a+b+c					 !perimeter of triangle
		s = perimeter/2.0					 
		area = sqrt(s*(s-a)*(s-b)*(s-c)) 	 !area of triangle

		radius = 2.0*area/perimeter			 !radius of inner circle
		circumference = 2.0*pi*radius		 !circumference of inner circle
		circle_area = pi*(radius**2)         !area of inner circle

		print 30,circle_area,circumference
		 30 format(2X,"The area of inner circle is:",f8.3/2X,"The circumference of inner circle is:",f8.3)

	  else
	     print*,"Doesn't form a triangle"
	  
	  end if
  
  end do
  
end program inner_circle      	   
