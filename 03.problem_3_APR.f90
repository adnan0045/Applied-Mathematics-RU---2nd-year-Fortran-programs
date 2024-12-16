program rectangle
implicit none

real::x1,x2,x3,x4,y1,y2,y3,y4,a,b,c,d,e,f,area,perimeter
integer:: si
  
  do
    print*,"Enter the serial number(input 0 to exit):"
	read*,si
	if(si==0)exit

	  print*,"Enter the first vertex"
	  read*,x1,y1

	  print*,"Enter the second vertex"
	  read*,x2,y2
	
	  print*,"Enter the third vertex"
	  read*,x3,y3	
	
	  print*,"Enter the fourth vertex"
	  read*,x4,y4			

	  a = sqrt((x1-x2)**2+(y1-y2)**2)  !first side
	  b = sqrt((x2-x3)**2+(y2-y3)**2)  !second side
	  c = sqrt((x3-x4)**2+(y3-y4)**2)  !third side
	  d = sqrt((x4-x1)**2+(y4-y1)**2)  !fourth side

	  e = sqrt((x1-x3)**2+(y1-y3)**2)  !first diagonal
	  f = sqrt((x2-x4)**2+(y2-y4)**2)  !second diagonal

	  if((a==c).and.(b==d).and.(e==f))then
	    
		area = a*b
		perimeter = 2*(a+b)

		print 20,area,perimeter
		20 format(2X,"The area of the rectangle is:",f8.3/2X,"The perimrter of the rectangle is:",f8.3)

	  else
	    
		print*,"Doesn't form a rectangle"

	  end if

   end do
end program rectangle
