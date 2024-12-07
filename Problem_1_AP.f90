program area_primeter

implicit none

integer:: si
real:: area,perimeter,x1,x2,x3,y1,y2,y3,a,b,c,s

  do
    print*, "Enter the serial number (input 0 to exit):"
	read*, si
    if(si==0)exit
	  
	  print*, "Enter the 1st vertex (x1,y1):"
	  read*, x1,y1

	  print*, "Enter the 2nd vertex (x2,y2):"
	  read*, x2,y2

	  print*, "Enter the 1st vertex (x3,y3):"
	  read*, x3,y3

	  a = sqrt((x1-x2)**2 + (y1-y2)**2)   !First edge of the triangle

	  b = sqrt((x2-x3)**2 + (y2-y3)**2)   !Second edge of the triangle

	  c = sqrt((x3-x1)**2 + (y3-y1)**2)   !Third edge of the triangle

	    if ((a+b>c) .and. (b+c>a) .and. (c+a>b)) then

	      perimeter = a + b + c

	      print*, "Perimeter of the triangle is:",perimeter

	      s = perimeter/2
		
	      area = sqrt(s*(s-a)*(s-b)*(s-c))
		
	      print*, "Area of the triangle is:",area

		else
		  
		  print*, "Doesn't form a triangle"

		end if

  end do

end program area_primeter    