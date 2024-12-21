program stline
implicit none
 
 integer::n,i,si
 real,allocatable,dimension(:)::x,y
 real::sx,sy,sxy,ssq,t,m,c

 do 
   print*,"Enter the serial number:"
   read*,si
   if(si==0)exit

   print*,"Enter the value of n:"
   read*,n
   allocate(x(n),y(n))

   print*,"Enter n pairs of x and y value:"
   read*,(x(i),y(i),i=1,n)

   sx=0.0
   sy=0.0
   sxy=0.0
   ssq=0.0

   do i=1,n

     sx	= sx+x(i)
	 sy = sy+y(i)
	 sxy = sxy+x(i)*y(i)
	 ssq = ssq+x(i)*x(i)

   end do

   t=n

   m = ((t*sxy-sx*sy)/(t*ssq-(sx**2)))
   c = (sy-m*sx)/t

   print 10,m,c
    10 format(2X,"Y=",f10.3,"x+",f10.3)

   deallocate(x)
   deallocate(y)

 end do

end program stline