program mat_smdfml
implicit none
 
 integer::si,m,n,p,q,i,j,k
 integer,allocatable,dimension(:,:)::A,B,sm,df,ml

 do
   print*,"Enter the serial number:"
   read*,si
   if(si==0)exit

   print*,"Enter the value of m and n:"
   read*,m,n
   print*,"Enter the value of p and q:"
   read*,p,q

   allocate(A(m,n),B(p,q),sm(m,n),df(p,q),ml(m,q))

   print*,"Enter the first matrix row-wise:"
   read*,((A(i,j),j=1,n),i=1,m)

   print*,"Enter the second matrix row-wise:"
   read*,((B(i,j),j=1,q),i=1,p)

   print*,"A matrix is:"
     do	i=1,m
	   print*,(A(i,j),j=1,n)
	 end do

   print*,"B matrix is:"
     do i=1,p
	   print*,(B(i,j),j=1,q)
	 end do

	   if(m==p.and.n==q)then
	   print*,"Sum of the matrix is:"
	     do i=1,m
		   do j=1,n
			 sm(i,j)=A(i,j)+B(i,j)
		   end do
		 end do
	   do i=1,m
	     print*,(sm(i,j),j=1,n)
	   end do

	   print*,"Difference of the matrix is:"
	     do i=1,m
		   do j=1,n
		     df(i,j)=A(i,j)-B(i,j)
		   end do
		 end do

	   do i=1,m
	     print*,(df(i,j),j=1,n)
	   end do

	   else
	     print*,"Sum and difference are not possible"
	   end if

	   if(n==p)then
	   ml=0
	     print*,"Multiplication of two matrices is:"

		   do i=1,m
		     do j=1,q
			   do k=1,n
			     ml(i,j)=ml(i,j)+A(i,k)*B(k,j)
			   end do
			 end do
		   end do
		 do i=1,m
		   print*,(ml(i,j),j=1,q)
		 end do

	   else
		 print*,"Multiplication is not possible"
	   end if
	deallocate(A)
	deallocate(B)
	deallocate(sm)
	deallocate(df)
	deallocate(ml)

  end do

end program mat_smdfml 
		     

	   