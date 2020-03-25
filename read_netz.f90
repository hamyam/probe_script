      parameter(mc=100,mb=5)
  
!**********************************************************             
! mb  nr. of blocks                                                     
! mc  nr. of cells (including boundary-cells of each block)             
! ms  nr. of boundary-cells  (including edges and vertices)             
! mn  nr. of neighbours of all boundary-cells on interfaces             
! m1  maximum nr. of cells in one direction +1                          
!**********************************************************             
                                                                        
      double precision x,y,z        
      integer nx,ny,nz
      dimension x(mc,mc,mc),y(mc,mc,mc),z(mc,mc,mc)
      dimension nx(mb),ny(mb),nz(mb)


!******* Anfang Lesen Koordinaten der Netzknoten aller Bl"ocke *********
      open(77,file='netz.dat',form='unformatted',status='unknown') 
      read(77)mblocks 
                                                                        
      do 200 m=1,mblocks 
                                                    !for every new block
        read(77)nx(m),ny(m),nz(m)
        write(*,*)"zellen in richtungen:",nx(m),ny(m),nz(m)
        read(77)(((x(k,j,i),y(k,j,i),               &
     &            z(k,j,i),k=1,nz(m)+1),j=1,ny(m)+1),i=1,nx(m)+1)      
        write(*,996)(((x(k,j,i),y(k,j,i),               &
     &            z(k,j,i),k=1,nz(m)+1),j=1,ny(m)+1),i=1,nx(m)+1) 
                                                                        
  200 continue 
      close(77) 
      
!******** Ende Lesen Koordinaten der Netzknoten aller Bl"ocke **********

      open(55,file='netz4.dat',form='formatted',status='unknown') 
      
      do 100 m=1,mblocks
        do 101 i=1,nx(m)+1
          do 102 j=1,ny(m)+1
            do 103 k=1,nz(m)+1
              write(55,996)x(k,j,i),y(k,j,i),z(k,j,i)
              
  103       continue
  102     continue
  101   continue
  100 continue
      
  996     format(3(1pe13.6,2x))
      close(55) 

END
