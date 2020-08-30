program gaussjordan

!Universidade de Brasilia - Metodos Computacionais A
!Professor: Luiz A. Ribeiro Junior - Instituto de Física
!Aluno: Alexandry Moreira Alves Pinto - 17/0078761
!Problema 1 - Solução de sistemas lineares usando metodo de Gauss-Jordan.
!Github url: 


!O algortimo é: pegamos um elemento da diagonal, dividimos toda sua linha pelo elemento da diagonal
!em seguida fazemos a operação que zera todo elemento da sua coluna(exceto o de indice repetido) acima e abaixo
!essa operação é feita com todas as linhas, em seguida substituimos os elementos antigos da matrix
!pelos da reduced que foram operados
!no final temos uma matrix identidade onde os indices (n+1,1) é os resultados de x.



integer*8 :: n,h,g,p,q
double precision :: e, r, f
double precision, dimension(100,100) :: matrix
double precision, dimension(100,100) :: reduced


!O usuario insere a ordem da matriz
write(*,*) "Bem vindo ao programa de solução de sistemas lineares usando metodo de Gauss-Jordan."
write(*,*) 
write(*,*) "Digite a ordem da sua matriz (numero inteiro e sem contar a coluna das variaveis):"
read (*,*) n

write(*,*) "Quando for escrever fração, transforme em numero decimal."
write(*,*)

!O usuario insere as entradas da matriz
do i = 1, n
	do j = 1, n+1
		if (j < (n+1)) then
		write(*,*) "Digite o",i, "elemento da",j, "coluna"
		read(*,*) matrix(i,j)
		else 
		write(*,*) "Elemento",i, " da coluna de resultados."
		read(*,*) matrix(i,j)
		end if
	end do
end do


!Clonamos a matriz
do h = 1, n 
do i = 1, n 

	do g = 1, n + 1
	do j = 1, n + 1
		if (h == i .and. g == j ) then
            reduced(h,g) = matrix(i,j) 
		end if
	end do
	end do
	
end do
end do


!p indica a qual linha e coluna devemos comecar nosso calculo
do p = 1,n 
!os novos elementos da reduced será substituido pelas operacoes que envolvem a matrix normal
!e que zeram os termos abaixo e acima da diagonal
do h = 1, n 
do i = 1, n 

	do g = 1, n + 1
	do j = 1, n + 1
		if (h==i .and. g==j .and. h==p ) then
			reduced(h,g) = matrix(i,j) / matrix(p,p)
		else if (h==i .and. g==j) then
			reduced(h,g) = matrix(i,j) - matrix(i,p) * matrix(p,j) / matrix(p,p)
		end if
	end do
	end do

end do
end do

!Aqui igualamos os elementos da matrix com os novos da reduced para no proximo "p" usarmos os novos valores calculados
!e nao usarmos os antigos da matrix
do h = 1, n 
do i = 1, n 

	do g = 1, n + 1
	do j = 1, n + 1
		if (h == i .and. g == j ) then
             matrix(i,j) = reduced(h,g)
		end if
	end do
	end do

end do
end do

end do



write(*,*)
write(*,*)



do h = 1, n
do i = 1, n

	do g = 1, n+1
	do j = 1, n+1
		if (h == i .and. g == j .and. g == n + 1) then !os resultados serao os termos (n+1) da matriz inicial matrix(i,j)
             write(*,*) "Variavel",i,"=",matrix(i,j)
		end if
	end do
	end do

end do
end do



!Se quiser imprimir a matriz para verificar que está igual a identidade
!do i=1,n
!do j=1,n+1
!write(*,*) matrix(i,j)
!end do
!end do

end program Gaussjordan