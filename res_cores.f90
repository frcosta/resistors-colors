! -----------------------------------------------
! Calculo de resistores baseado nas cores
! GNU Fortran (tdm64-1) 10.3.0 on Windows 11
! Fabiano Costa Sep/21/2025
! compile: make all 
! -----------------------------------------------
! Tabela de cores
! -----------------------------------------------
! 0 - Preto          6 - Azul
! 1 - Marrom         7 - Violeta
! 2 - Vermelho       8 - Cinza
! 3 - Laranja        9 - Branco
! 4 - Amarelo        D - Dourado 5% tolerancia
! 5 - Verde          P - Prateado 10% tolerancia
! -----------------------------------------------

program resistor_color
    use iso_c_binding
    implicit none
    integer :: k, cont
    integer, dimension(4) :: c4
    integer, dimension(5) :: c5
    character(len=10), dimension(12) :: cor = &
           & ['Preto     ','Marrom    ', &
           &  'Vermelho  ','Laranja   ', &
           &  'Amarelo   ','Verde     ', &
           &  'Azul      ','Violeta   ', &
           &  'Cinza     ','Branco    ', &
           &  'Dourado   ','Prateado  ']

    interface
        function getch() bind(C, name="getch")
          import :: c_int
          integer(c_int) :: getch
        end function getch
    end interface

    write(*,*)
    write(*,'(A)') '========= SELECIONE O TIPO DE RESISTOR ==========='
    write(*,'(A)') '[4] Quatro faixas - Padrao 5% ou 10% de tolerancia'
    write(*,'(A)') '[5] Cinco faixas  - Precisao'
    k = getch()

    select case (k)
    case (52)
        call showTable
        write(*,*)
        write(*,'(A)') 'Resistor padrao com 4 faixas'
        call dataEntry( c4, 4 )
        call proc4Colors ( c4, 4 )

    case (53)
        call showTable
        write(*,*)
        write(*,'(A)') 'Resistor de precisao com 5 faixas'
        call dataEntry( c5, 5)
        call proc5Colors ( c5, 5 )
    end select
    
    contains

    subroutine proc5Colors( vector, size )
        integer, intent(in)    :: size
        integer, intent(inout) :: vector(size)   
        integer                :: soma
        character(len=10)      :: texto
        character(len=5), dimension(8) :: tolerancia = ['1%   ','2%   ','3%   ','4%   ', &
                                                     &  '0,5% ','0,25%','0,1% ','0,05%']
        
        write(*,'(/ A)', advance="no") 'Resistor em ohms: ' 
                                       
        select case (vector(4))
        case (0)
            write(*,'(I1, I1, I1)') vector(1), vector(2), vector(3)

        case (11)
            write(*,'(I1, I1, A, I1)') vector(1), vector(2), ',', vector(3)

        case (12)
            write(*,'(I1, A, I1, I1)') vector(1), ',', vector(2), vector(3)

        case default
            soma = (vector(1) * 100 + vector(2) * 10 + vector(3)) * 10 ** vector(4)
            write(texto, '(I10)') soma
            write(*,'(A)') adjustl(texto)

        end select
        
        write(*,'(A, A)') 'Tolerancia:       ', tolerancia(vector(5))

    end subroutine

    subroutine proc4Colors( vector, size )
        integer, intent(in)    :: size
        integer, intent(inout) :: vector(size)   
        integer                :: soma
        character(len=10)      :: texto
        
        write(*,'(/ A)', advance="no") 'Resistor em ohms: ' 
                                       
        select case (vector(3))
        case (0)
            write(*,'(I1, I1)') vector(1), vector(2)

        case (11)
            write(*,'(A, I1, I1)') '0,', vector(1), vector(2)

        case (12)
            write(*,'(I1, A, I1)') vector(1), ',', vector(2)

        case default
            soma = 10 ** vector(3) * (vector(1) * 10 + vector(2))
            write(texto, '(I10)') soma
            write(*,'(A)') adjustl(texto)

        end select
        
        if (vector(5) == 11) then
            write(*,'(A/)') 'Tolerancia:       5%'
        else if (vector(4) == 12) then 
            write(*,'(A/)') 'Tolerancia:       10%'
        end if
    end subroutine

    subroutine dataEntry( vector, size )
        integer, intent(in)    :: size
        integer, intent(inout) :: vector(size)
        do cont = 1, size
            write(*, '(A, I1, A)', advance="no") 'Faixa', cont, ':'
            k = getch()

            if (k == 68 .or. k == 100) then         ! k='d' or k='D'
                write (*,*) cor(11)
                vector(cont) = 11
            else if ( k == 80 .or. k == 112) then   ! k='p' or k='P'
                write (*,*) cor(12)
                vector(cont) = 12
            else
                write(*,*) cor(k-48+1)
                vector(cont) = k-48
            end if
        end do
    end subroutine


    subroutine showTable
        integer :: i
        write(*,*)
        do i = 1, 5
            write (*, '(I2, A, A, I2, A, A)') i-1,' - ', cor(i), i+5-1,' - ', cor(i+5)
        end do
        write (*,*)
        write (*,'(A, A, A, A)') ' D - ', cor(11), ' P - ', cor(12)
    end subroutine showTable

end program resistor_color