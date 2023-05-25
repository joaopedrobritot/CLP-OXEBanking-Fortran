module cartoes
    implicit none
    integer, parameter :: max_cartoes = 100
    integer :: num_cartoes = 0
    
    type :: CartaoFisico
        integer :: numero
        real :: limite_credito
        logical :: bloqueado
        logical :: eh_debito
        logical :: eh_credito
    end type CartaoFisico

    type :: CartaoVirtual
        integer :: numero
        real :: limite_credito
        logical :: bloqueado
        integer :: cartao_fisico_associado
    end type CartaoVirtual

    type(CartaoFisico), dimension(max_cartoes) :: cartoes_fisicos
    type(CartaoVirtual), dimension(max_cartoes) :: cartoes_virtuais
    
    contains

    ! Implementação das funcionalidades

    subroutine criar_cartao_fisico(cartao, num, limite, eh_debito, eh_credito)
        type(CartaoFisico), intent(out) :: cartao
        integer, intent(in) :: num
        real, intent(in) :: limite
        logical, intent(in) :: eh_debito
        logical, intent(in) :: eh_credito
        cartao%numero = num
        cartao%limite_credito = limite
        cartao%bloqueado = .false.
        cartao%eh_debito = eh_debito
        cartao%eh_credito = eh_credito
        num_cartoes = num_cartoes + 1
        cartoes_fisicos(num_cartoes) = cartao
        print *, "Cartão físico criado com sucesso."
    end subroutine criar_cartao_fisico

    subroutine criar_cartao_virtual(cartao_virtual, cartao_fisico, num)
        type(CartaoVirtual), intent(out) :: cartao_virtual
        integer, intent(in) :: cartao_fisico, num
        cartao_virtual%numero = num
        cartao_virtual%limite_credito = cartoes_fisicos(cartao_fisico)%limite_credito
        cartao_virtual%bloqueado = cartoes_fisicos(cartao_fisico)%bloqueado
        cartao_virtual%cartao_fisico_associado = cartao_fisico
        num_cartoes = num_cartoes + 1
        cartoes_virtuais(num_cartoes) = cartao_virtual
        print *, "Cartão virtual criado com sucesso."
    end subroutine criar_cartao_virtual

    subroutine remover_cartao_virtual(num)
        integer, intent(in) :: num
        integer :: i, j
        do i = 1, num_cartoes
            if (cartoes_virtuais(i)%numero == num) then
                do j = i, num_cartoes-1
                    cartoes_virtuais(j) = cartoes_virtuais(j+1)
                end do
                num_cartoes = num_cartoes - 1
                print *, "Cartão virtual removido com sucesso."
                return
            end if
        end do
        print *, "Cartão virtual não encontrado."
    end subroutine remover_cartao_virtual

    subroutine bloquear_cartao_fisico(num)
        integer, intent(in) :: num
        integer :: i
        do i = 1, num_cartoes
            if (cartoes_fisicos(i)%numero == num) then
                cartoes_fisicos(i)%bloqueado = .true.
                print *, "Cartão físico bloqueado com sucesso."
                return
            end if
        end do
        print *, "Cartão físico não encontrado."
    end subroutine bloquear_cartao_fisico

    subroutine exibir_fatura(num)
        integer, intent(in) :: num
        integer :: i
        do i = 1, num_cartoes
            if (cartoes_virtuais(i)%numero == num) then
                print *, "Fatura do cartão virtual", cartoes_virtuais(i)%numero
                print *, "Limite de crédito:", cartoes_virtuais(i)%limite_credito
                print *, "Bloqueado:", cartoes_virtuais(i)%bloqueado
                return
            end if
        end do
        print *, "Cartão virtual não encontrado."
    end subroutine exibir_fatura

    ! Exemplo de uso das funcionalidades

    

end module cartoes

program main
   use cartoes
   implicit none
   
   call criar_cartao_fisico(cartoes_fisicos(1), 1234, 1000.0, .true., .true.)
    call criar_cartao_virtual(cartoes_virtuais(1), 1, 5678)
    call exibir_fatura(5678)
    call bloquear_cartao_fisico(1234)
    call remover_cartao_virtual(5678)
    
    print *, "Cartão Físico:"
    print *, "Número:", cartoes_fisicos(1)%numero
    print *, "Limite de crédito:", cartoes_fisicos(1)%limite_credito
    print *, "Bloqueado:", cartoes_fisicos(1)%bloqueado
    if (cartoes_fisicos(1)%eh_debito) then
        print *, "Tipo: débito"
    end if
    if (cartoes_fisicos(1)%eh_credito) then
        print *, "Tipo: crédito"
    end if
  
 end program main
