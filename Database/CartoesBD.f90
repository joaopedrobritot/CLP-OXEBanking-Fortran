! Requer a biblioteca externa "SQLite3" disponível em: https://github.com/interkosmos/fortran-sqlite3
! Certifique-se de compilar o código com a opção de vinculação apropriada para a biblioteca SQLite.

program cartoes_crud
    use, intrinsic :: iso_c_binding
    implicit none

    ! Definição dos tipos de dados
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

    ! Arrays para armazenar os cartões
    type(CartaoFisico), dimension(:), allocatable :: cartoes_fisicos
    type(CartaoVirtual), dimension(:), allocatable :: cartoes_virtuais

    ! Banco de dados SQLite
    type(c_ptr) :: db

    ! Função para inicializar o banco de dados
    subroutine init_database(db)
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(out) :: db

        character(c_char), dimension(256) :: dbname
        integer(c_int) :: rc

        ! Nome do banco de dados
        dbname = "cartoes.db"

        ! Inicializar o banco de dados
        rc = sqlite3_open(dbname, db)
        if (rc /= 0) then
            print *, "Erro ao abrir o banco de dados"
            stop
        end if
    end subroutine init_database

    ! Função para criar a tabela de cartões físicos no banco de dados
    subroutine create_cartoes_fisicos_table(db)
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(in) :: db

        character(c_char), dimension(256) :: sql
        integer(c_int) :: rc

        ! Consulta SQL para criar a tabela de cartões físicos
        sql = "CREATE TABLE IF NOT EXISTS cartoes_fisicos (numero INTEGER PRIMARY KEY, limite_credito REAL, bloqueado INTEGER, eh_debito INTEGER, eh_credito INTEGER);"

        ! Executar a consulta SQL
        rc = sqlite3_exec(db, sql, c_null_ptr, c_null_ptr, c_null_ptr)
        if (rc /= 0) then
            print *, "Erro ao criar a tabela de cartões físicos"
            stop
        end if
    end subroutine create_cartoes_fisicos_table

    ! Função para criar a tabela de cartões virtuais no banco de dados
    subroutine create_cartoes_virtuais_table(db)
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(in) :: db

        character(c_char), dimension(256) :: sql
        integer(c_int) :: rc

        ! Consulta SQL para criar a tabela de cartões virtuais
        sql = "CREATE TABLE IF NOT EXISTS cartoes_virtuais (numero INTEGER PRIMARY KEY, limite_credito REAL, bloqueado INTEGER, cartao_fisico_associado INTEGER);"

        ! Executar a consulta SQL
        rc = sqlite3_exec(db, sql, c_null_ptr, c_null_ptr, c_null_ptr)
        if (rc /= 0) then
            print *, "Erro ao criar a tabela de cartões virtuais"
            stop
        end if
    end subroutine create_cartoes_virtuais_table

    ! Função para inserir um cartão físico no banco de dados
    subroutine insert_cartao_fisico(db, cartao)
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(in) :: db
        type(CartaoFisico), intent(in) :: cartao

        character(c_char), dimension(256) :: sql
        integer(c_int) :: rc

        ! Consulta SQL para inserir um cartão físico
        sql = "INSERT INTO cartoes_fisicos (numero, limite_credito, bloqueado, eh_debito, eh_credito) VALUES (?, ?, ?, ?, ?);"

        ! Preparar a consulta SQL
        rc = sqlite3_prepare_v2(db, sql, len_trim(sql), c_null_ptr, c_null_ptr)
        if (rc /= 0) then
            print *, "Erro ao preparar a consulta SQL"
            stop
        end if

        ! Bind dos parâmetros na consulta SQL
        rc = sqlite3_bind_int(db, 1, cartao%numero)
        rc = sqlite3_bind_real(db, 2, cartao%limite_credito)
        rc = sqlite3_bind_int(db, 3, cartao%bloqueado)
        rc = sqlite3_bind_int(db, 4, cartao%eh_debito)
        rc = sqlite3_bind_int(db, 5, cartao%eh_credito)

        ! Executar a consulta SQL
        rc = sqlite3_step(db)
        if (rc /= 101) then ! SQLITE_DONE
            print *, "Erro ao inserir o cartão físico"
            stop
        end if

        ! Finalizar a consulta SQL
        rc = sqlite3_finalize(db)
    end subroutine insert_cartao_fisico

    ! Função para inserir um cartão virtual no banco de dados
    subroutine insert_cartao_virtual(db, cartao)
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(in) :: db
        type(CartaoVirtual), intent(in) :: cartao

        character(c_char), dimension(256) :: sql
        integer(c_int) :: rc

        ! Consulta SQL para inserir um cartão virtual
        sql = "INSERT INTO cartoes_virtuais (numero, limite_credito, bloqueado, cartao_fisico_associado) VALUES (?, ?, ?, ?);"

        ! Preparar a consulta SQL
        rc = sqlite3_prepare_v2(db, sql, len_trim(sql), c_null_ptr, c_null_ptr)
        if (rc /= 0) then
            print *, "Erro ao preparar a consulta SQL"
            stop
        end if

        ! Bind dos parâmetros na consulta SQL
        rc = sqlite3_bind_int(db, 1, cartao%numero)
        rc = sqlite3_bind_real(db, 2, cartao%limite_credito)
        rc = sqlite3_bind_int(db, 3, cartao%bloqueado)
        rc = sqlite3_bind_int(db, 4, cartao%cartao_fisico_associado)

        ! Executar a consulta SQL
        rc = sqlite3_step(db)
        if (rc /= 101) then ! SQLITE_DONE
            print *, "Erro ao inserir o cartão virtual"
            stop
        end if

        ! Finalizar a consulta SQL
        rc = sqlite3_finalize(db)
    end subroutine insert_cartao_virtual

    ! Função para recuperar os cartões físicos do banco de dados
    subroutine retrieve_cartoes_fisicos(db, cartoes)
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(in) :: db
        type(CartaoFisico), dimension(:), allocatable :: cartoes

        character(c_char), dimension(256) :: sql
        integer(c_int) :: rc, rowcount, i
        type(CartaoFisico) :: cartao

        ! Consulta SQL para recuperar os cartões físicos
        sql = "SELECT numero, limite_credito, bloqueado, eh_debito, eh_credito FROM cartoes_fisicos;"

        ! Preparar a consulta SQL
        rc = sqlite3_prepare_v2(db, sql, len_trim(sql), c_null_ptr, c_null_ptr)
        if (rc /= 0) then
            print *, "Erro ao preparar a consulta SQL"
            stop
        end if

        ! Contar o número de linhas no resultado da consulta
        rowcount = 0
        do
            rc = sqlite3_step(db)
            if (rc == 100) then ! SQLITE_ROW
                rowcount = rowcount + 1
            elseif (rc == 101) then ! SQLITE_DONE
                exit
            else
                print *, "Erro ao executar a consulta SQL"
                stop
            end if
        end do

        ! Alocar o array de cartões
        allocate(cartoes(rowcount))

        ! Reiniciar o cursor da consulta
        rc = sqlite3_reset(db)

        ! Recuperar os dados dos cartões físicos
        do i = 1, rowcount
            rc = sqlite3_step(db)
            if (rc /= 100) then ! SQLITE_ROW
                print *, "Erro ao recuperar os dados dos cartões físicos"
                stop
            end if
            cartoes(i)%numero = sqlite3_column_int(db, 0)
            cartoes(i)%limite_credito = sqlite3_column_double(db, 1)
            cartoes(i)%bloqueado = sqlite3_column_int(db, 2) /= 0
            cartoes(i)%eh_debito = sqlite3_column_int(db, 3) /= 0
            cartoes(i)%eh_credito = sqlite3_column_int(db, 4) /= 0
        end do

        ! Finalizar a consulta SQL
        rc = sqlite3_finalize(db)
    end subroutine retrieve_cartoes_fisicos

    ! Função para recuperar os cartões virtuais do banco de dados
    subroutine retrieve_cartoes_virtuais(db, cartoes)
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(in) :: db
        type(CartaoVirtual), dimension(:), allocatable :: cartoes

        character(c_char), dimension(256) :: sql
        integer(c_int) :: rc, rowcount, i
        type(CartaoVirtual) :: cartao

        ! Consulta SQL para recuperar os cartões virtuais
        sql = "SELECT numero, limite_credito, bloqueado, cartao_fisico_associado FROM cartoes_virtuais;"

        ! Preparar a consulta SQL
        rc = sqlite3_prepare_v2(db, sql, len_trim(sql), c_null_ptr, c_null_ptr)
        if (rc /= 0) then
            print *, "Erro ao preparar a consulta SQL"
            stop
        end if

        ! Contar o número de linhas no resultado da consulta
        rowcount = 0
        do
            rc = sqlite3_step(db)
            if (rc == 100) then ! SQLITE_ROW
                rowcount = rowcount + 1
            elseif (rc == 101) then ! SQLITE_DONE
                exit
            else
                print *, "Erro ao executar a consulta SQL"
                stop
            end if
        end do

        ! Alocar o array de cartões
        allocate(cartoes(rowcount))

        ! Reiniciar o cursor da consulta
        rc = sqlite3_reset(db)

        ! Recuperar os dados dos cartões virtuais
        do i = 1, rowcount
            rc = sqlite3_step(db)
            if (rc /= 100) then ! SQLITE_ROW
                print *, "Erro ao recuperar os dados dos cartões virtuais"
                stop
            end if
            cartoes(i)%numero = sqlite3_column_int(db, 0)
            cartoes(i)%limite_credito = sqlite3_column_double(db, 1)
            cartoes(i)%bloqueado = sqlite3_column_int(db, 2) /= 0
            cartoes(i)%cartao_fisico_associado = sqlite3_column_int(db, 3)
        end do

        ! Finalizar a consulta SQL
        rc = sqlite3_finalize(db)
    end subroutine retrieve_cartoes_virtuais

    ! Função para exibir os cartões físicos
    subroutine exibir_cartoes_fisicos(cartoes)
        implicit none
        type(CartaoFisico), dimension(:), intent(in) :: cartoes
        integer :: i

        print *, "Cartões Físicos:"
        do i = 1, size(cartoes)
            print *, "Número: ", cartoes(i)%numero
            print *, "Limite de Crédito: ", cartoes(i)%limite_credito
            print *, "Bloqueado: ", cartoes(i)%bloqueado
            if (cartoes(i)%eh_debito) then
                print *, "Tipo: débito"
            end if
            if (cartoes(i)%eh_credito) then
                print *, "Tipo: crédito"
            end if
        end do
    end subroutine exibir_cartoes_fisicos

    ! Função para exibir os cartões virtuais
    subroutine exibir_cartoes_virtuais(cartoes)
        implicit none
        type(CartaoVirtual), dimension(:), intent(in) :: cartoes
        integer :: i

        print *, "Cartões Virtuais:"
        do i = 1, size(cartoes)
            print *, "Número: ", cartoes(i)%numero
            print *, "Limite de Crédito: ", cartoes(i)%limite_credito
            print *, "Bloqueado: ", cartoes(i)%bloqueado
            print *, "Cartão Físico Associado: ", cartoes(i)%cartao_fisico_associado
        end do
    end subroutine exibir_cartoes_virtuais

    ! Subprograma principal
    type(CartaoFisico), dimension(:), allocatable :: cartoes_fisicos
    type(CartaoVirtual), dimension(:), allocatable :: cartoes_virtuais

    ! Inicializar o banco de dados
    call init_database(db)

    ! Criar as tabelas de cartões físicos e virtuais
    call create_cartoes_fisicos_table(db)
    call create_cartoes_virtuais_table(db)

    ! Inserir cartões físicos no banco de dados
    allocate(cartoes_fisicos(2))
    cartoes_fisicos(1)%numero = 1234
    cartoes_fisicos(1)%limite_credito = 1000.0
    cartoes_fisicos(1)%bloqueado = .false.
    cartoes_fisicos(1)%eh_debito = .true.
    cartoes_fisicos(1)%eh_credito = .true.
    call insert_cartao_fisico(db, cartoes_fisicos(1))

    cartoes_fisicos(2)%numero = 5678
    cartoes_fisicos(2)%limite_credito = 2000.0
    cartoes_fisicos(2)%bloqueado = .true.
    cartoes_fisicos(2)%eh_debito = .false.
    cartoes_fisicos(2)%eh_credito = .true.
    call insert_cartao_fisico(db, cartoes_fisicos(2))

    ! Inserir cartões virtuais no banco de dados
    allocate(cartoes_virtuais(2))
    cartoes_virtuais(1)%numero = 111
    cartoes_virtuais(1)%limite_credito = 500.0
    cartoes_virtuais(1)%bloqueado = .false.
    cartoes_virtuais(1)%cartao_fisico_associado = 1234
    call insert_cartao_virtual(db, cartoes_virtuais(1))

    cartoes_virtuais(2)%numero = 222
    cartoes_virtuais(2)%limite_credito = 1000.0
    cartoes_virtuais(2)%bloqueado = .true.
    cartoes_virtuais(2)%cartao_fisico_associado = 5678
    call insert_cartao_virtual(db, cartoes_virtuais(2))

    ! Recuperar os cartões físicos do banco de dados e exibi-los
    call retrieve_cartoes_fisicos(db, cartoes_fisicos)
    call exibir_cartoes_fisicos(cartoes_fisicos)

    ! Recuperar os cartões virtuais do banco de dados e exibi-los
    call retrieve_cartoes_virtuais(db, cartoes_virtuais)
    call exibir_cartoes_virtuais(cartoes_virtuais)

    ! Fechar o banco de dados
    call sqlite3_close(db)

end program cartoes_crud
