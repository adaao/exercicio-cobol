              program-id. Program1 as "Program1".

       environment division.
       configuration section.
       
       special-names.
           decimal-point is comma.
           
       input-output section.
       file-control.
           select arq-conta assign to disk
           organization indexed
           access mode dynamic
           record key cod-id
           file status arq-ok.
          
       data division.
       file section.
       FD arq-conta label record standard
           value of file-id is "arqAgencia.dat".
           
       01 reg-arqAgencia.
           02 dadosDeCodigo.
               03 cod-agencia pic 9(4).
               03 cod-conta-corrente pic 9(4).
           02 cod-id redefines dadosDeCodigo pic 9(8).    
           02 nome pic a(40).
           02 saldo pic s9(9)v99.
           02 cod-cliente pic 9(11).
           
       working-storage section.
       
       01 reg-arqAgencia-e.
   
           02 dadosDeCodigo-e.
               03 cod-agencia-e pic zzz9 value zeros.
               03 cod-conta-corrente-e pic zzz9 value zeros.
           02 nome-e pic a(40) value spaces.
           02 saldo-e pic -zzz.zzz.zz9,99 value zeros.
           02 cod-cliente-e pic zzz.zzz.zzz/z9 value zeros.
      
       01 arq-status.
           02 arq-ok pic x(2) value zeros.
        
       01 dados-string.
           02 inserirSN pic a(1) value space.
           02 espera pic x(1) value space.
** espera serve para o parar o programa, similar a um getchar() no C    
           
       01 dados-int.
           02 opcao pic 9(1) value zeros.
           02 valor-movimentacao pic s9(9)v99.
           02 valor-movimentacao-e pic zzz.zzz.zz9,99.
           02 entrada pic 9(4) value zeros.
           02 saldo-depois-movimentacao pic s9(9)v99.
           02 saldo-atual pic s9(9)v99.
           02 limpar pic 9(4) value zeros.
           
       01 mensagens.
           02 mensaEspera pic x(40) value
               "Pressione uma tecla para continuar".
                           
       screen section.
       01 tela01.
           02 line 01 column 25 value 
           "**** PROGRAMA CONTA CORRENTE ****".
           02 line 03 column 01 value "Digite uma Opcao: ".
           02 line 04 column 01 value "1 - para incluir registro".
           02 line 05 column 01 value "2 - para alterar registro".
           02 line 06 column 01 value "3 - para excluir registro".
           02 line 07 column 01 value "4 - para consultar registro".
           02 line 08 column 01 value "9 - para sair do programa".
           
       01 telaRegistro.
           02 line 01 column 01 value "Codigo da Agencia: ".
           02 line 02 column 01 value "Codigo da Conta Corrente: ".
           02 line 03 column 01 value "Nome: ".
           02 line 04 column 01 value "Saldo: ".
           02 line 05 column 01 value "Codigo cliente (CPF): ".

       01 telaAlterar1.
           02 line 02 column 25 value 
           "**** ALTERACAO DA CONTA CORRENTE ****".
           02 line 04 column 01 value 
           "Digite os codigos de identificacao da conta".                 
	       02 line 06 column 01 value "Codigo da Agencia: ".
           02 line 07 column 01 value "Codigo da Conta Corrente: ".

       01 telaAlterar2.
           02 line 13 column 01 value "Escolha uma opcao ".
           02 line 15 column 01 value " 1 -> Aterar Nome".
           02 line 16 column 01 value " 2 -> Realizar Deposito".
           02 line 17 column 01 value " 3 -> Realizar Saque".

       01 telaConsultar.
           02 line 02 column 25 value 
        "**** CONSULTA CONTA CORRENTE ****".
           02 line 04 column 01 value 
           "Digite os codigos de identificaca da conta ".             
	       02 line 06 column 01 value "Codigo da Agencia: ".
           02 line 07 column 01 value "Codigo da Conta Corrente: ".
       
       01 telaExcluir.
           02 line 02 column 25 value 
       "**** EXCLUIR CONTA CORRENTE ****".
           02 line 04 column 01 value 
           "Digite os codigos de identificaca da conta ".             
	       02 line 06 column 01 value "Codigo da Agencia: ".
           02 line 07 column 01 value "Codigo da Conta Corrente: ".

       procedure division.
       abre.
           perform abre-arq.
       
       inicio.
           perform mostra-tela.
           perform escolhe-opcao.
           perform inicio until opcao=9.
           display spaces.                                              

           stop run.
        
       mostra-tela.
           display erase at 0101.
           display tela01 at 0101.
           
       escolhe-opcao.
           accept opcao at 0319.
           evaluate opcao
           when 1
               perform recebe
           when 2
               perform alterar
           when 3
               perform excluir
           when 4
               perform consultar
           when 9
               display "Saindo do programa" at 2226
           when other
               display "Atencao: Opcao Invalida!" at 2226
               perform escolhe-opcao.
           
       abre-arq.
           open i-o arq-conta.
         
           if arq-ok not ="00"
               close arq-conta
               display "fechando o arquivo" AT 0101
               open output arq-conta.

       limpar-campos.
           move zeros to dadosDeCodigo-e.
           move spaces to nome-e.
           move zeros to saldo-e.
           move spaces to dados-string.
           move zeros to cod-cliente-e.
           move zeros to valor-movimentacao-e.
           move zeros to entrada.
           move zeros to limpar.
           move zeros to opcao.
       
       recebe.
           display spaces at 1101.
           display spaces at 1201.
           move 2001 to limpar.
           display "Digite os seguintes campos" at 1101.
           display telaRegistro at 1201.
           move 1220 to entrada.
           perform testa-cod-agencia.
           move 1327 to entrada.
           perform testa-cod-conta-corrente.
           perform testa-cod-id.
           move 1407 to entrada.
           perform testa-nome.
           move 1507 to entrada.
           perform testa-saldo.
           perform chama-validarCPF.
           perform inserir.
          
       testa-cod-agencia.
           accept cod-agencia-e at entrada.
           move cod-agencia-e to cod-agencia.
           if cod-agencia= zeros                                        
               display "Codigo nao pode ser igual a zero" at limpar     
               perform testa-cod-agencia.                               
           display spaces at limpar.                                    

       testa-cod-conta-corrente.
           accept cod-conta-corrente-e at entrada.
           move cod-conta-corrente-e to cod-conta-corrente.
           if cod-conta-corrente=zeros
               display "Codigo nao pode ser igual a zero" at limpar
               perform testa-cod-conta-corrente.
           display spaces at limpar.
           
       testa-cod-id.
           read arq-conta invalid key display spaces at 2001
           not invalid key
               display "Conta corrente ja cadastrada nesta agencia" 
                   at limpar
               move 1220 to entrada
               perform testa-cod-agencia
               move 1327 to entrada
               perform testa-cod-conta-corrente
               perform testa-cod-id.
           
       testa-nome.
           accept nome-e at entrada.
           move nome-e to nome.
           if nome=spaces
               display "Nome nao pode ser branco" at limpar
               perform testa-nome.
           display spaces at limpar.
           
       testa-saldo.
           accept saldo-e at entrada.
           move saldo-e to saldo.                                       
           if saldo=zeros                                               
               display "Valor deve ser maior que zero" 
                   at limpar
                   perform testa-saldo.
           display spaces at limpar.
       
       testa-valor-movimentacao.
           accept valor-movimentacao-e at entrada.
           move valor-movimentacao-e to valor-movimentacao.           
           if valor-movimentacao=zeros                                  
               display "Valor deve ser maior que zero" 
                   at limpar
                   perform testa-valor-movimentacao.
           display spaces at limpar.
       
       chama-validarCPF.
           call "validarCPF" using by reference cod-cliente.
           move cod-cliente to cod-cliente-e.
           
       inserir.
           display "Deseja inserir no arquivo os dados digitados (s/n)?"
               at 1801.
           accept inserirSN at 1852.
           if inserirSN="s" or "S"
               write reg-arqAgencia invalid key perform verifica-erro
               not invalid key display "Dados Inseridos com sucesso" at 
               2001
           stop " " 
                   perform pausar
               end-write
               move saldo to  saldo-depois-movimentacao
               call "REG-EXTR" using by reference cod-id, saldo, 
               saldo-depois-movimentacao
           else
               if inserirSN ="n" or "N" 
                   display spaces at 1901
                   display mensaEspera at 2001 
                   accept espera at 2040 auto
                       else
                           display "digite s ou n" at 2001
                           perform inserir.
           perform limpar-campos.

       consultar.
           move 2001 to limpar.
           display erase at 0101.
           display telaConsultar at 0101.
           move 0620 to entrada.
           perform testa-cod-agencia.
           move 0727 to entrada.
           perform testa-cod-conta-corrente.
           perform ler-consulta.
           if arq-ok not= 23
               display "< Informacoes da Conta >" at 1001
               display 
               "Deseja visualizar movimentacoes da conta?" at 1901
               display "1 -> Sim " at 2001
               display "2 -> Nao " at 2101
               perform visualizar-extrato.
           perform limpar-campos.
            
       visualizar-extrato.
           accept opcao at 1945
           evaluate opcao
           when = 1
               call "CONS-EXTR" using by reference cod-id
           when = 2
               display "Consulta Encerrada" at 2601
           stop " "
           when other 
               display "Opcao Invalida" at 2601
               perform visualizar-extrato.
           
       ler-consulta.    
           read arq-conta invalid key
               display "Codigo nao encontrado" at 2226
               stop " "
               not invalid key
               move cod-agencia to cod-agencia-e
               move cod-conta-corrente to cod-conta-corrente-e
               move nome to nome-e
               move saldo to saldo-e
               move cod-cliente to cod-cliente-e
               display telaRegistro at 1201
               display cod-agencia-e at 1220
               display cod-conta-corrente-e at 1326
               display nome-e at 1408
               display saldo-e at 1508
               display cod-cliente-e at 1623
               move saldo to saldo-atual 
           end-read.
           perform limpar-campos.
       
       verifica-opcao.
           accept opcao at entrada.
           display spaces at 2601
           evaluate opcao
               when = 1 or = 2 or = 3
               next sentence
           when other
               display "opcao invalida" at 2601
               perform verifica-opcao.

       alterar.
           display erase at 0101.
           display telaAlterar1 at 0101.
           move 2001 to limpar
           move 0620 to entrada.
           perform testa-cod-agencia.
           move 0727 to entrada.
           perform testa-cod-conta-corrente.     
           perform ler-consulta.
           if arq-ok not= 23
               display " < Informacoes da Conta >" at 1001
               move zero to opcao
               move 2901 to limpar
               display telaAlterar2 at 0701
               move 1920 to entrada
               perform verifica-opcao
               evaluate opcao
               when = 1
                   perform alteraNome
               when = 2
                   move 2901 to limpar
                   perform alteraSaldo1
               when = 3
                   move 2901 to limpar
                   perform alteraSaldo2
           else 
               perform limpar-campos
               perform pausar.
          
       alteraNome.    
           display "Digite o novo nome do clinte: " at 2601.
           move 2631 to entrada.
           perform testa-nome.
      
           perform gravar-registro.
           perform limpar-campos.
           
       alteraSaldo1.    
           display "Informe o valor do deposito" at 2601.
           move 2630 to entrada.
           perform testa-valor-movimentacao.
           perform deposito.
           
       alteraSaldo2.
           display "Informe o valor do saque" at 2601.
           move 2630 to entrada.
           perform testa-valor-movimentacao.
           if (valor-movimentacao > saldo-atual)
               display "Saldo Insuficiente" at 2901
               perform limpar-campos
           stop "  "
           else
               perform saque.
           
       saque.
           compute saldo-depois-movimentacao = saldo-atual -
           valor-movimentacao.
           call "REG-EXTR" using by reference cod-id, saldo-atual
           saldo-depois-movimentacao.
           move saldo-depois-movimentacao to saldo.
           perform gravar-registro.
           perform limpar-campos.
           
       deposito.
           compute saldo-depois-movimentacao = saldo-atual + 
           valor-movimentacao.
           call "REG-EXTR" using by reference cod-id, saldo
           saldo-depois-movimentacao.
           move saldo-depois-movimentacao to saldo.
           perform gravar-registro.
           perform limpar-campos.
               
       gravar-registro.
           rewrite reg-arqAgencia invalid key perform verifica-erro
               end-rewrite.

                display "Operacao Finalizada" at 2801.
           stop "  "                                          
            perform limpar-campos.
       
       pausar.
           display erase at 0101.
           display mensaEspera at 2525.
           accept espera at 2570  auto.
           
       excluir.
           move 2001 to limpar.
           display erase at 0101.
           display telaExcluir at 0101.
           move 0620 to entrada.
           perform testa-cod-agencia.
           move 0727 to entrada.
           perform testa-cod-conta-corrente.
           perform ler-consulta.
           if arq-ok not = 23
               display "< Informacoes da Conta> " at 1001
               perform excluir-confirmacao.
           stop " "
           perform limpar-campos.
           
       excluir-confirmacao.
           display "Excluir esse registro? (s/n)" at 1901.
           accept inserirSN at 1930.
           evaluate inserirSN
           when = "s" or "S"
               delete arq-conta invalid key perform verifica-erro
               end-delete
               display "Registro exluido com sucesso" at 2101
           when = "n" or "N"
               display spaces at 1801
               display "Registro nao excluido" at 2101
           when other
               display "digite s ou n" at 2101
               perform excluir-confirmacao.
                           
       verifica-erro.    
           evaluate arq-ok                                              
           when 10                                                      
               display "Erro - Programa encontrou fim do arquivo EOF" 
               at 2001
           when 22 
               display "Erro - Chave duplicada" at 2001
           when 23
               display "Error - Chave nao encontrada" at 2001. 
           end program Program1.

