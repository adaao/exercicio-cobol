INICIO.
           PERFORM ABRIR-TELAINICIAL.           
           PERFORM MENU UNTIL WS-OPCAO EQUAL "9".
           
       MENU.
           PERFORM EXIBIR-HORARIO.
           
           ACCEPT WS-OPCAO AT 2113.
           EVALUATE TRUE
           WHEN  WS-OPCAO EQUAL "1"
               CALL "CAD-CLIENTE" USING LK-USUARIO
           WHEN  WS-OPCAO EQUAL "2"
               CALL "CAD-CONTA"   USING LK-USUARIO
           WHEN  WS-OPCAO EQUAL "3"
               CALL "LISTAR-CLIENTE" USING LK-USUARIO
           WHEN  WS-OPCAO EQUAL "4"
               CALL "LISTAR-CONTA" USING LK-USUARIO
           WHEN  WS-OPCAO EQUAL "5"
               CALL "LISTAR-CONTA-CLIENTE" USING LK-USUARIO
           WHEN  WS-OPCAO EQUAL "9"
               STOP RUN
           WHEN  OTHER
               MOVE "OPCAO INVALIDA" TO WS-MENSAGEM
               PERFORM EXIBIR-MENSAGEM.