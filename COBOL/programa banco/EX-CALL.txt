**** PGM  MENU ****

ENVIRONMENT DIVISION.
       DATA DIVISION.
           
	   WORKING-STORAGE SECTION.
       
       77 WS-DATA-SISTEMA   PIC X(19) VALUE SPACES.
       
       01 DATA-SISTEMA.
          02 ANO   PIC 9(04).
          02 MES   PIC 9(02).
          02 DIA   PIC 9(02).
          02 HORA  PIC 9(02).
          02 MIN   PIC 9(02).
          02 SEG   PIC 9(02).
           
       LINKAGE SECTION.
       77 LKU PIC X(10).
           
PROCEDURE DIVISION USING LK-USUARIO.
    
       
           ACCEPT OPCAO AT 1010.
           EVALUATE TRUE
           WHEN  OPCAO EQUAL "1"
               CALL "CAD-CLIENTE" USING LKU
           WHEN  WS-OPCAO EQUAL "2"
               CALL "CAD-CONTA"   USING LKU
           WHEN  WS-OPCAO EQUAL "3"
               CALL "LIS-CLIENTE" USING LKU
           WHEN  WS-OPCAO EQUAL "4"
               CALL "LIS-CONTA" USING LKU
           WHEN  WS-OPCAO EQUAL "5"
               CALL "LIS-CONTA-CLIENTE" USING LKU
           WHEN  WS-OPCAO EQUAL "9"
               STOP RUN
           WHEN  OTHER
               MOVE "OPCAO INVALIDA" TO MENSAGEM
               PERFORM EXIBIR-MENSAGEM.


***** PGM CAD-CLIENTE - OPÇÃO 1 ******

DATA DIVISION.
       
           
	   WORKING-STORAGE SECTION.
LINKAGE SECTION.
       77 LKU PIC X(10).

PROCEDURE DIVISION USING USUARIO.

......
......
......
......

CALL "MENU" USING LKU



