IDENTIFICATION DIVISION.
       PROGRAM-ID.  Calaulo-Area-Circunferencia.
       ENVIRONMENT DIVISION.

       special-names.
           decimal-point is comma.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 DADOS.
           02 w-Raio      PIC  9(03)v99.
           02 w-Area      PIC  9(05)V99.
       
           
       01 MENSAGEMS-DE-TELA.
           02 MENSA1            PIC X(50) VALUE
                "DIGITE O Raio".
           02 MENSA2            PIC X(30) VALUE
                "F I M  D O  P R O G R A M A".
           02 MENSA3            PIC X(30) VALUE SPACE.

       01 DATA-DO-SISTEMA.
           02 ANO               PIC 9(02) VALUE ZEROS.
           02 MES               PIC 9(02) VALUE ZEROS.
           02 DIA               PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
       01 TELA01.
           02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
           02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
           02 LINE 02 COLUMN 11 PIC 9(02)  USING ANO.
           02 LINE 02 COLUMN 28 VALUE
                "Cálcula da Área de um Circulo".
           02 LINE 08 COLUMN 15 VALUE "Raio:".
           02 LINE 10 COLUMN 15 VALUE "Área:".
 
       PROCEDURE DIVISION.

       Inicio.

           ACCEPT  DATA-DO-SISTEMA FROM DATE.

           DISPLAY ERASE       AT    0101.

           DISPLAY TELA01      AT    0101.

           MOVE    ZEROS       TO    DADOS.
      
       Entrada.

           DISPLAY MENSA1 AT 2030.

           ACCEPT w-Raio AT 0821.
           

           DISPLAY MENSA3 AT 1830.

       Calcula.
           
           compute w-Area = 3,1416*(w-Raio**2).
           
           Display w-Area AT 1021.

       Finaliza.

           DISPLAY MENSA2 AT 1830.
           stop " ".

	        Stop Run.
