       IDENTIFICATION DIVISION.
       PROGRAM-ID CONS-EXTR.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-EXTR ASSIGN TO DISK
           ORGANIZATION LINE SEQUENTIAL
           ACCESS MODE SEQUENTIAL
           FILE STATUS ARQ-OK.

       DATA DIVISION.

       FILE SECTION.
           FD ARQ-EXTR LABEL RECORD STANDARD
           RECORD CONTAINS 77
           DATA RECORD IS REG-EXTR
           VALUE OF FILE-ID IS "ARQEXTR.DAT".

       01 R-EXTR.
           02 DATA-SISTEMA.
               03 ANO PIC 99.
               03 MES PIC 99.
               03 DIA PIC 99.
           02 CODIGO.
               03 AGENCIA PIC 9(4).
               03 CONTA PIC 9(4).
           02 VL-MOV PIC S9(9)v99.
           02 SD-ATUAL PIC S9(9)v99.
           66 COD-ID RENAMES AGENCIA THRU CONTA.

       WORKING-STORAGE SECTION.
       01 LINHA PIC 99 VALUE ZEROS.
       01 ARQ-OK PIC X(2).
       01 EOF PIC 9 VALUE ZERO.
       
       01 DADOS.
           02 CODIGO-E.
               03 AGENCIA-E PIC ZZZ9.
               03 CONTA-E PIC ZZZ9.
           02 VL-MOV-E PIC ---------9,99.
           02 SD-ATUAL-E PIC -ZZZZZZZZ9,99.
       
       01 POSICOES.
           02 POS-DT PIC 9(4) VALUE 0602.
           02 POS-AG PIC 9(4) VALUE 0611.
           02 POS-CT PIC 9(4) VALUE 0619.
           02 POS-MOV PIC 9(4) VALUE 0625.
           02 POS-SD PIC 9(4) VALUE 0640.

       LINKAGE SECTION.
       01 LS-COD-ID PIC 9(8).
       
       SCREEN SECTION.
       01 TELA01.
           02 LINE 02 COLUMN 30 "EXTRATO DA CONTA".
           02 LINE 04 COLUMN 02 "DATA".
           02 LINE 04 COLUMN 11 "AGENCIA".
           02 LINE 04 COLUMN 19 "CONTA".
           02 LINE 04 COLUMN 25 "MOVIMENTACAO".
           02 LINE 04 COLUMN 40 "SALDO ATUAL".
       
       PROCEDURE DIVISION USING LS-COD-ID.
           DISPLAY ERASE AT 0101.
           DISPLAY TELA01 AT 0101.
           OPEN INPUT ARQ-EXTR.
           PERFORM IMPRIMIR UNTIL EOF EQUAL 1.
           CLOSE ARQ-EXTR.
           DISPLAY "PRESSIONE QUALQUER TECLA" AT 2030.
           STOP " ".
           PERFORM REINICIA.
           EXIT PROGRAM.
           
       IMPRIMIR.
           READ ARQ-EXTR AT END MOVE 1 TO EOF.
           IF EOF = 0 AND COD-ID = LS-COD-ID
               MOVE AGENCIA TO AGENCIA-E
               MOVE CONTA TO CONTA-E
               MOVE VL-MOV TO VL-MOV-E
               MOVE SD-ATUAL TO SD-ATUAL-E
               DISPLAY DIA AT POS-DT
               DISPLAY "/", MES, "/", ANO
               DISPLAY AGENCIA-E AT POS-AG
               DISPLAY CONTA-E AT POS-CT
               DISPLAY VL-MOV-E AT POS-MOV
               DISPLAY SD-ATUAL-E AT POS-SD
               ADD 100 TO POS-AG, POS-CT, POS-MOV, POS-SD, POS-DT
               ADD 1 TO LINHA
               IF LINHA = 10
                   DISPLAY "PRESSIONE QUALQUER TECLA" AT 2030
                   STOP " "
                   SUBTRACT 1000 FROM POS-AG, POS-CT, POS-MOV, POS-SD, 
                   POS-DT
                   DISPLAY ERASE AT 0101
                   DISPLAY TELA01 AT 0101
                   MOVE 0 TO LINHA..
                   
       REINICIA.
           MOVE 0602 TO POS-DT.
           MOVE 0611 TO POS-AG.
           MOVE 0619 TO POS-CT.
           MOVE 0625 TO POS-MOV.
           MOVE 0640 TO POS-SD.
           MOVE 0 TO EOF.
           MOVE 0 TO LINHA.
