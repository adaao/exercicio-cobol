                    IDENTIFICATION DIVISION.
       PROGRAM-ID REG-EXTR.

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
       01 ARQ-OK PIC X(2).
       
       LINKAGE SECTION.
       01 LS-COD-ID PIC 9(8).
       01 LS-NOME PIC A(40).
       01 LS-SD-ANTERIOR PIC S9(9)V99.
       01 LS-SD-ATUAL PIC S9(9)V99.

       PROCEDURE DIVISION USING LS-COD-ID, LS-SD-ANTERIOR, 
       LS-SD-ATUAL.
       
           OPEN EXTEND ARQ-EXTR.
               MOVE LS-COD-ID TO CODIGO.
               SUBTRACT LS-SD-ANTERIOR FROM LS-SD-ATUAL GIVING VL-MOV.
               MOVE LS-SD-ATUAL TO SD-ATUAL.
               ACCEPT DATA-SISTEMA FROM DATE.
               WRITE R-EXTR.

           CLOSE ARQ-EXTR.
       EXIT PROGRAM.