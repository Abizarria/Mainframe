      *-------------------------------------------------------*
      *                  CADASTRO DE ALUNOS                   *
      *-------------------------------------------------------*

       FD  CADALU          RECORD      CONTAINS 60 CHARACTERS           00270058
                           RECORDING   MODE     IS F                    00280001
                           LABEL       RECORD   IS STANDARD             00290001
                           DATA        RECORD   IS REG-ALU.             00300058
                                                                        00310031
       01  REG-ALU.                                                     00320058
           03  COD-ALa     PIC  9(04).                                  00330058
           03  NOM-ALU     PIC  X(30).                                  00340058
           03  TUR-ALU     PIC  9(03).                                  00350058
           03  NT1-ALU     PIC  9(02)V99.                               00360058
           03  NT2-ALU     PIC  9(02)V99.                               00370058
           03  NT3-ALU     PIC  9(02)V99.                               00380058
           03  NT4-ALU     PIC  9(02)V99.                               00390058
           03  FILLER      PIC  X(07).                                  00400058
                                                                        00410032
