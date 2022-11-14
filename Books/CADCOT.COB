      *-------------------------------------------------------*
      *                  CADASTRO DE COTACOES                 *
      *-------------------------------------------------------*

       FD  CADCOT          RECORD      CONTAINS 80 CHARACTERS           00320059
                           RECORDING   MODE     IS F                    00330001
                           LABEL       RECORD   IS STANDARD             00340001
                           DATA        RECORD   IS REG-COT.             00350059
                                                                        00360031
       01  REG-COT.                                                     00370059
           03  COD-COT         PIC  9(05).                              00380059
           03  QTD-COT         PIC  9(05).                              00390059
           03  DTA-COT         PIC  9(06).                              00400059
           03  C01-COT.                                                 00410059
               05  FR1-COT     PIC  9(05).                              00420059
               05  VC1-COT     PIC  9(13)V99.                           00430059
           03  C02-COT.                                                 00440059
               05  FR2-COT     PIC  9(05).                              00450059
               05  VC2-COT     PIC  9(13)V99.                           00460059
           03  C03-COT.                                                 00470059
               05  FR3-COT     PIC  9(05).                              00480059
               05  VC3-COT     PIC  9(13)V99.                           00490059
           03  FILLER          PIC  X(04).                              00500059
                                                                        00510032
