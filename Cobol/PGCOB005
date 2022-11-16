       IDENTIFICATION      DIVISION.                                    00010038
       PROGRAM-ID.         ABSPG005.                                    00020056
       AUTHOR.             ADILSON BIZARRIA.                            00030038
       INSTALLATION.       FUTURE SCHOOL.                               00040038
       DATE-WRITTEN.       10/10/2022.                                  00050057
       DATE-COMPILED.                                                   00060025
      *--------------------------------------------------------*        00070070
      * EXER-05 - GERAR RELACAO DE ALUNOS A PARTIR DO CADALUNO *        00080070
      *--------------------------------------------------------*        00090070
       ENVIRONMENT         DIVISION.                                    00100001
       CONFIGURATION       SECTION.                                     00110001
       SOURCE-COMPUTER.    IBM-3090.                                    00120029
       OBJECT-COMPUTER.    IBM-3090.                                    00130029
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                      00140058
       INPUT-OUTPUT        SECTION.                                     00150001
                                                                        00160030
       FILE-CONTROL.                                                    00170001
           SELECT CADALU   ASSIGN  TO  UT-S-CADALU                      00180065
                  FILE     STATUS  IS  FST-ALU.                         00190065
                                                                        00200000
           SELECT RELATO   ASSIGN  TO  UT-S-RELATO                      00210065
                  FILE     STATUS  IS  FST-REL.                         00220065
                                                                        00230031
       DATA                DIVISION.                                    00240001
       FILE                SECTION.                                     00250008
                                                                        00260031
       FD  CADALU          RECORD      CONTAINS 60 CHARACTERS           00270058
                           RECORDING   MODE     IS F                    00280001
                           LABEL       RECORD   IS STANDARD             00290001
                           DATA        RECORD   IS REG-ALU.             00300058
                                                                        00310031
       01  REG-ALU.                                                     00320058
           03  COD-ALU     PIC  9(04).                                  00330058
           03  NOM-ALU     PIC  X(30).                                  00340058
           03  TUR-ALU     PIC  9(03).                                  00350058
           03  NT1-ALU     PIC  9(02)V99.                               00360058
           03  NT2-ALU     PIC  9(02)V99.                               00370058
           03  NT3-ALU     PIC  9(02)V99.                               00380058
           03  NT4-ALU     PIC  9(02)V99.                               00390058
           03  FILLER      PIC  X(07).                                  00400058
                                                                        00410032
       FD  RELATO          RECORD      CONTAINS 77 CHARACTERS           00420061
                           RECORDING   MODE     IS F                    00430001
                           LABEL       RECORD   IS OMITTED              00440058
                           DATA        RECORD   IS REG-REL.             00450058
                                                                        00460032
       01  REG-REL         PIC  X(77).                                  00470058
                                                                        00480032
       WORKING-STORAGE SECTION.                                         00490001
                                                                        00500032
       77  FST-ALU         PIC  X(02).                                  00510060
       77  FST-REL         PIC  X(02).                                  00520060
                                                                        00530034
       77  FST-ERR         PIC  X(02).                                  00540034
       77  COD-ERR         PIC  9(01)  VALUE ZEROS.                     00550066
       77  OPR-ERR         PIC  X(13).                                  00560005
       77  ARQ-ERR         PIC  X(08).                                  00570046
                                                                        00580033
       77  ACM-LIN         PIC  9(02)  VALUE  99.                       00590066
       77  ACM-PAG         PIC  9(04)  VALUE  ZEROS.                    00600066
       77  ACM-LID         PIC  9(12)  COMP-3 VALUE ZEROS.              00610066
       77  ACM-IMP         PIC  9(12)  COMP-3 VALUE ZEROS.              00620066
                                                                        00630033
       77  MSK-LID         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00640034
       77  MSK-IMP         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00650059
                                                                        00660033
       01  DTA-SYS.                                                     00670059
           03  ANO-SYS     PIC  9(04).                                  00680059
           03  MES-SYS     PIC  9(02).                                  00690059
           03  DIA-SYS     PIC  9(02).                                  00700059
           03  HRA-SYS     PIC  9(02).                                  00710059
           03  MIN-SYS     PIC  9(02).                                  00720059
           03  FILLER      PIC  X(09).                                  00730059
                                                                        00740059
       01  CAB-001.                                                     00750058
           03  FILLER      PIC  X(66)  VALUE                            00760058
               'FUTURE SCHOOL CURSOS DE COMPUTACAO'.                    00770058
           03  FILLER      PIC  X(06)  VALUE  'PAG.:'.                  00780058
           03  PAG-CB1     PIC  Z.ZZ9.                                  00790058
                                                                        00800058
       01  CAB-002.                                                     00810058
           03  FILLER      PIC  X(51)  VALUE  'PROGRAMA = ABSPG005'.    00820058
           03  HRA-CB2     PIC  9(02).                                  00830058
           03  FILLER      PIC  X(01)  VALUE  ":".                      00840062
           03  MIN-CB2     PIC  9(02).                                  00850058
           03  FILLER      PIC  X(11)  VALUE  SPACES.                   00860058
           03  DIA-CB2     PIC  9(02).                                  00870058
           03  FILLER      PIC  X(01)  VALUE  "/".                      00880062
           03  MES-CB2     PIC  9(02).                                  00890058
           03  FILLER      PIC  X(01)  VALUE  "/".                      00900062
           03  ANO-CB2     PIC  9(04).                                  00910058
                                                                        00920058
       01  CAB-003.                                                     00930058
           03  FILLER      PIC  X(23)  VALUE  SPACES.                   00940058
           03  FILLER      PIC  X(54)  VALUE                            00950058
               'RELATORIO DO CADASTRO DE ALUNOS'.                       00960058
                                                                        00970058
       01  CAB-004.                                                     00980058
           03  FILLER      PIC  X(14)  VALUE  'CODIGO'.                 00990058
           03  FILLER      PIC  X(26)  VALUE  'NOME DO ALUNO'.          01000058
           03  FILLER      PIC  X(17)  VALUE  'TURMA    NOTA'.          01010058
           03  FILLER      PIC  X(20)  VALUE  'NOTA    NOTA    NOTA'.   01020058
                                                                        01030058
       01  CAB-005.                                                     01040058
           03  FILLER      PIC  X(48)  VALUE  SPACES.                   01050058
           03  FILLER      PIC  X(16)  VALUE  '1.BIM   2.BIM'.          01060058
           03  FILLER      PIC  X(13)  VALUE  '3.BIM   4.BIM'.          01070058
                                                                        01080058
       01  DET-001.                                                     01090058
           03  FILLER      PIC  X(01)  VALUE  SPACES.                   01100058
           03  COD-DT1     PIC  9(04).                                  01110058
           03  FILLER      PIC  X(03)  VALUE  SPACES.                   01120058
           03  NOM-DT1     PIC  X(33).                                  01130058
           03  TUR-DT1     PIC  9(03).                                  01140058
           03  FILLER      PIC  X(04)  VALUE  SPACES.                   01150058
           03  NT1-DT1     PIC  Z9,99.                                  01160058
           03  FILLER      PIC  X(03)  VALUE  SPACES.                   01170058
           03  NT2-DT1     PIC  Z9,99.                                  01180058
           03  FILLER      PIC  X(03)  VALUE  SPACES.                   01190058
           03  NT3-DT1     PIC  Z9,99.                                  01200058
           03  FILLER      PIC  X(03)  VALUE  SPACES.                   01210058
           03  NT4-DT1     PIC  Z9,99.                                  01220058
                                                                        01230058
       PROCEDURE        DIVISION.                                       01240064
                                                                        01250033
       000-INICIO.                                                      01260001
           OPEN    INPUT      CADALU                                    01270064
                   OUTPUT     RELATO.                                   01280064
                                                                        01290068
           IF  FST-ALU  =  '00' AND  FST-REL  =  '00'                   01300071
               PERFORM  100-IMP-REL  UNTIL  FST-ALU = '10'              01310071
           ELSE                                                         01320003
               MOVE  1  TO  COD-ERR                                     01330071
               PERFORM  200-ERR-ARQ                                     01340071
           END-IF                                                       01350003
                                                                        01360035
           CLOSE    CADALU   RELATO                                     01370071
                                                                        01380006
           IF  FST-ALU = '00' AND FST-REL  =  '00'                      01390060
               MOVE  ACM-LID  TO  MSK-LID                               01400006
               MOVE  ACM-IMP  TO  MSK-IMP                               01410059
                                                                        01420035
               DISPLAY '-> ABSPG005 <-> ESTATISTICA DE PROCESSAMENTO <-'01430056
               DISPLAY '|                                             |'01440006
               DISPLAY '| REGISTROS LIDOS (CADALUNO)  ' MSK-LID ' |'    01450060
               DISPLAY '|                                             |'01460006
               DISPLAY '| REGISTROS IMPRESSOS (RELATO)' MSK-IMP ' |'    01470060
               DISPLAY '|                                             |'01480006
               DISPLAY '-----------------------------------------------'01490006
                                                                        01500035
               STOP  RUN                                                01510042
           ELSE                                                         01520042
               MOVE  4  TO  COD-ERR                                     01530063
               PERFORM  200-ERR-ARQ                                     01540063
           END-IF.                                                      01550042
                                                                        01560035
       100-IMP-REL.                                                     01570060
           READ      CADALU                                             01580060
                                                                        01590005
           IF  FST-ALU  =   '00'                                        01600060
               ADD   1  TO  ACM-LID                                     01610054
                                                                        01620054
               IF  ACM-LIN  >    59                                     01630063
                   PERFORM       110-IMP-CAB                            01640063
               END-IF                                                   01650063
                                                                        01660063
               MOVE     COD-ALU  TO  COD-DT1                            01670063
               MOVE     NOM-ALU  TO  NOM-DT1                            01680063
               MOVE     TUR-ALU  TO  TUR-DT1                            01690063
               MOVE     NT1-ALU  TO  NT1-DT1                            01700063
               MOVE     NT2-ALU  TO  NT2-DT1                            01710063
               MOVE     NT3-ALU  TO  NT3-DT1                            01720063
               MOVE     NT4-ALU  TO  NT4-DT1                            01730064
               WRITE    REG-REL FROM DET-001  AFTER  1                  01740071
                                                                        01750060
               IF  FST-REL   =  '00'                                    01760064
                   ADD       1   TO  ACM-IMP                            01770064
                   ADD       1   TO  ACM-LIN                            01780064
               ELSE                                                     01790060
                   MOVE      3   TO  COD-ERR                            01800064
                   PERFORM       200-ERR-ARQ                            01810064
               END-IF                                                   01820060
           ELSE                                                         01830005
               IF  FST-ALU  NOT  =   '10'                               01840064
                   MOVE      2   TO  COD-ERR                            01850064
                   PERFORM       200-ERR-ARQ                            01860064
               END-IF                                                   01870043
           END-IF.                                                      01880005
                                                                        01890035
       110-IMP-CAB.                                                     01900060
           MOVE    FUNCTION     CURRENT-DATE  TO  DTA-SYS               01910060
                                                                        01920064
           ADD     1        TO  ACM-PAG                                 01930064
           MOVE    ACM-PAG  TO  PAG-CB1                                 01940064
           MOVE    HRA-SYS  TO  HRA-CB2                                 01950060
           MOVE    MIN-SYS  TO  MIN-CB2                                 01960060
           MOVE    DIA-SYS  TO  DIA-CB2                                 01970060
           MOVE    MES-SYS  TO  MES-CB2                                 01980060
           MOVE    ANO-SYS  TO  ANO-CB2                                 01990060
                                                                        02000060
           IF  ACM-PAG  =   1                                           02010064
               WRITE   REG-REL FROM CAB-001  AFTER  0                   02020071
           ELSE                                                         02030064
               WRITE   REG-REL FROM CAB-001  AFTER  PAGE                02040071
           END-IF                                                       02050064
                                                                        02060064
           WRITE   REG-REL FROM CAB-002  AFTER  1                       02070071
           WRITE   REG-REL FROM CAB-003  AFTER  2                       02080071
           WRITE   REG-REL FROM CAB-004  AFTER  2                       02090071
           WRITE   REG-REL FROM CAB-005  AFTER  1                       02100071
           MOVE    SPACES   TO  REG-REL                                 02110060
           WRITE   REG-REL               AFTER  1                       02120071
           MOVE    8        TO  ACM-LIN.                                02130060
                                                                        02140060
       200-ERR-ARQ.                                                     02150005
           IF  COD-ERR  =  1  AND  FST-ALU = '10'                       02160060
               DISPLAY '-----------> ABSPG005 <-----------'             02170056
               DISPLAY '|                                |'             02180005
               DISPLAY '|     ARQUIVO CADALUNO VAZIO     |'             02190060
               DISPLAY '|                                |'             02200005
               DISPLAY '|       PROGRAMA ENCERRADO!      |'             02210005
               DISPLAY '|                                |'             02220005
               DISPLAY '----------------------------------'             02230005
           ELSE                                                         02240005
               EVALUATE COD-ERR                                         02250005
                   WHEN 1                                               02260005
                        MOVE  'NA ABERTURA'    TO  OPR-ERR              02270005
                   WHEN 2                                               02280005
                        MOVE  'NA LEITURA'     TO  OPR-ERR              02290005
                   WHEN 3                                               02300005
                        MOVE  'NA IMPRESSAO'   TO  OPR-ERR              02310060
                   WHEN 4                                               02320005
                        MOVE  'NO FECHAMENTO'  TO  OPR-ERR              02330005
               END-EVALUATE                                             02340020
                                                                        02350035
               IF  FST-ALU  >  '00' AND (COD-ERR =  1 OR 2 OR 4)        02360060
                   MOVE     FST-ALU  TO  FST-ERR                        02370060
                   MOVE     'CADALUNO'   TO  ARQ-ERR                    02380060
               ELSE                                                     02390005
                   MOVE     FST-REL  TO  FST-ERR                        02400060
                   MOVE     'RELATO' TO  ARQ-ERR                        02410060
               END-IF                                                   02420020
                                                                        02430035
               DISPLAY '--------------> ABSPG005 <----------------'     02440056
               DISPLAY '|                                        |'     02450046
               DISPLAY '| ERRO ' OPR-ERR ' DO ARQUIVO ' ARQ-ERR ' |'    02460038
               DISPLAY '|                                        |'     02470046
               DISPLAY '|            FILE-STATUS: ' FST-ERR '           02480021
      -                '  |'                                            02490046
               DISPLAY '|                                        |'     02500046
               DISPLAY '|           PROGRAMA ENCERRADO!          |'     02510046
               DISPLAY '|                                        |'     02520046
               DISPLAY '------------------------------------------'     02530046
           END-IF.                                                      02540005
                                                                        02550035
           STOP    RUN.                                                 02560006
