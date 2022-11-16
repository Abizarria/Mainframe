//JEXPG009 JOB IBMUSERA,MSGCLASS=A,CLASS=A,NOTIFY=ALU0407,TIME=(0,01)   00010099
//JOBLIB   DD  DISP=SHR,DSN=FUTURE.LMODUL.COBOL                         00020099
//*                                                                     00030075
//STEP1   EXEC PGM=ABSPG009                                             00040099
//*-------------------------------------------------------------*       00050099
//* EXER-08 - GERAR LISTA DE MATERIAS E CALCULAR PRECO DE VENDA *       00060099
//*-------------------------------------------------------------*       00070099
//*                                                                     00080075
//CADCOT   DD  DSN=FUTURE.ARQUIVOS.CADCOTA,                             00090099
//             DISP=SHR,VOL=SER=PR39D7,UNIT=3390                        00100099
//*                                                                     00110084
//CADSEL   DD  DSN=ABS.AULAS.CADSEL.PG009,                              00141099
//*            DISP=(NEW,CATLG,DELETE),                                 00142099
//             DISP=SHR,                                                00143099
//             DCB=(RECFM=FB,LRECL=040,BLKSIZE=0400),                   00144099
//             SPACE=(TRK,(3,3),RLSE),UNIT=3390                         00145099
//*                                                                     00146099
//CADDSP   DD  DSN=ABS.AULAS.CADDSP.PG009,                              00147099
//*            DISP=(NEW,CATLG,DELETE),                                 00148099
//             DISP=SHR,                                                00149099
//             DCB=(RECFM=FB,LRECL=040,BLKSIZE=0400),                   00149199
//             SPACE=(TRK,(3,3),RLSE),UNIT=3390                         00149299
//*                                                                     00149399
//SYSPRINT DD  SYSOUT=*                                                 00150099
//SYSOUT   DD  SYSOUT=*                                                 00160099
//SYSUDUMP DD  SYSOUT=Y                                                 00170099
//SYSABEND DD  SYSOUT=*                                                 00180099
//*                                                                     00190075
