//JEXPG006 JOB IBMUSERA,MSGCLASS=A,CLASS=A,NOTIFY=ALU0407,TIME=(0,01)   00010099
//JOBLIB   DD  DISP=SHR,DSN=FUTURE.LMODUL.COBOL                         00020099
//*                                                                     00030075
//STEP1   EXEC PGM=ABSPG006                                             00040099
//*--------------------------------------------------------*            00050099
//* EXER-06 - GERAR RELATORIO ANALITICO DE PECAS COM TOTAL *            00060099
//*--------------------------------------------------------*            00070099
//*                                                                     00080075
//CADPEC   DD  DSN=FUTURE.ARQUIVOS.CADPECA,                             00090099
//             DISP=SHR,VOL=SER=PR39C7,UNIT=3390                        00100099
//*                                                                     00110084
//*RELATO   DD  SYSOUT=T,                                               00120099
//*             DCB=(LRECL=104,RECFM=FBA)                               00130099
//*                                                                     00140099
//RELATO   DD  DSN=ABS.AULAS.RELATO.PG006,                              00141099
//             DISP=(NEW,CATLG,DELETE),                                 00142099
//*            DISP=SHR,                                                00143099
//             DCB=(RECFM=FB,LRECL=105,BLKSIZE=1050),                   00144099
//             SPACE=(TRK,(3,3),RLSE),UNIT=3390                         00145099
//*                                                                     00146099
//SYSPRINT DD  SYSOUT=*                                                 00150099
//SYSOUT   DD  SYSOUT=*                                                 00160099
//SYSUDUMP DD  SYSOUT=Y                                                 00170099
//SYSABEND DD  SYSOUT=*                                                 00180099
//*                                                                     00190075
