//JEXPG003 JOB IBMUSERA,MSGCLASS=A,CLASS=A,NOTIFY=ALU0407,TIME=(0,01)   00010099
//JOBLIB   DD  DISP=SHR,DSN=FUTURE.LMODUL.COBOL                         00020099
//*                                                                     00030075
//STEP1   EXEC PGM=ABSPG003                                             00040099
//*--------------------------------------------------------*            00050099
//* EXER-03 - CRIAR CADSEL A PARTIR DO CADPECA DESPREZANDO *            00060099
//*           REGISTROS COM CODIGOS ENTRE 10001 E 19999    *            00070099
//*--------------------------------------------------------*            00080099
//*                                                                     00090099
//CADPEC   DD  DSN=FUTURE.ARQUIVOS.CADPECA,                             00100099
//             DISP=SHR,VOL=SER=PR39C7,UNIT=3390                        00110099
//*                                                                     00120084
//CADSEL   DD  DSN=ABS.AULAS.CADSEL.PG003,                              00130099
//             DISP=(NEW,CATLG,DELETE),                                 00140099
//*            DISP=SHR,                                                00150099
//             DCB=(RECFM=FB,LRECL=050,BLKSIZE=0500),                   00160099
//             SPACE=(TRK,(3,3),RLSE),UNIT=3390                         00170099
//*                                                                     00180099
//SYSPRINT DD  SYSOUT=*                                                 00190099
//SYSOUT   DD  SYSOUT=*                                                 00200099
//SYSUDUMP DD  SYSOUT=Y                                                 00210099
//SYSABEND DD  SYSOUT=*                                                 00220099
//*                                                                     00230075
