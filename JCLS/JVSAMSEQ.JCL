//JVSAMSEQ JOB IBMUSERC,MSGCLASS=A,CLASS=C,NOTIFY=ALU0407               00010000
//JOBLIB   DD  DSN=FUTURE.LMODUL.COBOL,DISP=SHR                         00020000
//*                                                                     00030000
//* COPIA ARQUIVO VSAM PARA ARQUIVO SEQUENCIAL                          00040000
//*                                                                     00050000
//REPRO2   EXEC PGM=IDCAMS,REGION=0M                                    00060000
//DDIN     DD   DSN=ABS.VSAM.CADALU.CLUSTER,DISP=SHR                    00070004
//DDOUT    DD   DSN=ABS.ARQUIVOS.CADALU                                 00090004
//         DISP=(,CATLG,DELETE),                                        00100003
//         SPACE=(TRK,(3,3),RLSE),                                      00110000
//         UNIT=3390                                                    00120000
//         DCB=(RECFM=FB,LRECL=135,BLKSIZE=810)                         00130004
//SYSPRINT DD SYSOUT=*                                                  00140000
//SYSIN    DD *                                                         00150000
   REPRO INFILE(DDIN) OUTFILE(DDOUT)                                    00160000
//*                                                                     00170004
