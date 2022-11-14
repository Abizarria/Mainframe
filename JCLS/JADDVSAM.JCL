//JADDVSAM JOB IBMUSERC,MSGCLASS=A,CLASS=C,NOTIFY=ALU0407               00010001
//JOBLIB   DD  DSN=FUTURE.LMODUL.COBOL,DISP=SHR                         00020004
//*                                                                     00030001
//* COPIA ARQUIVO VISAM PARA OUTRO VSAM                                 00040001
//*                                                                     00050001
//REPRO1   EXEC PGM=IDCAMS,REGION=0M                                    00060001
//*                                                                     00070003
//DDIN     DD   DSN=ABS.VSAM.CADALU.CLUSTER,DISP=SHR                    00080003
//DDOUT    DD   DSN=ABS.VSAM.CADTST.CLUSTER,DISP=SHR                    00090003
//*                                                                     00100001
//SYSPRINT DD SYSOUT=*                                                  00110001
//SYSIN    DD *                                                         00120001
   REPRO INFILE(DDIN) OUTFILE(DDOUT)                                    00130001
//                                                                      00140001
