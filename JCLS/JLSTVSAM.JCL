//JLSTVSAM JOB IBMUSERC,MSGCLASS=A,CLASS=C,NOTIFY=ALU0407,              00010007
//         MSGLEVEL=(1,1)                                               00020003
//*                                                                     00030006
//* LISTA ARQUIVO VSAM                                                  00040006
//*                                                                     00050006
//DEDEF1   EXEC PGM=IDCAMS,REGION=1M                                    00060003
//DDFILE   DD   UNIT=3390,VOL=SER=PR39D7,DISP=SHR                       00070006
//* TRANSACOES  ->  ABS1...ABS5                                         00080006
//ARQPRINT DD   DSN=ABS.VSAM.CADTST.CLUSTER,DISP=SHR                    00090008
//SYSPRINT DD   SYSOUT=*                                                00100004
//SYSIN    DD *                                                         00110003
     PRINT INFILE(ARQPRINT) CHAR                                        00120003
