// JOB    VCATPGM   DEFINE DATASET, LOAD IT, CREATE CSD GROUP                   
/. STEP01 - DEFINE DATASET FOR VCATPGM (IDCAMS)                                 
/*                                                                              
/* SPECIFY YOUR VOLUME ID AND YOUR CATALOG DATASET NAME BELOW                   
/*                                                                              
// EXEC   PGM=IDCAMS,SIZE=AUTO                                                  
   DEFINE CLUSTER( NAME('DAPSCO.SYSTEM.FILE')                         -         
                   INDEXED                                            -         
                   FREESPACE(0 0)                                     -         
                   KEYS(20 0)                                         -         
                   RECORDSIZE(250 250)                                -         
                   NOREPLICATE                                        -         
                   REUSE                                              -         
                   SHAREOPTIONS(4 3)                                  -         
                 )                                                    -         
          DATA   ( NAME('DAPSCO.SYSTEM.FILE.DATA')                    -         
                   TRACKS(1 1)                                        -         
                   VOLUMES( volume  )                                 -         
                   CISZ(4096)                                         -         
                   SPEED                                              -         
                 )                                                    -         
          INDEX  ( NAME('DAPSCO.SYSTEM.FILE.INDEX')                   -         
                   TRACKS(1 1)                                        -         
                   VOLUMES( volume  )                                 -         
                   CISZ(2560)                                         -         
                 )                                                    -         
          CATALOG(catalog.dataset)                                              
/* EOD                                                                          
/. STEP02 - INITIALIZE DATASET FOR VCATPGM (DITTO)                              
/*                                                                              
/* SPECIFY YOUR CATALOG NAME BELOW AND ADD YOUR CATALOG ENTRIES                 
/*                                                                              
// UPSI   1                                                                     
// DLBL   DAPSYSF,'DAPSCO.SYSTEM.FILE',,VSAM,CAT=catalog                        
// EXEC   DITTO,SIZE=256K                                                       
$$DITTO CVS FILEOUT=DAPSYSF                                                     
VSMCATALOG=>IJSYSCT  VSAM.MASTER.CATALOG                                        
VSMCATALOG=>VSESPUC  VSESP.USER.CATALOG                                         
/*                                                                              
$$DITTO EOJ                                                                     
/* EOD                                                                          
/. STEP03 - CREATE CSD GROUP FOR VCATPGM (DFHCSDUP)                             
/*                                                                              
/* DEFINE THE ABOVE FILE IN YOUR FCT AND THE FOLLOWING IN YOUR CSD              
/*                                                                              
// EXEC   PGM=DFHCSDUP,SIZE=300K                                                
DEFINE MAPSET(VCATMS) GROUP(VCAT)                                               
       RESIDENT(NO) RSL(0) STATUS(ENABLED)                                      
DEFINE MAPSET(VCA2MS) GROUP(VCAT)                                               
       RESIDENT(NO) RSL(0) STATUS(ENABLED)                                      
DEFINE PROFILE(VCATALT) GROUP(VCAT)                                             
       SCRNSIZE(ALTERNATE) UCTRAN(YES) PRINTERCOMP(NO) MSGJRNL(NO)              
       MSGINTEG(NO) ONEWTE(NO) PROTECT(NO) DVSUPRT(ALL) INBFMH(NO)              
       RAQ(NO) LOGREC(NO) NEPCLASS(0)                                           
DEFINE PROFILE(VCATDEF) GROUP(VCAT)                                             
       SCRNSIZE(DEFAULT) UCTRAN(NO) PRINTERCOMP(NO) MSGJRNL(NO)                 
       MSGINTEG(NO) ONEWTE(NO) PROTECT(NO) DVSUPRT(ALL) INBFMH(NO)              
       RAQ(NO) LOGREC(NO) NEPCLASS(0) RTIMOUT(500)                              
DEFINE PROGRAM(IBM3270) GROUP(VCAT)                                             
       LANGUAGE(ASSEMBLER) RELOAD(NO) RESIDENT(NO) RSL(0)                       
       STATUS(ENABLED) EXECUTIONSET(FULLAPI)                                    
DEFINE PROGRAM(VCATPGM) GROUP(VCAT)                                             
       LANGUAGE(COBOL) RELOAD(NO) RESIDENT(NO) RSL(0) STATUS(ENABLED)           
       EXECUTIONSET(FULLAPI)                                                    
DEFINE TRANSACTION(VCAT) GROUP(VCAT)                                            
       PROGRAM(VCATPGM) TWASIZE(0) PROFILE(VCATDEF) STATUS(ENABLED)             
       PRIMEDSIZE(0) DYNAMIC(NO) TRPROF(DFHCICSS) LOCALQ(NO)                    
       PRIORITY(1) INDOUBT(BACKOUT) RESTART(NO) SPURGE(NO) TPURGE(NO)           
       DUMP(YES) TRACE(YES) EXTSEC(NO) TRANSEC(4) RSL(0) RSLC(NO)               
DEFINE TRANSACTION(VCA2) GROUP(VCAT)                                            
       PROGRAM(VCATPGM) TWASIZE(0) PROFILE(VCATALT) STATUS(ENABLED)             
       PRIMEDSIZE(0) DYNAMIC(NO) TRPROF(DFHCICSS) LOCALQ(NO)                    
       PRIORITY(1) INDOUBT(BACKOUT) RESTART(NO) SPURGE(NO) TPURGE(NO)           
       DUMP(YES) TRACE(YES) EXTSEC(NO) TRANSEC(4) RSL(0) RSLC(NO)               
/* EOD                                                                          
/& EOJ                                                                          
