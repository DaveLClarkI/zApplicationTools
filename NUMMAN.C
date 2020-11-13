      *                                                                         
      * NUMBER MANIPULATION SUBROUTINE DATA STRUCTURE                           
      *                                                                         
       01  NUMMAN-AREAS.                                                        
         03  NUMMAN                    PIC  X(08)   VALUE 'NUMMAN'.             
         03  NUMMAN-PARMS.                                                      
           05  NUM-REQU                PIC  X(01)   VALUE 'E'.                  
               88  NUM-REQUEST-NORMAL-EDIT          VALUE 'E'.                  
               88  NUM-REQUEST-IGNORE-FORMAT        VALUE 'I'.                  
           05  NUM-RETN                PIC  X(01)   VALUE SPACES.               
               88  NUM-REQUEST-COMPLETED            VALUE ' '.                  
               88  NUM-INVALID-REQU                 VALUE 'E'.                  
               88  NUM-INVALID-TOTL                 VALUE 'T'.                  
               88  NUM-INVALID-DECM                 VALUE 'D'.                  
               88  NUM-INVALID-COMB                 VALUE 'C'.                  
               88  NUM-FORMAT-ERROR                 VALUE 'F'.                  
               88  NUM-OVERFLOW-HIGH                VALUE '<'.                  
               88  NUM-OVERFLOW-DECM                VALUE '>'.                  
           05  NUM-TOTL                PIC S9(04)   COMP   VALUE +15.           
           05  NUM-DECM                PIC S9(04)   COMP   VALUE +9.            
           05  FILLER                  PIC  X(01)   VALUE LOW-VALUE.            
           05  NUM-DATA                PIC  X(15)   VALUE SPACES.               
      *                                                                         
           05  NUM-T15-D0    REDEFINES NUM-DATA     PIC S9(15).                 
           05  NUM-T15-D1    REDEFINES NUM-DATA     PIC S9(14)V9.               
           05  NUM-T15-D2    REDEFINES NUM-DATA     PIC S9(13)V99.              
           05  NUM-T15-D3    REDEFINES NUM-DATA     PIC S9(12)V999.             
           05  NUM-T15-D4    REDEFINES NUM-DATA     PIC S9(11)V9(4).            
           05  NUM-T15-D5    REDEFINES NUM-DATA     PIC S9(10)V9(5).            
           05  NUM-T15-D6    REDEFINES NUM-DATA     PIC S9(09)V9(6).            
           05  NUM-T15-D7    REDEFINES NUM-DATA     PIC S9(08)V9(7).            
           05  NUM-T15-D8    REDEFINES NUM-DATA     PIC S9(07)V9(8).            
           05  NUM-T15-D9    REDEFINES NUM-DATA     PIC S9(06)V9(9).            
      *                                                                         
      * THE ABOVE ARE ALL YOU REALLY NEED FOR RETRIEVING OUTPUT VALUES.         
      * HOWEVER, OLDER COMPILERS REQUIRED THE FOLLOWING IN ORDER TO             
      * AVOID COMPILE-TIME WARNING MESSAGES CONCERNING TRUNCATION.              
      * THUS, YOU MAY OPTIONALLY USE THESE MORE-SPECIFIC DEFINITIONS.           
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(01).                              
               10  NUM-T14-D0          PIC S9(14).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(01).                              
               10  NUM-T14-D1          PIC S9(13)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(01).                              
               10  NUM-T14-D2          PIC S9(12)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(01).                              
               10  NUM-T14-D3          PIC S9(11)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(01).                              
               10  NUM-T14-D4          PIC S9(10)V9(4).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(01).                              
               10  NUM-T14-D5          PIC S9(09)V9(5).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(01).                              
               10  NUM-T14-D6          PIC S9(08)V9(6).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(01).                              
               10  NUM-T14-D7          PIC S9(07)V9(7).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(01).                              
               10  NUM-T14-D8          PIC S9(06)V9(8).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(01).                              
               10  NUM-T14-D9          PIC S9(05)V9(9).                         
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(02).                              
               10  NUM-T13-D0          PIC S9(13).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(02).                              
               10  NUM-T13-D1          PIC S9(12)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(02).                              
               10  NUM-T13-D2          PIC S9(11)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(02).                              
               10  NUM-T13-D3          PIC S9(10)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(02).                              
               10  NUM-T13-D4          PIC S9(09)V9(4).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(02).                              
               10  NUM-T13-D5          PIC S9(08)V9(5).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(02).                              
               10  NUM-T13-D6          PIC S9(07)V9(6).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(02).                              
               10  NUM-T13-D7          PIC S9(06)V9(7).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(02).                              
               10  NUM-T13-D8          PIC S9(05)V9(8).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(02).                              
               10  NUM-T13-D9          PIC S9(04)V9(9).                         
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(03).                              
               10  NUM-T12-D0          PIC S9(12).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(03).                              
               10  NUM-T12-D1          PIC S9(11)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(03).                              
               10  NUM-T12-D2          PIC S9(10)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(03).                              
               10  NUM-T12-D3          PIC S9(09)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(03).                              
               10  NUM-T12-D4          PIC S9(08)V9(4).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(03).                              
               10  NUM-T12-D5          PIC S9(07)V9(5).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(03).                              
               10  NUM-T12-D6          PIC S9(06)V9(6).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(03).                              
               10  NUM-T12-D7          PIC S9(05)V9(7).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(03).                              
               10  NUM-T12-D8          PIC S9(04)V9(8).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(03).                              
               10  NUM-T12-D9          PIC S9(03)V9(9).                         
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(04).                              
               10  NUM-T11-D0          PIC S9(11).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(04).                              
               10  NUM-T11-D1          PIC S9(10)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(04).                              
               10  NUM-T11-D2          PIC S9(09)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(04).                              
               10  NUM-T11-D3          PIC S9(08)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(04).                              
               10  NUM-T11-D4          PIC S9(07)V9(4).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(04).                              
               10  NUM-T11-D5          PIC S9(06)V9(5).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(04).                              
               10  NUM-T11-D6          PIC S9(05)V9(6).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(04).                              
               10  NUM-T11-D7          PIC S9(04)V9(7).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(04).                              
               10  NUM-T11-D8          PIC S9(03)V9(8).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(04).                              
               10  NUM-T11-D9          PIC S9(02)V9(9).                         
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(05).                              
               10  NUM-T10-D0          PIC S9(10).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(05).                              
               10  NUM-T10-D1          PIC S9(09)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(05).                              
               10  NUM-T10-D2          PIC S9(08)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(05).                              
               10  NUM-T10-D3          PIC S9(07)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(05).                              
               10  NUM-T10-D4          PIC S9(06)V9(4).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(05).                              
               10  NUM-T10-D5          PIC S9(05)V9(5).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(05).                              
               10  NUM-T10-D6          PIC S9(04)V9(6).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(05).                              
               10  NUM-T10-D7          PIC S9(03)V9(7).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(05).                              
               10  NUM-T10-D8          PIC S9(02)V9(8).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(05).                              
               10  NUM-T10-D9          PIC S9(01)V9(9).                         
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(06).                              
               10  NUM-T09-D0          PIC S9(09).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(06).                              
               10  NUM-T09-D1          PIC S9(08)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(06).                              
               10  NUM-T09-D2          PIC S9(07)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(06).                              
               10  NUM-T09-D3          PIC S9(06)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(06).                              
               10  NUM-T09-D4          PIC S9(05)V9(4).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(06).                              
               10  NUM-T09-D5          PIC S9(04)V9(5).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(06).                              
               10  NUM-T09-D6          PIC S9(03)V9(6).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(06).                              
               10  NUM-T09-D7          PIC S9(02)V9(7).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(06).                              
               10  NUM-T09-D8          PIC S9(01)V9(8).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(06).                              
               10  NUM-T09-D9          PIC SV9(9).                              
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(07).                              
               10  NUM-T08-D0          PIC S9(08).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(07).                              
               10  NUM-T08-D1          PIC S9(07)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(07).                              
               10  NUM-T08-D2          PIC S9(06)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(07).                              
               10  NUM-T08-D3          PIC S9(05)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(07).                              
               10  NUM-T08-D4          PIC S9(04)V9(4).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(07).                              
               10  NUM-T08-D5          PIC S9(03)V9(5).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(07).                              
               10  NUM-T08-D6          PIC S9(02)V9(6).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(07).                              
               10  NUM-T08-D7          PIC S9(01)V9(7).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(07).                              
               10  NUM-T08-D8          PIC SV9(8).                              
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(08).                              
               10  NUM-T07-D0          PIC S9(07).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(08).                              
               10  NUM-T07-D1          PIC S9(06)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(08).                              
               10  NUM-T07-D2          PIC S9(05)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(08).                              
               10  NUM-T07-D3          PIC S9(04)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(08).                              
               10  NUM-T07-D4          PIC S9(03)V9(4).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(08).                              
               10  NUM-T07-D5          PIC S9(02)V9(5).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(08).                              
               10  NUM-T07-D6          PIC S9(01)V9(6).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(08).                              
               10  NUM-T07-D7          PIC SV9(7).                              
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(09).                              
               10  NUM-T06-D0          PIC S9(06).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(09).                              
               10  NUM-T06-D1          PIC S9(05)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(09).                              
               10  NUM-T06-D2          PIC S9(04)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(09).                              
               10  NUM-T06-D3          PIC S9(03)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(09).                              
               10  NUM-T06-D4          PIC S9(02)V9(4).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(09).                              
               10  NUM-T06-D5          PIC S9(01)V9(5).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(09).                              
               10  NUM-T06-D6          PIC SV9(6).                              
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(10).                              
               10  NUM-T05-D0          PIC S9(05).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(10).                              
               10  NUM-T05-D1          PIC S9(04)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(10).                              
               10  NUM-T05-D2          PIC S9(03)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(10).                              
               10  NUM-T05-D3          PIC S9(02)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(10).                              
               10  NUM-T05-D4          PIC S9(01)V9(4).                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(10).                              
               10  NUM-T05-D5          PIC SV9(5).                              
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(11).                              
               10  NUM-T04-D0          PIC S9(04).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(11).                              
               10  NUM-T04-D1          PIC S9(03)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(11).                              
               10  NUM-T04-D2          PIC S9(02)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(11).                              
               10  NUM-T04-D3          PIC S9(01)V999.                          
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(11).                              
               10  NUM-T04-D4          PIC SV9(4).                              
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(12).                              
               10  NUM-T03-D0          PIC S9(03).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(12).                              
               10  NUM-T03-D1          PIC S9(02)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(12).                              
               10  NUM-T03-D2          PIC S9(01)V99.                           
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(12).                              
               10  NUM-T03-D3          PIC SV999.                               
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(13).                              
               10  NUM-T02-D0          PIC S9(02).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(13).                              
               10  NUM-T02-D1          PIC S9(01)V9.                            
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(13).                              
               10  NUM-T02-D2          PIC SV99.                                
      *                                                                         
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(14).                              
               10  NUM-T01-D0          PIC S9(01).                              
           05  FILLER        REDEFINES NUM-DATA.                                
               10  FILLER              PIC  X(14).                              
               10  NUM-T01-D1          PIC SV9.                                 
