      *                                                                         
      * RETURN CODE MANAGER SUBROUTINE DATA STRUCTURE                           
      *                                                                         
       01  RTCMAN-AREA.                                                         
         03  RTCMAN                    PIC  X(8)    VALUE 'RTCMAN  '.           
         03  RTCMAN-PARMS.                                                      
           05  RTC-REQU                PIC  X(01)   VALUE 'S'.                  
             88  RTC-REQUEST-START                  VALUE 'S'.                  
             88  RTC-REQUEST-UPDATE                 VALUE 'U'.                  
             88  RTC-REQUEST-FINISH                 VALUE 'F'.                  
             88  RTC-REQUEST-DUMP                   VALUE 'D'.                  
             88  RTC-REQUEST-CANCEL                 VALUE 'C'.                  
           05  RTC-RETN                PIC  X(01)   VALUE SPACES.               
             88  RTC-REQUEST-COMPLETED              VALUE ' '.                  
             88  RTC-PUTRC-ERROR                    VALUE 'E'.                  
             88  RTC-GETLRC-ERROR                   VALUE 'L'.                  
             88  RTC-GETMRC-ERROR                   VALUE 'M'.                  
           05  RTC-CODE                PIC S9(04)   COMP VALUE ZERO.            
