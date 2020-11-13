       01  WILDCOMP-PARMS.
      * wildcard comparison program to call
         03  WILDCOMP-CTL              PIC  X(8).
           88  WILDCOMP-PGM                         VALUE 'WILDCOMP'.
      * wildcard comparison result or error code
         03  WILDCOMP-RESULT           PIC S9(3)    PACKED-DECIMAL.
           88  WILDCOMP-STR1-LT-STR2                VALUE -1.
           88  WILDCOMP-STR1-EQ-STR2                VALUE ZERO.
           88  WILDCOMP-STR1-GT-STR2                VALUE +1.
           88  WILDCOMP-LINK-AREA-FMT               VALUE +11.
           88  WILDCOMP-WILD-ARE-SAME               VALUE +12.
           88  WILDCOMP-LEN-IN-ERROR                VALUE +13.
      * wildcard character specifications
         03  WILDCOMP-WILD.
           05  WILDCOMP-ONE            PIC  X.
             88  WILDCOMP-ONE-CHAR-QMARK            VALUE '?'.
           05  WILDCOMP-ALL            PIC  X.
             88  WILDCOMP-ALL-CHAR-ASTER            VALUE '*'.
      * comparison length for both strings
         03  WILDCOMP-LEN              PIC S9(4)    BINARY.
      * known string (which can contain wild characters)
         03  WILDCOMP-STR1             PIC  X(40).
      * unknown string to compare against
         03  WILDCOMP-STR2             PIC  X(40).
