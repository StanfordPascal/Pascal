program PCODE_TRANSLATOR ( PCODE , PCODE1 , PCODE2 , PCODE3 , OUTPUT ,
                           OBJCODE , LIST003 , TRACEF ) ;

//******************************************************************
//   New Pascal PCODE Translator                                    
//   started from scratch in 2021                                   
//   by Bernd Oppolzer                                              
//******************************************************************
//   to do list:                                                    
//   - check static areas (CST etc.)                                
//******************************************************************
//   ready:                                                         
//   * modify string inside add_string_str (take care of X and B    
//   * and double apostrophs)                                       
//   * Timer                                                        
//   * check with large program (Pascal 1)                          
//   * add function add_string_set                                  
//   * check size limit on string buffer                            
//   * add function add_string_str                                  
//   * print string area at end of function                         
//   * no length fields with strings (they are in LCA instruction)  
//******************************************************************



const VERSION = '2021.04' ;        // Version for display message
      PCODE_COUNT = 106 ;
      FUN_COUNT = 60 ;
      MAXSB = 100000 ;

      //**********
      // opcodes  
      //**********

      XXX_ABI = 0 ;
      XXX_ABR = 1 ;
      XXX_ADA = 2 ;
      XXX_ADI = 3 ;
      XXX_ADR = 4 ;
      XXX_AND = 5 ;
      XXX_ASE = 6 ;
      XXX_ASR = 7 ;
      XXX_BGN = 8 ;
      XXX_CHK = 9 ;
      XXX_CHR = 10 ;
      XXX_CRD = 11 ;
      XXX_CSP = 12 ;
      XXX_CST = 13 ;
      XXX_CTI = 14 ;
      XXX_CTS = 15 ;
      XXX_CUP = 16 ;
      XXX_DBG = 17 ;
      XXX_DEC = 18 ;
      XXX_DEF = 19 ;
      XXX_DFC = 20 ;
      XXX_DIF = 21 ;
      XXX_DVI = 22 ;
      XXX_DVR = 23 ;
      XXX_END = 24 ;
      XXX_ENT = 25 ;
      XXX_EQU = 26 ;
      XXX_FJP = 27 ;
      XXX_FLO = 28 ;
      XXX_FLR = 29 ;
      XXX_FLT = 30 ;
      XXX_GEQ = 31 ;
      XXX_GRT = 32 ;
      XXX_IAC = 33 ;
      XXX_INC = 34 ;
      XXX_IND = 35 ;
      XXX_INN = 36 ;
      XXX_INT = 37 ;
      XXX_IOR = 38 ;
      XXX_IXA = 39 ;
      XXX_LAB = 40 ;
      XXX_LCA = 41 ;
      XXX_LDA = 42 ;
      XXX_LDC = 43 ;
      XXX_LEQ = 44 ;
      XXX_LES = 45 ;
      XXX_LOC = 46 ;
      XXX_LOD = 47 ;
      XXX_MCC = 48 ;
      XXX_MCP = 49 ;
      XXX_MCV = 50 ;
      XXX_MFI = 51 ;
      XXX_MOD = 52 ;
      XXX_MOV = 53 ;
      XXX_MPI = 54 ;
      XXX_MPR = 55 ;
      XXX_MSE = 56 ;
      XXX_MST = 57 ;
      XXX_MV1 = 58 ;
      XXX_MZE = 59 ;
      XXX_NEQ = 60 ;
      XXX_NEW = 61 ;
      XXX_NGI = 62 ;
      XXX_NGR = 63 ;
      XXX_NOT = 64 ;
      XXX_ODD = 65 ;
      XXX_ORD = 66 ;
      XXX_PAK = 67 ;
      XXX_POP = 68 ;
      XXX_RET = 69 ;
      XXX_RND = 70 ;
      XXX_RST = 71 ;
      XXX_SAV = 72 ;
      XXX_SBA = 73 ;
      XXX_SBI = 74 ;
      XXX_SBR = 75 ;
      XXX_SCL = 76 ;
      XXX_SLD = 77 ;
      XXX_SMV = 78 ;
      XXX_SQI = 79 ;
      XXX_SQR = 80 ;
      XXX_STO = 81 ;
      XXX_STP = 82 ;
      XXX_STR = 83 ;
      XXX_TRC = 84 ;
      XXX_UJP = 85 ;
      XXX_UNI = 86 ;
      XXX_UXJ = 87 ;
      XXX_VC1 = 88 ;
      XXX_VC2 = 89 ;
      XXX_VCC = 90 ;
      XXX_VIX = 91 ;
      XXX_VLD = 92 ;
      XXX_VLM = 93 ;
      XXX_VMV = 94 ;
      XXX_VPO = 95 ;
      XXX_VPU = 96 ;
      XXX_VRP = 97 ;
      XXX_VSM = 98 ;
      XXX_VST = 99 ;
      XXX_XBG = 100 ;
      XXX_XEN = 101 ;
      XXX_XJP = 102 ;
      XXX_XLB = 103 ;
      XXX_XOR = 104 ;
      XXX_XPO = 105 ;

      //*********************
      // standard functions  
      //*********************

      CSP_CTR = 0 ;
      CSP_N01 = 1 ;
      CSP_N02 = 2 ;
      CSP_N03 = 3 ;
      CSP_N04 = 4 ;
      CSP_N05 = 5 ;
      CSP_N06 = 6 ;
      CSP_N07 = 7 ;
      CSP_N08 = 8 ;
      CSP_N09 = 9 ;
      CSP_PAG = 10 ;
      CSP_GET = 11 ;
      CSP_PUT = 12 ;
      CSP_RES = 13 ;
      CSP_REW = 14 ;
      CSP_RDC = 15 ;
      CSP_WRI = 16 ;
      CSP_WRE = 17 ;
      CSP_WRR = 18 ;
      CSP_WRC = 19 ;
      CSP_WRS = 20 ;
      CSP_WRX = 21 ;
      CSP_RDB = 22 ;
      CSP_WRB = 23 ;
      CSP_RDR = 24 ;
      CSP_RDH = 25 ;
      CSP_RDY = 26 ;
      CSP_EOL = 27 ;
      CSP_EOT = 28 ;
      CSP_RDD = 29 ;
      CSP_WRD = 30 ;
      CSP_CLK = 31 ;
      CSP_WLN = 32 ;
      CSP_RLN = 33 ;
      CSP_RDI = 34 ;
      CSP_EOF = 35 ;
      CSP_ELN = 36 ;
      CSP_RDS = 37 ;
      CSP_TRP = 38 ;
      CSP_XIT = 39 ;
      CSP_FDF = 40 ;
      CSP_SIO = 41 ;
      CSP_EIO = 42 ;
      CSP_MSG = 43 ;
      CSP_SKP = 44 ;
      CSP_LIM = 45 ;
      CSP_TRA = 46 ;
      CSP_WRP = 47 ;
      CSP_CLS = 48 ;
      CSP_DAT = 49 ;
      CSP_TIM = 50 ;
      CSP_FLR = 51 ;
      CSP_TRC = 52 ;
      CSP_RND = 53 ;
      CSP_WRV = 54 ;
      CSP_APN = 55 ;
      CSP_RDV = 56 ;
      CSP_RFC = 57 ;      // read char from file
      CSP_RFS = 58 ;      // read char array from file
      CSP_RFV = 59 ;      // read varchar from file


type POSINT = 0 .. 214748360 ;
     HALFWORD = 0 .. 32767 ;
     BYTE = 0 .. 255 ;
     OPTAB_ELEM = record
                    OPC : CHAR ( 3 ) ;     // Opcode = Key
                    OPC_NUM : BYTE ;       // Opcode numerisch
                    STATUS : 0 .. 3 ;      // State
                    OPTYPE : CHAR          // Type of Operands
                  end ;
     FUNTAB_ELEM = record
                     FUNC : CHAR ( 3 ) ;   // Funcode = Key
                     FUNC_NUM : BYTE ;     // Funcode numerisch
                     NUMBER1 : INTEGER ;   // Stack usage 1
                     NUMBER2 : INTEGER ;   // Stack usage 2
                     NUMBER3 : INTEGER ;   // Stack usage 3
                     FLAGX : CHAR          // Flag
                   end ;
     PSC = -> SC_CODE ;
     SC_CODE = record
                 NUM : INTEGER ;       // number of PCode instr
                 STATUS : 0 .. 3 ;     // 0 = incomplete
                 OP : BYTE ;           // internal Op-Code number
                 T : CHAR ;            // type field
                 T2 : CHAR ;           // 2nd type field (CUP lang)
                 P : INTEGER ;         // p = lexical level
                 Q : INTEGER ;         // q = address
                 X : INTEGER ;         // used for different codes
                 IPMST : INTEGER ;     // points to MST (from CUP)
                 LAB : CHAR ( 8 ) ;    // label or blank
                 IXOPER : INTEGER ;    // string index: operands
                 IXCOMM : INTEGER ;    // string index: comments
                 PSECT : PSC ;         // points to section
                 LOC : INTEGER ;       // LOC = line of code
                 PREV : PSC ;          // chaining
                 NEXT : PSC ;          // chaining
               end ;


var PCODEP : -> TEXT ;

    //*****************************************************
    // PCODE   = primary pcode input file                  
    // PCODE1  = first pcode include file                  
    // PCODE2  = second pcode include file                 
    // PCODE3  = third pcode include file                  
    // OBJCODE = 370 objcode output file                   
    // list003 = Datei fuer ASSEMBLER-Ausgabe              
    //*****************************************************

    PCODE_FILENO : 0 .. 3 ;
    EOF_PCODE : BOOLEAN ;
    PCODE : TEXT ;
    PCODE1 : TEXT ;
    PCODE2 : TEXT ;
    PCODE3 : TEXT ;
    PCODE_LINENO : INTEGER := 0 ;
    PCODE_LINENO1 : INTEGER := 0 ;
    PCODE_LINENO2 : INTEGER := 0 ;
    PCODE_LINENO3 : INTEGER := 0 ;
    OBJCODE : TEXT ;
    LIST003 : TEXT ;
    TRACEF : TEXT ;
    TIMER : POSINT ;
    ERRORCNT : INTEGER ;

    //*****************************************
    // optab_name - indexed by opcode number   
    //*****************************************

    OPTAB_NAME : array [ 0 .. 120 ] of CHAR ( 3 ) ;

    //*****************************************
    // allgemein                               
    //*****************************************

    I : INTEGER ;
    ACTNUM : INTEGER := 0 ;
    LOC_AKTUELL : INTEGER := 0 ;
    ENT_AKTUELL : INTEGER := - 1 ;
    STRING_BUFFER : array [ 1 .. MAXSB ] of CHAR ;
    STRING_POS : INTEGER := 1 ;

    //*****************************************
    // sc_code chain ... 0 = anker, a = active 
    //*****************************************

    SC_CODE0 : PSC := NIL ;
    SC_CODEA : PSC := NIL ;
    SC_CODEA_ALT : PSC := NIL ;
    PLOC : PSC ;
    SC_CODE_RESERVE : PSC := NIL ;
    SC_CODE_INIT : SC_CODE :=
                   ( 0 , 0 , 0 , ' ' , ' ' , 0 , 0 , 0 , 0 , ' ' , 0 ,
                     0 , NIL , 0 , NIL , NIL ) ;

    //*****************************************
    // variablen fuer parsen der eingabezeile  
    //*****************************************

    ZEILE : STRING ( 100 ) ;
    X : INTEGER ;
    LABX : CHAR ( 8 ) ;
    OPCODE : CHAR ( 3 ) ;
    OPC_NUMBER : INTEGER ;
    OPC_TYPE : CHAR ;
    OPERANDS : STRING ( 300 ) ;
    COMMENT : STRING ( 100 ) ;
    REST : STRING ( 100 ) ;
    CONTINUE1 : BOOLEAN ;
    OP2 : STRING ( 300 ) ;
    L1 : INTEGER ;

    /**********************************************************/
    /*   Verzeichnis der OpTypen                              */
    /**********************************************************/
    /*   A = nur numerische Adresse (z.B. LOC, IXA)           */
    /*   B = Level und Adresse / Kennung und Laenge           */
    /*   C = Konstante, wie bei LDC                           */
    /*   D = Typ, Adresse (wie bei DEC und INC z.B.)          */
    /*   E = fuer LCA (Adressen von Strings usw.)             */
    /*   F = Adresse und Bedingung (1,0) - fuer XEN           */
    /*   G = Adresse und Modus (1,2, ...)                     */
    /*   J = Sprungziel sichern (Operand bei FJP und UJP)     */
    /*   K = fuer DEF (Typ und Konstante)                     */
    /*   L = Label (Offset uebernehmen)                       */
    /*   R = RET                                              */
    /*   S = Typ, Level, Adresse (wie bei STR z.B.)           */
    /*   T = nur Typbuchstabe                                 */
    /*   U = CUP (call user procedure)                        */
    /*   V = Vergleich, also Typ und bei M noch Anzahl        */
    /*   W = Typ-Kennzeichen bei IOR und XOR                  */
    /*   X = Sprungziel sichern bei XJP (Case)                */
    /*   Y = Call Standard Function                           */
    /*   Z = Parameter fuer CHK                               */
    /*   0 = hat keine Operanden (auch Blank)                 */
    /*   1 = CST (sozusagen statische CSECT)                  */
    /*   2 = DFC (Definition in statischer CSECT)             */
    /*   3 = BGN (Programmheader und Startposition)           */
    /*   4 = ENT (Entry Point)                                */
    /**********************************************************/



const OPTAB : array [ 1 .. PCODE_COUNT ] of OPTAB_ELEM =
      ( ( 'ABI' , XXX_ABI , 0 , ' ' ) , //
        ( 'ABR' , XXX_ABR , 0 , ' ' ) , //
        ( 'ADA' , XXX_ADA , 0 , ' ' ) , // neu Opp 2016
        ( 'ADI' , XXX_ADI , 0 , ' ' ) , //
        ( 'ADR' , XXX_ADR , 0 , ' ' ) , //
        ( 'AND' , XXX_AND , 0 , 'W' ) , // Typ Kennz neu / Opp 2016
        ( 'ASE' , XXX_ASE , 0 , 'A' ) , // neu McGill: Add to Set
        ( 'ASR' , XXX_ASR , 0 , 'G' ) , // neu 2019: Add Set Range
        ( 'BGN' , XXX_BGN , 0 , '3' ) , //
        ( 'CHK' , XXX_CHK , 0 , 'Z' ) , //
        ( 'CHR' , XXX_CHR , 0 , ' ' ) , //
        ( 'CRD' , XXX_CRD , 0 , ' ' ) , // nicht in Stanford-Papier
        ( 'CSP' , XXX_CSP , 0 , 'Y' ) , //
        ( 'CST' , XXX_CST , 0 , '1' ) , // neu McGill: STATIC CSECT
        ( 'CTI' , XXX_CTI , 0 , ' ' ) , // nicht in Stanford-Papier
        ( 'CTS' , XXX_CTS , 0 , ' ' ) , //
        ( 'CUP' , XXX_CUP , 0 , 'U' ) , //
        ( 'DBG' , XXX_DBG , 0 , 'A' ) , // neu 2017: Debug Instrukt.
        ( 'DEC' , XXX_DEC , 0 , 'D' ) , //
        ( 'DEF' , XXX_DEF , 0 , 'K' ) , //
        ( 'DFC' , XXX_DFC , 0 , '2' ) , // neu McGill: Def Constant
        ( 'DIF' , XXX_DIF , 0 , ' ' ) , //
        ( 'DVI' , XXX_DVI , 0 , ' ' ) , //
        ( 'DVR' , XXX_DVR , 0 , ' ' ) , //
        ( 'END' , XXX_END , 0 , ' ' ) , // nicht in Stanford-Papier
        ( 'ENT' , XXX_ENT , 0 , '4' ) , //
        ( 'EQU' , XXX_EQU , 0 , 'V' ) , //
        ( 'FJP' , XXX_FJP , 0 , 'J' ) , //
        ( 'FLO' , XXX_FLO , 0 , ' ' ) , //
        ( 'FLR' , XXX_FLR , 0 , ' ' ) , //
        ( 'FLT' , XXX_FLT , 0 , ' ' ) , //
        ( 'GEQ' , XXX_GEQ , 0 , 'V' ) , //
        ( 'GRT' , XXX_GRT , 0 , 'V' ) , //
        ( 'IAC' , XXX_IAC , 0 , 'D' ) , //
        ( 'INC' , XXX_INC , 0 , 'D' ) , //
        ( 'IND' , XXX_IND , 0 , 'D' ) , //
        ( 'INN' , XXX_INN , 0 , ' ' ) , //
        ( 'INT' , XXX_INT , 0 , ' ' ) , //
        ( 'IOR' , XXX_IOR , 0 , 'W' ) , // Typ Kennz neu / Opp 2016
        ( 'IXA' , XXX_IXA , 0 , 'A' ) , //
        ( 'LAB' , XXX_LAB , 0 , 'L' ) , //
        ( 'LCA' , XXX_LCA , 0 , 'E' ) , //
        ( 'LDA' , XXX_LDA , 0 , 'B' ) , //
        ( 'LDC' , XXX_LDC , 0 , 'C' ) , //
        ( 'LEQ' , XXX_LEQ , 0 , 'V' ) , //
        ( 'LES' , XXX_LES , 0 , 'V' ) , //
        ( 'LOC' , XXX_LOC , 0 , 'M' ) , //
        ( 'LOD' , XXX_LOD , 0 , 'S' ) , //
        ( 'MCC' , XXX_MCC , 0 , 'A' ) , // neu 2018: Memcmp Instrukt.
        ( 'MCP' , XXX_MCP , 0 , ' ' ) , // neu 2017: Memcpy Instrukt.
        ( 'MCV' , XXX_MCV , 0 , ' ' ) , // neu 2018: Memcmp Instrukt.
        ( 'MFI' , XXX_MFI , 0 , 'A' ) , // neu 2017: Mem Fill fest.L.
        ( 'MOD' , XXX_MOD , 0 , ' ' ) , //
        ( 'MOV' , XXX_MOV , 0 , 'A' ) , //
        ( 'MPI' , XXX_MPI , 0 , ' ' ) , //
        ( 'MPR' , XXX_MPR , 0 , ' ' ) , //
        ( 'MSE' , XXX_MSE , 0 , 'A' ) , // neu 2017: Memset Instrukt.
        ( 'MST' , XXX_MST , 0 , '5' ) , //
        ( 'MV1' , XXX_MV1 , 0 , 'A' ) , // neu 2020: MOV Push 1 Adr
        ( 'MZE' , XXX_MZE , 0 , 'A' ) , // neu 2017: Mem Zero fest.L.
        ( 'NEQ' , XXX_NEQ , 0 , 'V' ) , //
        ( 'NEW' , XXX_NEW , 0 , 'B' ) , //
        ( 'NGI' , XXX_NGI , 0 , ' ' ) , //
        ( 'NGR' , XXX_NGR , 0 , ' ' ) , //
        ( 'NOT' , XXX_NOT , 0 , 'W' ) , // Typ Kennz neu / Opp 2016
        ( 'ODD' , XXX_ODD , 0 , ' ' ) , //
        ( 'ORD' , XXX_ORD , 0 , ' ' ) , //
        ( 'PAK' , XXX_PAK , 0 , ' ' ) , // nicht in Stanford-Papier
        ( 'POP' , XXX_POP , 0 , ' ' ) , // nicht in Stanford-Papier
        ( 'RET' , XXX_RET , 0 , 'R' ) , //
        ( 'RND' , XXX_RND , 0 , ' ' ) , // gibt's nicht mehr, ist CSP
        ( 'RST' , XXX_RST , 0 , ' ' ) , //
        ( 'SAV' , XXX_SAV , 0 , ' ' ) , //
        ( 'SBA' , XXX_SBA , 0 , ' ' ) , // neu Opp 2016
        ( 'SBI' , XXX_SBI , 0 , ' ' ) , //
        ( 'SBR' , XXX_SBR , 0 , ' ' ) , //
        ( 'SCL' , XXX_SCL , 0 , 'B' ) , // neu McGill: Set Clear
        ( 'SLD' , XXX_SLD , 0 , 'B' ) , // neu McGill: Set Load
        ( 'SMV' , XXX_SMV , 0 , 'B' ) , // neu McGill: Set Move
        ( 'SQI' , XXX_SQI , 0 , ' ' ) , //
        ( 'SQR' , XXX_SQR , 0 , ' ' ) , //
        ( 'STO' , XXX_STO , 0 , 'T' ) , //
        ( 'STP' , XXX_STP , 0 , ' ' ) , //
        ( 'STR' , XXX_STR , 0 , 'S' ) , //
        ( 'TRC' , XXX_TRC , 0 , ' ' ) , // gibt's nicht mehr, ist CSP
        ( 'UJP' , XXX_UJP , 0 , 'J' ) , //
        ( 'UNI' , XXX_UNI , 0 , ' ' ) , //
        ( 'UXJ' , XXX_UXJ , 0 , 'J' ) , // neu McGill: Long Jump
        ( 'VC1' , XXX_VC1 , 0 , ' ' ) , // varchar convert 1
        ( 'VC2' , XXX_VC2 , 0 , 'A' ) , // varchar convert 2
        ( 'VCC' , XXX_VCC , 0 , ' ' ) , // varchar concat
        ( 'VIX' , XXX_VIX , 0 , ' ' ) , // varchar index
        ( 'VLD' , XXX_VLD , 0 , 'B' ) , // varchar load
        ( 'VLM' , XXX_VLM , 0 , ' ' ) , // varchar load maxlength
        ( 'VMV' , XXX_VMV , 0 , 'A' ) , // varchar move
        ( 'VPO' , XXX_VPO , 0 , 'B' ) , // varchar pop workarea addr
        ( 'VPU' , XXX_VPU , 0 , 'B' ) , // varchar push workarea addr
        ( 'VRP' , XXX_VRP , 0 , ' ' ) , // varchar repeatstr
        ( 'VSM' , XXX_VSM , 0 , 'A' ) , // varchar set maxlength
        ( 'VST' , XXX_VST , 0 , 'B' ) , // varchar store
        ( 'XBG' , XXX_XBG , 0 , 'A' ) , // bedingte Codeseq. Anfang
        ( 'XEN' , XXX_XEN , 0 , 'F' ) , // bedingte Codeseq. Ende
        ( 'XJP' , XXX_XJP , 0 , 'X' ) , //
        ( 'XLB' , XXX_XLB , 0 , 'L' ) , // neu McGill: Long Jump Targ
        ( 'XOR' , XXX_XOR , 0 , 'W' ) , // neu Opp 2017
        ( 'XPO' , XXX_XPO , 0 , ' ' )   // nicht in Stanford-Papier
        ) ;

      /**********************************************************/
      /*                                                        */
      /*   CSP-Table / Stand 2020                               */
      /*                                                        */
      /**********************************************************/

      FUNTAB : array [ 1 .. FUN_COUNT ] of FUNTAB_ELEM =
      ( ( 'APN' , CSP_APN , 1 , 0 , 0 , ' ' ) ,        //
        ( 'CLK' , CSP_CLK , 1 , 0 , 0 , ' ' ) ,        //
        ( 'CLS' , CSP_CLS , 1 , 0 , 0 , ' ' ) ,        //
        ( 'CTR' , CSP_CTR , 0 , 0 , 0 , ' ' ) ,        //
        ( 'DAT' , CSP_DAT , 0 , 0 , 0 , ' ' ) ,        //
        ( 'EIO' , CSP_EIO , 1 , 1 , 0 , ' ' ) ,        //
        ( 'ELN' , CSP_ELN , 1 , - 1 , 0 , ' ' ) ,      //
        ( 'EOF' , CSP_EOF , 1 , - 1 , 0 , ' ' ) ,      //
        ( 'EOL' , CSP_EOL , 1 , - 1 , 0 , ' ' ) ,      //
        ( 'EOT' , CSP_EOT , 1 , - 1 , 0 , ' ' ) ,      //
        ( 'FDF' , CSP_FDF , 3 , 2 , 0 , ' ' ) ,        //
        ( 'FLR' , CSP_FLR , - 2 , 0 , 0 , ' ' ) ,      //
        ( 'GET' , CSP_GET , 1 , 0 , 0 , ' ' ) ,        //
        ( 'LIM' , CSP_LIM , 0 , 0 , 0 , ' ' ) ,        //
        ( 'MSG' , CSP_MSG , 0 , 0 , 0 , ' ' ) ,        //
        ( 'N01' , CSP_N01 , 0 , 0 , 0 , ' ' ) ,        //
        ( 'N02' , CSP_N02 , 0 , 0 , 0 , ' ' ) ,        //
        ( 'N03' , CSP_N03 , 0 , 0 , 0 , ' ' ) ,        //
        ( 'N04' , CSP_N04 , 0 , 0 , 0 , ' ' ) ,        //
        ( 'N05' , CSP_N05 , 0 , 0 , 0 , ' ' ) ,        //
        ( 'N06' , CSP_N06 , 0 , 0 , 0 , ' ' ) ,        //
        ( 'N07' , CSP_N07 , 0 , 0 , 0 , ' ' ) ,        //
        ( 'N08' , CSP_N08 , 0 , 0 , 0 , ' ' ) ,        //
        ( 'N09' , CSP_N09 , 0 , 0 , 0 , ' ' ) ,        //
        ( 'PAG' , CSP_PAG , 0 , 0 , 0 , ' ' ) ,        //
        ( 'PUT' , CSP_PUT , 1 , 0 , 0 , ' ' ) ,        //
        ( 'RDB' , CSP_RDB , 2 , 1 , 0 , ' ' ) ,        //
        ( 'RDC' , CSP_RDC , 3 , 2 , 0 , ' ' ) ,        //
        ( 'RDD' , CSP_RDD , 3 , 2 , 0 , ' ' ) ,        //
        ( 'RDH' , CSP_RDH , 2 , 1 , 0 , ' ' ) ,        //
        ( 'RDI' , CSP_RDI , 2 , 1 , 0 , ' ' ) ,        //
        ( 'RDR' , CSP_RDR , 2 , 1 , 0 , ' ' ) ,        //
        ( 'RDS' , CSP_RDS , 3 , 2 , 0 , ' ' ) ,        //
        ( 'RDV' , CSP_RDV , 3 , 2 , 0 , ' ' ) ,        //
        ( 'RDY' , CSP_RDY , 2 , 1 , 0 , ' ' ) ,        //
        ( 'RES' , CSP_RES , 1 , 0 , 0 , ' ' ) ,        //
        ( 'REW' , CSP_REW , 1 , 0 , 0 , ' ' ) ,        //
        ( 'RFC' , CSP_RFC , 3 , 0 , 0 , ' ' ) ,        //
        ( 'RFS' , CSP_RFS , 4 , 3 , 0 , ' ' ) ,        //
        ( 'RFV' , CSP_RFV , 4 , 3 , 0 , ' ' ) ,        //
        ( 'RLN' , CSP_RLN , 1 , 0 , 0 , ' ' ) ,        //
        ( 'RND' , CSP_RND , - 2 , 0 , 0 , ' ' ) ,      //
        ( 'SIO' , CSP_SIO , 1 , 0 , 0 , ' ' ) ,        //
        ( 'SKP' , CSP_SKP , 0 , 0 , 0 , ' ' ) ,        //
        ( 'TIM' , CSP_TIM , 0 , 0 , 0 , ' ' ) ,        //
        ( 'TRA' , CSP_TRA , 0 , 0 , 0 , ' ' ) ,        //
        ( 'TRC' , CSP_TRC , - 2 , 0 , 0 , ' ' ) ,      //
        ( 'TRP' , CSP_TRP , 0 , 0 , 0 , ' ' ) ,        //
        ( 'WLN' , CSP_WLN , 1 , 0 , 0 , ' ' ) ,        //
        ( 'WRB' , CSP_WRB , 3 , 2 , 0 , ' ' ) ,        //
        ( 'WRC' , CSP_WRC , 3 , 2 , 0 , ' ' ) ,        //
        ( 'WRD' , CSP_WRD , 3 , 2 , 0 , ' ' ) ,        //
        ( 'WRE' , CSP_WRE , - 3 , 3 , 0 , ' ' ) ,      //
        ( 'WRI' , CSP_WRI , 3 , 2 , 0 , ' ' ) ,        //
        ( 'WRP' , CSP_WRP , 3 , 2 , 0 , ' ' ) ,        //
        ( 'WRR' , CSP_WRR , - 1 , 4 , 0 , ' ' ) ,      //
        ( 'WRS' , CSP_WRS , 4 , 3 , 0 , ' ' ) ,        //
        ( 'WRV' , CSP_WRV , 3 , 2 , 0 , ' ' ) ,        //
        ( 'WRX' , CSP_WRX , 4 , 3 , 0 , ' ' ) ,        //
        ( 'XIT' , CSP_XIT , 1 , 0 , 0 , ' ' )          //
        ) ;



procedure ERROR_SYMB ( ERRCODE : INTEGER ; ERR_SYMBOL : CHAR ( 8 ) ) ;

   var ERRLINE : INTEGER ;

   begin (* ERROR_SYMB *)
     ERRORCNT := ERRORCNT + 1 ;
     case PCODE_FILENO of
       1 : ERRLINE := PCODE_LINENO1 ;
       2 : ERRLINE := PCODE_LINENO2 ;
       3 : ERRLINE := PCODE_LINENO3 ;
       otherwise
         ERRLINE := PCODE_LINENO
     end (* case *) ;
     WRITELN ( ' ' : 4 , '+++ Error in line ' , ERRLINE : 1 ,
               ' in file PCODE' , PCODE_FILENO : 1 ) ;
     WRITE ( ' ' : 8 ) ;
     case ERRCODE of
       99 : WRITELN ( 'not yet implememted' ) ;
       101 : WRITELN ( 'PCODE instruction unknown' ) ;
       102 : WRITELN ( 'CSP function name unknown' ) ;
       103 : WRITELN ( 'PCODE operand type unknown' ) ;
       104 : WRITELN ( 'String buffer overrun (compiler limit)' ) ;
       105 : WRITELN ( 'Length of String const > 100 '
                       '(compiler limit)' ) ;
       106 : WRITELN ( 'No ENT found before LAB/XLB' ) ;
       107 : WRITELN ( 'Tag byte for String const not implemented' ) ;
       301 : WRITELN ( 'Unknown PCODE Macro Instruction' ) ;
       302 : WRITELN ( 'Invalid PCODE Include File Name' ) ;
       303 : WRITELN ( 'Invalid PCODE Include File Number' ) ;
       304 : WRITELN ( 'Unexpected EOF on PCODE File' ) ;
       otherwise
         WRITELN ( 'Unknown Error Code ' , ERRCODE : 1 ) ;
     end (* case *) ;
     if ERR_SYMBOL <> ' ' then
       WRITELN ( ' ' : 8 , 'Symbol in error = ' , ERR_SYMBOL ) ;
   end (* ERROR_SYMB *) ;



procedure ERROR ( ERRCODE : INTEGER ) ;

   begin (* ERROR *)
     ERROR_SYMB ( ERRCODE , '   ' ) ;
   end (* ERROR *) ;



function ALLOC_SC_CODE : PSC ;

   var P : PSC ;

   begin (* ALLOC_SC_CODE *)
     if SC_CODE_RESERVE = NIL then
       NEW ( P )
     else
       begin
         P := SC_CODE_RESERVE ;
         SC_CODE_RESERVE := SC_CODE_RESERVE -> . NEXT ;
         P -> . NEXT := NIL
       end (* else *) ;
     ALLOC_SC_CODE := P
   end (* ALLOC_SC_CODE *) ;





//****************************************
// procedure FREE_SC_CODE ( P : PSC ) ;   
//                                        
//    begin (* FREE_SC_CODE *)            
//      P -> . NEXT := SC_CODE_RESERVE ;  
//      SC_CODE_RESERVE := P ;            
//    end (* FREE_SC_CODE *) ;            
//****************************************




procedure FREE_ALL_SC_CODES ( P : PSC ) ;

   var PSTART : PSC ;

   begin (* FREE_ALL_SC_CODES *)
     PSTART := P ;
     while P -> . NEXT <> NIL do
       P := P -> . NEXT ;
     P -> . NEXT := SC_CODE_RESERVE ;
     SC_CODE_RESERVE := PSTART ;
   end (* FREE_ALL_SC_CODES *) ;



function HEXVAL ( C : CHAR ) : INTEGER ;

   begin (* HEXVAL *)
     if C in [ '0' .. '9' ] then
       HEXVAL := ORD ( C ) - ORD ( '0' )
     else
       if C in [ 'a' .. 'f' ] then
         HEXVAL := ORD ( C ) - ORD ( 'a' ) + 10
       else
         if C in [ 'A' .. 'F' ] then
           HEXVAL := ORD ( C ) - ORD ( 'A' ) + 10
         else
           HEXVAL := 0
   end (* HEXVAL *) ;



function ADD_STRING_STR ( STRING_TAG : CHAR ; const S : STRING ; LEN :
                        INTEGER ) : INTEGER ;

   var L : HALFWORD ;
       CP : -> CHAR ;
       PCA : -> CHAR ;
       CA : STRING ( 1000 ) ;
       IX : INTEGER ;
       IY : INTEGER ;
       IX2 : INTEGER ;
       LCA : INTEGER ;
       Y : CHAR ( 512 ) ;
       H1 , H2 : CHAR ;
       HW : INTEGER ;

   begin (* ADD_STRING_STR *)
     L := LENGTH ( S ) ;
     if L > 1000 then
       begin
         ERROR ( 105 ) ;
         CA := ''
       end (* then *)
     else
       if STRING_TAG = 'X' then
         begin

     //************************************************
     // convert hex string to internal representation  
     //************************************************

           CA := S ;
           IX := 1 ;
           IY := 0 ;
           while IX < LENGTH ( CA ) do
             begin
               H1 := CA [ IX ] ;
               IX := IX + 1 ;
               H2 := CA [ IX ] ;
               IX := IX + 1 ;
               HW := HEXVAL ( H1 ) * 16 + HEXVAL ( H2 ) ;
               IY := IY + 1 ;
               Y [ IY ] := CHR ( HW ) ;
             end (* while *) ;
           L := IY ;
           PCA := ADDR ( Y ) ;
         end (* then *)
       else
         begin

     //***************************************
     // take care of double apostrophes       
     //***************************************

           CA := S ;
           IX := INDEX ( CA , '''' ) ;
           LCA := LENGTH ( CA ) ;
           if IX > 0 then
             begin
               IX2 := IX ;
               while IX < LCA do
                 begin
                   if CA [ IX ] = '''' then
                     if IX + 1 <= LCA then
                       if CA [ IX + 1 ] = '''' then
                         begin
                           IX := IX + 1 ;
                           L := L - 1
                         end (* then *) ;
                   if IX2 < IX then
                     CA [ IX2 ] := CA [ IX ] ;
                   IX := IX + 1 ;
                   IX2 := IX2 + 1 ;
                 end (* while *)
             end (* then *) ;
           PCA := PTRADD ( ADDR ( CA ) , 4 ) ;
         end (* else *) ;

     //**********************************************
     // l is known - maybe modified                  
     // pca is now set (source address)              
     // check for enough space in buffer             
     // init target area in string buffer to blanks  
     // copy string from pca in length L             
     // return original string_pos as function result
     // increment string_pos                         
     //**********************************************

     ADD_STRING_STR := STRING_POS ;
     if STRING_POS + LEN >= MAXSB then
       ERROR ( 104 )
     else
       begin
         CP := ADDR ( STRING_BUFFER [ STRING_POS ] ) ;
         MEMSET ( CP , ' ' , LEN ) ;
         MEMCPY ( CP , PCA , L ) ;
         STRING_POS := STRING_POS + LEN ;
       end (* else *)
   end (* ADD_STRING_STR *) ;



function ADD_STRING_SET ( const S : STRING ; var RLEN : INTEGER ) :
                        INTEGER ;

//***********************************************
// add set representation to string buffer       
// return position as function result            
// and length of set in variable rlen            
//***********************************************


   var DUMMY : CHAR ;
       AD : INTEGER ;
       X : set of CHAR ;
       Y : CHAR ( 256 ) ;
       S_INTERN : STRING ( 300 ) ;
       IX : INTEGER ;
       IY : INTEGER ;
       H1 , H2 : CHAR ;
       HW : INTEGER ;
       TAG : CHAR ;
       L : INTEGER ;
       CP : -> CHAR ;

   begin (* ADD_STRING_SET *)
     TAG := S [ 1 ] ;
     case TAG of
       'E' : begin
               RLEN := 0 ;
               AD := 0 ;
             end (* tag/ca *) ;
       'C' : begin
               READSTR ( S , DUMMY , RLEN ) ;
               X := [ ] ;
               IX := INDEX ( S , '''' ) ;
               L := LENGTH ( S ) - IX - 1 ;
               S_INTERN := SUBSTR ( S , IX + 1 , L ) ;
               for IX := 1 to LENGTH ( S_INTERN ) do
                 X := X + [ S_INTERN [ IX ] ] ;
               AD := STRING_POS ;
               if STRING_POS + RLEN >= MAXSB then
                 ERROR ( 104 )
               else
                 begin
                   CP := ADDR ( STRING_BUFFER [ STRING_POS ] ) ;
                   MEMCPY ( CP , ADDR ( X ) , RLEN ) ;
                   STRING_POS := STRING_POS + RLEN ;
                 end (* else *)
             end (* tag/ca *) ;
       otherwise
         begin
           READSTR ( S , DUMMY , RLEN ) ;
           MEMSET ( ADDR ( Y ) , x'00' , 256 ) ;
           IX := INDEX ( S , '''' ) ;
           L := LENGTH ( S ) - IX - 1 ;
           S_INTERN := SUBSTR ( S , IX + 1 , L ) ;
           IX := 1 ;
           IY := 0 ;
           while IX < LENGTH ( S_INTERN ) do
             begin
               H1 := S_INTERN [ IX ] ;
               IX := IX + 1 ;
               H2 := S_INTERN [ IX ] ;
               IX := IX + 1 ;
               HW := HEXVAL ( H1 ) * 16 + HEXVAL ( H2 ) ;
               IY := IY + 1 ;
               Y [ IY ] := CHR ( HW ) ;
             end (* while *) ;
           AD := STRING_POS ;
           if STRING_POS + RLEN >= MAXSB then
             ERROR ( 104 )
           else
             begin
               CP := ADDR ( STRING_BUFFER [ STRING_POS ] ) ;
               MEMCPY ( CP , ADDR ( Y ) , RLEN ) ;
               STRING_POS := STRING_POS + RLEN ;
             end (* else *)
         end (* otherw *)
     end (* case *) ;
     ADD_STRING_SET := AD ;
   end (* ADD_STRING_SET *) ;



function ADD_STRING_REAL ( const S : STRING ) : INTEGER ;

//***********************************************
// add string to string_buffer; return position  
//***********************************************


   var L : HALFWORD ;
       CP : -> CHAR ;
       CA : STRING ( 1000 ) ;

   begin (* ADD_STRING_REAL *)
     L := LENGTH ( S ) ;
     if L > 1000 then
       begin
         ERROR ( 105 ) ;
         CA := ''
       end (* then *)
     else
       CA := S ;
     ADD_STRING_REAL := STRING_POS + 2 ;
     if STRING_POS + L + 4 >= MAXSB then
       ERROR ( 104 )
     else
       begin
         CP := ADDR ( STRING_BUFFER [ STRING_POS ] ) ;
         MEMCPY ( CP , ADDR ( CA ) , L + 4 ) ;
         STRING_POS := STRING_POS + L + 4 ;
       end (* else *)
   end (* ADD_STRING_REAL *) ;



function READNXTINST ( var PCODEF : TEXT ; var ZEILE : STRING ) :
                     BOOLEAN ;

   begin (* READNXTINST *)
     if EOF ( PCODEF ) then
       begin
         ZEILE := ' ' ;
         READNXTINST := TRUE
       end (* then *)
     else
       begin
         READLN ( PCODEF , ZEILE ) ;
         READNXTINST := FALSE ;
         case PCODE_FILENO of
           1 : PCODE_LINENO1 := PCODE_LINENO1 + 1 ;
           2 : PCODE_LINENO2 := PCODE_LINENO2 + 1 ;
           3 : PCODE_LINENO3 := PCODE_LINENO3 + 1 ;
           otherwise
             PCODE_LINENO := PCODE_LINENO + 1
         end (* case *)
       end (* else *)
   end (* READNXTINST *) ;



procedure CHK_INCLUDE ;

   var INCLUDECMD : CHAR ( 100 ) ;
       INCLFILE : CHAR ( 8 ) ;

   begin (* CHK_INCLUDE *)

     //**************************************************
     // check for %INCLUDE                               
     // if so, change input file from PCODE to PCODEx    
     //**************************************************

     if PCODE_FILENO = 0 then
       begin
         if PCODE -> = '%' then
           begin
             READLN ( PCODE , INCLUDECMD ) ;
             PCODE_LINENO := PCODE_LINENO + 1 ;
             if LEFT ( INCLUDECMD , 9 ) <> '%INCLUDE ' then
               ERROR ( 301 )
             else
               begin
                 INCLFILE := SUBSTR ( INCLUDECMD , 10 , 8 ) ;
                 if LEFT ( INCLFILE , 5 ) <> 'pcode' then
                   ERROR ( 302 ) ;
                 case INCLFILE [ 6 ] of
                   '1' : begin
                           RESET ( PCODE1 ) ;
                           PCODE_FILENO := 1 ;
                           PCODEP := ADDR ( PCODE1 ) ;
                         end (* tag/ca *) ;
                   '2' : begin
                           RESET ( PCODE2 ) ;
                           PCODE_FILENO := 2 ;
                           PCODEP := ADDR ( PCODE2 ) ;
                         end (* tag/ca *) ;
                   '3' : begin
                           RESET ( PCODE3 ) ;
                           PCODE_FILENO := 3 ;
                           PCODEP := ADDR ( PCODE3 ) ;
                         end (* tag/ca *) ;
                   otherwise
                     ERROR ( 303 )
                 end (* case *)
               end (* else *)
           end (* then *)
       end (* then *) ;
   end (* CHK_INCLUDE *) ;



function POS_COMMENT ( const S : STRING ) : INTEGER ;

   begin (* POS_COMMENT *)
     X := LENGTH ( S ) ;
     while X > 0 do
       begin
         if S [ X ] = ';' then
           break ;
         if S [ X ] = '''' then
           begin
             X := 0 ;
             break
           end (* then *) ;
         X := X - 1
       end (* while *) ;
     POS_COMMENT := X
   end (* POS_COMMENT *) ;



procedure CHECK_OPCODE ( OPCODE : CHAR ( 3 ) ; var OPC_NUMBER : INTEGER
                       ; var OPC_TYPE : CHAR ) ;

//*******************************************
// binary search for opcode in opcode table  
// return opc number and operand type        
//*******************************************


   var IMIN , IMAX , IMID : INTEGER ;

   begin (* CHECK_OPCODE *)
     IMAX := PCODE_COUNT ;
     IMIN := 1 ;
     OPC_NUMBER := - 1 ;
     OPC_TYPE := ' ' ;
     while IMAX >= IMIN do
       begin
         IMID := ( IMAX + IMIN ) DIV 2 ;
         if OPTAB [ IMID ] . OPC < OPCODE then
           IMIN := IMID + 1
         else
           if OPTAB [ IMID ] . OPC > OPCODE then
             IMAX := IMID - 1
           else
             begin
               OPC_NUMBER := OPTAB [ IMID ] . OPC_NUM ;
               OPC_TYPE := OPTAB [ IMID ] . OPTYPE ;
               break
             end (* else *)
       end (* while *) ;
   end (* CHECK_OPCODE *) ;



procedure CHECK_FUNC ( FUNAME : CHAR ( 3 ) ; var FUNC_NUMBER : INTEGER
                     ) ;

//*******************************************
// binary search for funame in func table    
// return func number                        
//*******************************************


   var IMIN , IMAX , IMID : INTEGER ;

   begin (* CHECK_FUNC *)
     IMAX := FUN_COUNT ;
     IMIN := 1 ;
     FUNC_NUMBER := - 1 ;
     while IMAX >= IMIN do
       begin
         IMID := ( IMAX + IMIN ) DIV 2 ;
         if FUNTAB [ IMID ] . FUNC < FUNAME then
           IMIN := IMID + 1
         else
           if FUNTAB [ IMID ] . FUNC > FUNAME then
             IMAX := IMID - 1
           else
             begin
               FUNC_NUMBER := FUNTAB [ IMID ] . FUNC_NUM ;
               break
             end (* else *)
       end (* while *) ;
   end (* CHECK_FUNC *) ;



procedure WORK_OPERANDS ( var SCC : SC_CODE ; OPTYPE : CHAR ; const
                        OPERANDS : STRING ) ;

/**********************************************************/
/*   A = nur numerische Adresse (z.B. LOC, IXA)           */
/*   B = Level und Adresse / Kennung und Laenge           */
/*   C = Konstante, wie bei LDC                           */
/*   D = Typ, Adresse (wie bei DEC und INC z.B.)          */
/*   E = fuer LCA (Adressen von Strings usw.)             */
/*   F = Adresse und Bedingung (1,0) - fuer XEN           */
/*   G = Adresse und Modus (1,2, ...)                     */
/*   J = Sprungziel sichern (Operand bei FJP und UJP)     */
/*   K = fuer DEF (Typ und Konstante)                     */
/*   L = Label (Offset uebernehmen)                       */
/*   R = RET                                              */
/*   S = Typ, Level, Adresse (wie bei STR z.B.)           */
/*   T = nur Typbuchstabe                                 */
/*   U = CUP (call user procedure)                        */
/*   V = Vergleich, also Typ und bei M noch Anzahl        */
/*   W = Typ-Kennzeichen bei IOR und XOR                  */
/*   X = Sprungziel sichern bei XJP (Case)                */
/*   Y = Call Standard Function                           */
/*   Z = Parameter fuer CHK                               */
/*   0 = hat keine Operanden (auch Blank)                 */
/*   1 = CST (sozusagen statische CSECT)                  */
/*   2 = DFC (Definition in statischer CSECT)             */
/*   3 = BGN (Programmheader und Startposition)           */
/*   4 = ENT (Entry Point)                                */
/**********************************************************/


   var KOMMA : CHAR ;
       DUMMY : CHAR ;
       HOCHKOMMA : CHAR ;
       C : CHAR ;
       FUNNAME : CHAR ( 3 ) ;
       IX : INTEGER ;
       OPW : STRING ( 300 ) ;
       RLEN : INTEGER ;
       STAG : CHAR ;

   begin (* WORK_OPERANDS *)
     case OPTYPE of
       'A' : begin
               READSTR ( OPERANDS , SCC . Q ) ;
             end (* tag/ca *) ;
       'B' : begin
               READSTR ( OPERANDS , SCC . P , KOMMA , SCC . Q ) ;
             end (* tag/ca *) ;
       'C' : begin
               SCC . T := OPERANDS [ 1 ] ;
               case SCC . T of
                 'N' : SCC . Q := 0 ;
                 'C' : begin
                         READSTR ( OPERANDS , DUMMY , KOMMA , HOCHKOMMA
                                   , C ) ;
                         SCC . Q := ORD ( C )
                       end (* tag/ca *) ;
                 'R' : begin
                         OPW := SUBSTR ( OPERANDS , 3 ) ;
                         IX := ADD_STRING_REAL ( OPW ) ;
                         SCC . Q := IX ;
                       end (* tag/ca *) ;
                 otherwise
                   READSTR ( OPERANDS , DUMMY , KOMMA , SCC . Q )
               end (* case *)
             end (* tag/ca *) ;
       'D' : begin
               READSTR ( OPERANDS , SCC . T , KOMMA , SCC . Q ) ;
             end (* tag/ca *) ;
       'E' : begin

     //***********************************************************
     // fuer lca                                                  
     //***********************************************************
     // lca m,size,string                                         
     // lca s,set (als c32'set' oder xnn'set')                    
     // lca p,lab8 (lab8 = name der statischen csect)             
     //***********************************************************
     // q = string adresse                                        
     // x = string laenge                                         
     // lca m und lca s haben evtl. fortsetzung auf neuer zeile   
     //***********************************************************

               SCC . T := OPERANDS [ 1 ] ;
               case SCC . T of
                 'M' : begin
                         READSTR ( OPERANDS , DUMMY , KOMMA , SCC . X )
                                   ;
                         OPW := SUBSTR ( OPERANDS , 3 ) ;
                         IX := INDEX ( OPW , ',' ) ;
                         STAG := OPW [ IX + 1 ] ;
                         case STAG of
                           '''' : begin
                                    OPW := SUBSTR ( OPW , IX + 2 ) ;
                                    IX := ADD_STRING_STR ( ' ' , OPW ,
                                          SCC . X ) ;
                                  end (* tag/ca *) ;
                           'X' : begin
                                   OPW := SUBSTR ( OPW , IX + 3 ) ;
                                   IX := ADD_STRING_STR ( 'X' , OPW ,
                                         SCC . X ) ;
                                 end (* tag/ca *) ;
                           otherwise
                             begin
                               IX := 0 ;
                               ERROR ( 107 )
                             end (* otherw *)
                         end (* case *) ;
                         SCC . Q := IX ;
                       end (* tag/ca *) ;
                 'S' : begin
                         OPW := SUBSTR ( OPERANDS , 3 ) ;
                         IX := ADD_STRING_SET ( OPW , RLEN ) ;
                         SCC . Q := IX ;
                         SCC . X := RLEN ;
                       end (* tag/ca *) ;
                 'P' : begin
                         SCC . LAB := SUBSTR ( OPERANDS , 3 ) ;
                       end (* tag/ca *) ;
                 otherwise
                   
               end (* case *)
             end (* tag/ca *) ;
       'F' , 'G' :
         begin
           READSTR ( OPERANDS , SCC . Q , KOMMA , SCC . P ) ;
         end (* tag/ca *) ;
       'J' : begin

     //************************************
     // genauer nachschauen, wie das geht  
     //************************************

               
             end (* tag/ca *) ;
       'K' : begin
               SCC . T := OPERANDS [ 1 ] ;
               if SCC . T = 'C' then
                 begin
                   READSTR ( OPERANDS , DUMMY , KOMMA , HOCHKOMMA , C )
                             ;
                   SCC . Q := ORD ( C )
                 end (* then *)
               else
                 READSTR ( OPERANDS , DUMMY , KOMMA , SCC . Q )
             end (* tag/ca *) ;
       'L' : begin
               SCC . Q := SCC . NUM ;
               SCC . X := ENT_AKTUELL ;
               if SCC . X < 0 then
                 ERROR ( 106 ) ;

     //*************************************************
     // hier noch nachtragen: scc.p = Level des Labels  
     // aus Entry                                       
     //*************************************************

             end (* tag/ca *) ;
       'M' : begin
               READSTR ( OPERANDS , SCC . Q ) ;
               LOC_AKTUELL := SCC . Q ;
             end (* tag/ca *) ;
       'R' : begin
               READSTR ( OPERANDS , SCC . T ) ;
             end (* tag/ca *) ;
       'S' : begin
               READSTR ( OPERANDS , SCC . T , KOMMA , SCC . P , KOMMA ,
                         SCC . Q ) ;
             end (* tag/ca *) ;
       'T' : begin
               READSTR ( OPERANDS , SCC . T ) ;
             end (* tag/ca *) ;
       'U' : begin

     //************************************
     // genauer nachschauen, wie das geht  
     //************************************

               
             end (* tag/ca *) ;
       'V' : begin

     //****************************************************
     // evtl. ist nur T oder nur ein Teil vorhanden,       
     // dann wird durch READSTR der Rest auf Null gesetzt  
     //****************************************************

               READSTR ( OPERANDS , SCC . T , KOMMA , SCC . Q , KOMMA ,
                         SCC . P ) ;
               if SCC . P = 0 then
                 SCC . P := SCC . Q ;
             end (* tag/ca *) ;
       'W' : begin

     //************************************
     // genauer nachschauen, wie das geht  
     //************************************

               
             end (* tag/ca *) ;
       'X' : begin

     //************************************
     // genauer nachschauen, wie das geht  
     //************************************

               
             end (* tag/ca *) ;
       'Y' : begin
               READSTR ( OPERANDS , FUNNAME , KOMMA , SCC . P ) ;
               IX := 1 ;
               CHECK_FUNC ( FUNNAME , SCC . Q ) ;
               if SCC . Q < 0 then
                 ERROR_SYMB ( 102 , FUNNAME ) ;
             end (* tag/ca *) ;
       'Z' : begin
               SCC . T := OPERANDS [ 1 ] ;
               repeat
                 if LENGTH ( OPERANDS ) < 3 then
                   break ;
                 OPW := SUBSTR ( OPERANDS , 3 ) ;
                 if OPW [ 1 ] = '''' then
                   begin
                     SCC . P := ORD ( OPW [ 2 ] ) ;
                     if LENGTH ( OPW ) < 4 then
                       break ;
                     OPW := SUBSTR ( OPW , 4 ) ;
                   end (* then *)
                 else
                   begin
                     READSTR ( OPW , SCC . P ) ;
                     IX := INDEX ( OPW , ',' ) ;
                     if IX = 0 then
                       break ;
                     OPW := SUBSTR ( OPW , IX + 1 ) ;
                   end (* else *) ;
                 if OPW [ 1 ] = '''' then
                   begin
                     SCC . Q := ORD ( OPW [ 2 ] ) ;
                   end (* then *)
                 else
                   begin
                     READSTR ( OPW , SCC . Q ) ;
                   end (* else *) ;
               until TRUE ;
             end (* tag/ca *) ;
       '1' : begin

     //***************
     // speziell CST  
     //***************

               
             end (* tag/ca *) ;
       '2' : begin

     //***************
     // speziell DFC  
     //***************

               
             end (* tag/ca *) ;
       '3' : begin

     //***************
     // speziell BGN  
     //***************

               
             end (* tag/ca *) ;
       '4' : begin

     //***************
     // speziell ENT  
     //***************

               ENT_AKTUELL := SCC . Q ;
             end (* tag/ca *) ;
       '5' : begin

     //***************************
     // nachschauen, was das ist  
     //***************************

               
             end (* tag/ca *) ;
       '0' , ' ' :
         ;
       otherwise
         ERROR_SYMB ( 103 , OPTYPE ) ;
     end (* case *) ;
     SCC . LOC := LOC_AKTUELL ;
   end (* WORK_OPERANDS *) ;



procedure HEAD2 ;

   begin (* HEAD2 *)
     WRITELN ( LIST003 , 'PCODE-Assembler-Listing' ) ;
     WRITELN ( LIST003 ) ;
     WRITELN ( LIST003 ) ;
     WRITELN ( LIST003 ,
     'Offset  OpN T AddrP     AddrQ     AddrX   Label    Opc Operands'
               ) ;
     WRITELN ( LIST003 ) ;
   end (* HEAD2 *) ;



procedure LIST_PCODE ( SC : SC_CODE ; OPC : CHAR ( 3 ) ; OPC_TYPE :
                     CHAR ; const OPERANDS : STRING ) ;

   begin (* LIST_PCODE *)
     with SC do
       WRITELN ( LIST003 , NUM : - 6 , ': ' , OP : - 3 , ' ' , T , P :
                 6 , Q : 10 , X : 10 , ' ' : 3 , LAB , ' ' , OPC , ' '
                 , '(' , OPC_TYPE , ')' , ' ' , OPERANDS ) ;
   end (* LIST_PCODE *) ;



procedure SHOW_STRING_BUFFER ( var F : TEXT ) ;

   const HEX : array [ 0 .. 15 ] of CHAR = '0123456789abcdef' ;

   var CH : CHAR ;
       C1 : CHAR ;
       C2 : CHAR ;
       START , ENDE : INTEGER ;
       I : INTEGER ;


   procedure DUMPCHAR ( var F : TEXT ; CH : CHAR ) ;

      begin (* DUMPCHAR *)
        if CH in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' , 'A' .. 'I' ,
        'J' .. 'R' , 'S' .. 'Z' , '0' .. '9' , ' ' , ',' , '.' , '-' ,
        ';' , ':' , '_' , '!' , '"' , '>' , '<' , '$' , '%' , '&' , '/'
        , '(' , ')' , '=' , '?' , '+' , '*' , '#' , '*' ] then
          WRITE ( F , CH )
        else
          WRITE ( F , '.' )
      end (* DUMPCHAR *) ;


   begin (* SHOW_STRING_BUFFER *)
     WRITELN ( F ) ;
     WRITELN ( F , 'Constant Buffer' ) ;
     WRITELN ( F ) ;
     if STRING_POS <= 1 then
       begin
         WRITE ( F , '*** empty ***' ) ;
         return
       end (* then *) ;
     START := 1 ;
     while START < STRING_POS do
       begin
         ENDE := START + 15 ;
         WRITE ( F , START : - 6 , ': ' ) ;
         for I := START to ENDE do
           begin
             if I < STRING_POS then
               begin
                 CH := STRING_BUFFER [ I ] ;
                 C1 := HEX [ ORD ( CH ) DIV 16 ] ;
                 C2 := HEX [ ORD ( CH ) MOD 16 ] ;
                 WRITE ( F , C1 , C2 ) ;
               end (* then *)
             else
               WRITE ( F , '..' ) ;
             if I MOD 4 = 0 then
               WRITE ( F , ' ' ) ;
           end (* for *) ;
         WRITE ( F , '  *' ) ;
         for I := START to ENDE do
           begin
             if I < STRING_POS then
               DUMPCHAR ( F , STRING_BUFFER [ I ] )
             else
               WRITE ( F , ' ' ) ;
           end (* for *) ;
         WRITE ( F , '*' ) ;
         WRITELN ( F ) ;
         START := START + 16 ;
       end (* while *)
   end (* SHOW_STRING_BUFFER *) ;



begin (* HAUPTPROGRAMM *)
  MEMSET ( ADDR ( OPTAB_NAME ) , ' ' , SIZEOF ( OPTAB_NAME ) ) ;
  for I := 1 to PCODE_COUNT do
    OPTAB_NAME [ OPTAB [ I ] . OPC_NUM ] := OPTAB [ I ] . OPC ;
  RESET ( PCODE ) ;
  REWRITE ( OBJCODE ) ;
  REWRITE ( LIST003 ) ;
  REWRITE ( TRACEF ) ;
  HEAD2 ;
  ERRORCNT := 0 ;
  TIMER := CLOCK ( 0 ) ;
  WRITELN ( OUTPUT , '****' : 7 ,
            ' STANFORD PASCAL POST-PROCESSOR, OPPOLZER VERSION OF ' ,
            VERSION ) ;
  WRITELN ( OUTPUT ) ;

  //******************************************************************
  // check Op Table and Fun Table                                     
  //******************************************************************

  for I := 2 to PCODE_COUNT do
    if OPTAB [ I ] . OPC <= OPTAB [ I - 1 ] . OPC then
      begin
        WRITELN ( '+++ error in PCODE-Table (optab) at pos ' , I : 1 )
                  ;
        WRITELN ( '+++ PCODE sequence: ' , OPTAB [ I - 1 ] . OPC , ' '
                  , OPTAB [ I ] . OPC ) ;
        EXIT ( 20 ) ;
      end (* then *) ;
  for I := 2 to FUN_COUNT do
    if FUNTAB [ I ] . FUNC <= FUNTAB [ I - 1 ] . FUNC then
      begin
        WRITELN ( '+++ error in FUNC-Table (funtab) at pos ' , I : 1 )
                  ;
        WRITELN ( '+++ FUNC sequence: ' , FUNTAB [ I - 1 ] . FUNC , ' '
                  , FUNTAB [ I ] . FUNC ) ;
        EXIT ( 20 ) ;
      end (* then *) ;

  //******************************************************************
  // read pcode (and implement %INCLUDE)                              
  //******************************************************************

  RESET ( PCODE ) ;
  PCODE_FILENO := 0 ;
  PCODEP := ADDR ( PCODE ) ;
  repeat
    CHK_INCLUDE ;
    EOF_PCODE := READNXTINST ( PCODEP -> , ZEILE ) ;
    if EOF_PCODE then
      begin
        if PCODE_FILENO > 0 then
          begin
            PCODE_FILENO := 0 ;
            PCODEP := ADDR ( PCODE ) ;
          end (* then *)
        else
          PCODEP := NIL ;
        continue
      end (* then *) ;

  //******************************************************************
  // extract parts from pcode line                                    
  //******************************************************************

    if LEFT ( ZEILE , 1 ) <> ' ' then
      begin
        X := INDEX ( ZEILE , ' ' ) ;
        LABX := LEFT ( ZEILE , X - 1 ) ;
        REST := SUBSTR ( ZEILE , X + 1 ) ;
      end (* then *)
    else
      begin
        LABX := ' ' ;
        REST := LTRIM ( ZEILE )
      end (* else *) ;
    REST := TRIM ( REST ) ;
    if LENGTH ( REST ) > 3 then
      begin
        OPCODE := LEFT ( REST , 3 ) ;
        OPERANDS := SUBSTR ( REST , 4 ) ;
        X := POS_COMMENT ( OPERANDS ) ;
        if X <> 0 then
          begin
            COMMENT := SUBSTR ( OPERANDS , X ) ;
            OPERANDS := LEFT ( OPERANDS , X - 1 ) ;
          end (* then *) ;
        OPERANDS := TRIM ( OPERANDS ) ;
      end (* then *)
    else
      begin
        OPCODE := REST ;
        OPERANDS := '' ;
        COMMENT := '' ;
      end (* else *) ;
    CHECK_OPCODE ( OPCODE , OPC_NUMBER , OPC_TYPE ) ;
    if OPC_NUMBER < 0 then
      begin
        ERROR_SYMB ( 101 , OPCODE ) ;
        continue
      end (* then *) ;

  //******************************************************************
  // some pcodes are continued on subsequent lines                    
  //******************************************************************

    CONTINUE1 := ( OPC_TYPE = 'E' ) and ( RIGHT ( OPERANDS , 1 ) = ','
                 ) ;
    while CONTINUE1 do
      begin
        EOF_PCODE := READNXTINST ( PCODEP -> , ZEILE ) ;
        if EOF_PCODE then
          begin
            ERROR ( 304 ) ;
            PCODEP := NIL ;
            break
          end (* then *) ;
        OP2 := TRIM ( ZEILE ) ;
        L1 := LENGTH ( OPERANDS ) ;
        OPERANDS := SUBSTR ( OPERANDS , 1 , L1 - 2 ) || SUBSTR ( OP2 ,
                    2 ) ;
        CONTINUE1 := RIGHT ( OPERANDS , 1 ) = ',' ;
      end (* while *) ;
    if PCODEP = NIL then
      break ;

  //******************************************************************
  // delete old sc_code list, if pcode = ENT                          
  //******************************************************************

    if OPC_TYPE = '4' then
      if ENT_AKTUELL >= 0 then
        begin
          PLOC := SC_CODEA ;
          while PLOC -> . OP <> XXX_LOC do
            PLOC := PLOC -> . PREV ;
          PLOC -> . PREV -> . NEXT := NIL ;
          FREE_ALL_SC_CODES ( SC_CODE0 ) ;
          SC_CODE0 := PLOC ;
          WRITELN ( LIST003 ) ;
          WRITELN ( LIST003 ) ;
          WRITELN ( LIST003 ) ;
          while PLOC <> NIL do
            begin
              LIST_PCODE ( PLOC -> , OPTAB_NAME [ PLOC -> . OP ] , ' '
                           , '' ) ;
              PLOC := PLOC -> . NEXT ;
            end (* while *)
        end (* then *) ;

  //******************************************************************
  // fill values into sc_code element                                 
  //******************************************************************

    if SC_CODE0 = NIL then
      begin
        SC_CODE0 := ALLOC_SC_CODE ;
        SC_CODEA_ALT := NIL ;
        SC_CODEA := SC_CODE0 ;
      end (* then *)
    else
      begin
        SC_CODEA -> . NEXT := ALLOC_SC_CODE ;
        SC_CODEA_ALT := SC_CODEA ;
        SC_CODEA := SC_CODEA -> . NEXT ;
      end (* else *) ;
    SC_CODEA -> := SC_CODE_INIT ;
    SC_CODEA -> . PREV := SC_CODEA_ALT ;
    ACTNUM := ACTNUM + 1 ;
    with SC_CODEA -> do
      begin
        NUM := ACTNUM ;
        STATUS := 0 ;
        OP := OPC_NUMBER ;
        LAB := LABX ;
      end (* with *) ;
    WORK_OPERANDS ( SC_CODEA -> , OPC_TYPE , OPERANDS ) ;

  //******************************************************************
  // show sc_code element on listing file                             
  //******************************************************************

    LIST_PCODE ( SC_CODEA -> , OPCODE , OPC_TYPE , OPERANDS ) ;
  until PCODEP = NIL ;
  SHOW_STRING_BUFFER ( LIST003 ) ;

  //******************************************************************
  // check timer                                                      
  //******************************************************************

  TIMER := CLOCK ( 0 ) - TIMER ;
  WRITE ( OUTPUT , '****' : 7 ) ;
  if ERRORCNT > 0 then
    WRITE ( OUTPUT , ' ' , ERRORCNT : 1 )
  else
    WRITE ( OUTPUT , ' NO' ) ;
  WRITELN ( OUTPUT , ' ASSEMBLY ERROR(S) DETECTED.' ) ;
  WRITELN ( OUTPUT , '****' : 7 , ' ' , TIMER * 0.001 : 1 : 2 ,
            ' SECONDS IN POST-PROCESSING.' ) ;
  EXIT ( ERRORCNT ) ;
end (* HAUPTPROGRAMM *) .
