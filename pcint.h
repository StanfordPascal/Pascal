/**********************************************************************/
/*                                                                    */
/*  P-Code Interpreter                                                */
/*                                                                    */
/*  here: definitions only                                            */
/*                                                                    */
/*  Oppolzer / from 2012 until today                                  */
/*                                                                    */
/**********************************************************************/

#define PCINT_VERSION  "1.0"

#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "oiscach.h"




/**********************************************************/
/*                                                        */
/*   lokale Makros                                        */
/*                                                        */
/**********************************************************/

/************************************************/
/*   SET: vorformatieren mit Hex-Nullen         */
/*   uebertragen in Laenge von y, maximal       */
/*   in Laenge von x minus eins                 */
/************************************************/

#define SET(x,y)   memset (x, 0x00, sizeof (x)),                  \
                   memcpy (x, y, (strlen (y) < sizeof (x) - 1 ?   \
                                  strlen (y) : sizeof (x) - 1))

/************************************************/
/*   SETB: vorformatieren mit Hex-Nullen        */
/*   uebertragen maximal bis zum ersten Blank   */
/*   in y, sonst Laenge von x minus eins        */
/************************************************/

#define SETB(x,y)  memset (x, 0x00, sizeof (x)),                  \
                   memcpy (x, y, (strcspn (y, " ") <              \
                                  sizeof (x) - 1 ?                \
                                  strcspn (y, " ") :              \
                                  sizeof (x) - 1))

/************************************************/
/*   SET: vorformatieren mit Blanks             */
/*   letztes Zeichen (Laenge l) = Hex Null      */
/*   uebertragen entsprechend strlen von y      */
/************************************************/

#define SETL(x,y,l)   memset (x, ' ', sizeof (x)),                   \
                      memcpy (x, y, (strlen (y) < sizeof (x) - 1 ?   \
                                     strlen (y) : sizeof (x) - 1)),  \
                      x [l] = 0x00

#define NHIGH(x,y)     x += (y - 1), x -= (x) % y

#define ADDRSTACK(x)   (void *) (gs -> stack0 + (x))

#define STACK_C(x)     *((char *)(ADDRSTACK (x)))
#define STACK_I(x)     *((int *)(ADDRSTACK (x)))
#define STACK_P(x)     *((void **)(ADDRSTACK (x)))
#define STACK_R(x)     *((double *)(ADDRSTACK (x)))

#define STACKTYPE(x)   (gs -> stacktype [(x)])

#define SET_ON_STACK(x,y)   ((x) + ((unsigned int)(y) << 24))
#define SET_ADDR(x)         ((x) & 0x00ffffff)
#define SET_LEN(x)          ((unsigned int)(x) >> 24)

#define ADDRSTOR(x)    (void *) (gs -> store0 + (x))

#define STOR_C(x)      *((char *)(ADDRSTOR (x)))
#define STOR_I(x)      *((int *)(ADDRSTOR (x)))
#define STOR_H(x)      *((short *)(ADDRSTOR (x)))
#define STOR_P(x)      *((void **)(ADDRSTOR (x)))
#define STOR_R(x)      *((double *)(ADDRSTOR (x)))

#define STOR_FCB(x,y)                                 \
                                                      \
{                                                     \
   int adrfcb = STOR_I ((x));                         \
   (y) = (filecb *) (gs -> store0 + adrfcb);          \
   if (memcmp ((y) -> eyecatch, "PFCB", 4) != 0)      \
      runtime_error (gs, BADFILE, NULL);              \
   if (STOR_I ((y) -> pstore) != adrfcb)              \
      runtime_error (gs, BADFILE, NULL);              \
}



/**********************************************************/
/*                                                        */
/*   define Konstanten                                    */
/*   und Fehlercodes                                      */
/*                                                        */
/*   INIT_PATTERN = Init Pattern fuer undef. Speicher     */
/*   IDSIZE       = max Laenge Bezeichner                 */
/*   CIXMAX       = max. Distanz Case-Labels              */
/*                                                        */
/**********************************************************/

#define INIT_PATTERN   0x81
#define IDSIZE         20
#define CIXMAX         400
#define SETLENMAX      256
#define CMDLINEMAX     1024

#define BADFILE        1
#define BADIO          2
#define FILENOTFOUND   3
#define RANGEERR       4
#define STACKNEG       5
#define STACKCOLL      6
#define BADBRANCH      7
#define BADLBRANCH     8
#define SETLERROR      9
#define TOOMUCHFILES  10
#define NOMOREHEAP    11
#define BADBOOL       12

static const char *runtime_errmsg [] =

{
   "BADFILE",
   "BADIO",
   "FILENOTFOUND",
   "RANGEERR",
   "STACKNEG",
   "STACKCOLL",
   "BADBRANCH",
   "BADLBRANCH",
   "SETLERROR",
   "TOOMUCHFILES",
   "NOMOREHEAP",
   "BADBOOL",
   NULL
};



/**********************************************************/
/*                                                        */
/*   lokale typedefs                                      */
/*                                                        */
/**********************************************************/

typedef void *cspfunc (void *gs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4);

typedef void *cspfunc2 (void *gs,
                        int parm1,
                        double parmd,
                        int parm3,
                        int parm4);

typedef void *cspfunc3 (void *gs,
                        double *parmdp);


typedef struct
{
   char *opcode;
   int opnum;
   int status;
   char optype;
}
opctab;


typedef struct
{
   char *fucode;
   int cspnum;
   cspfunc *func;
   int parmcnt;
   int numpops;
   int status;
   char optype;
}
funtab;


typedef struct
{                 /*******************************/
   char op;       /* operation code              */
   char t;        /* type field                  */
   int  p;        /* lexical level               */
   int  q;        /* address                     */
   int  x;        /* bei CUP: Addr neues Display */
                  /* bei ENT: Speicherbedarf     */
                  /* auch bei anderen Befehlen   */
   char *plabel;  /* Label                       */
   char *poper;   /* weitere Operanden           */
   void *psect;   /* Verweis auf Section         */
   char *psource; /* Zeiger auf Sourcecode       */
   int  loc;      /* LOC = line of code          */
}                 /*******************************/
sc_code;


typedef struct cst_sect
{
   struct cst_sect *next;
   char name_short [9];
   char name_long [IDSIZE + 1];
   int lfdnr;
   int flag1;
   int flag2;
   int flag3;
   int cst_alloc;
   int cst_used;
   char *cst0;
   int cst_start;
}
cst_section;


typedef struct ent_sect
{
   struct ent_sect *next;
   char name_short [9];
   char name_long [IDSIZE + 1];
   int pcodenr;
   char size_label [9];
   int size;
   int level;
   int flag1;
   int flag2;
   int flag3;
   int flag4;
   int numb1;
   int numb2;
   char sourcename [16];
}
ent_section;


typedef struct cup_sect
{
   char eyecatch [4];
   int backchain;
   int back_static;
   short level_caller;
   short level_called;
   int olddisp;
   int newdisp;
   int returnaddr;
   int calladdr;
}
cup_section;


typedef struct
{                                 /**********************************/
   FILE *inpfile;                 /* Eingabedatei fuer PRR          */
   char inpzeile [256];           /* Eingabebuffer fuer PRR         */
   FILE *outfile;                 /* Ausgabedatei fuer Listing      */
   opctab *ot;                    /* Tabelle Opcodes etc.           */
   funtab *ft;                    /* Tabelle Funktionen             */
   char sourcename [16];          /* Sourcename                     */
   void *sourcecache;             /* Cache fuer Sourcecode          */
   int loc_aktuell;               /* aktueller LOC Wert             */
   int effadr;                    /* effektive Adr. bei LOD usw.    */
   char sch_debug;                /* Debug-Dialog, ja oder nein     */
   char sch_listing;              /* Listing, ja oder nein          */
   int firstalloc;                /* Pos. in STORE, wo ALLOC beg.   */
   int nextalloc;                 /* naechste ALLOC Position        */
                                  /*                                */
   int code_alloc;                /*                                */
   int code_used;                 /*                                */
   sc_code *code0;                /* Speicher fuer Code             */
                                  /*                                */
   int stack_alloc;               /*                                */
   int stack_used;                /*                                */
   char *stack0;                  /* STACK                          */
   char *stacktype;               /* STACK-Typenkennung             */
                                  /*                                */
   int store_alloc;               /*                                */
   int store_used;                /*                                */
   char *store0;                  /* STORE                          */
                                  /*                                */
                                  /*--------------------------------*/
                                  /* Der STORE teilt sich so auf:   */
                                  /* a) Auto Variablen der Procs    */
                                  /* b) Old Style Heap (mark/rel)   */
                                  /* c) Limit definiert durch       */
                                  /*    Run-Time parm (z.B. 2 MB)   */
                                  /* d) RCONST Area                 */
                                  /* e) STRING Const Area           */
                                  /* f) Const Area der Procs (CST)  */
                                  /* g) Platz fuer 100 FCBs         */
                                  /* h) Anfang New Style Heap       */
                                  /*--------------------------------*/
   int maxstor;                   /* max used stor (current frame)  */
   int start_const;               /* Pos. in STORE, wo Konst. beg.  */
   int maxfiles;                  /* maximale Anzahl Files          */
   int actfiles;                  /* aktuelle Anzahl Files          */
   int firstfilepos;              /* erste FCB Position             */
   int actfilepos;                /* aktuelle FCB Position          */
   int sizenewheap;               /* Groesse des Heaps (new Style)  */
   int firstnewheap;              /* Beginn new Heap                */
   int actnewheap;                /* Aktueller new Heap             */
                                  /*                                */
   int rconst_alloc;              /*                                */
   int rconst_used;               /*                                */
   int rconst_start;              /*                                */
   char *rconst0;                 /* temporaer: Real-Konstanten     */
                                  /* werden nach ASM wieder freig.  */
                                  /*                                */
   int string_alloc;              /*                                */
   int string_used;               /*                                */
   int string_start;              /*                                */
   char *string0;                 /* temporaer: String-Konstanten   */
                                  /* werden nach ASM wieder freig.  */
                                  /*                                */
   cst_section *pcst_first;       /*                                */
   cst_section *pcst_last;        /* CST-Elemente                   */
   ent_section *pent_first;       /*                                */
   ent_section *pent_last;        /* ENT-Elemente                   */
   char progheader [100];         /* Programm-Header aus BGN        */
   int startpos;                  /* Startadresse in CODE           */
   int lineofcode;                /* lineofcode = LOC value         */
   char date_string [25];         /* Datum und (Start)Uhrzeit       */
   int cmdline;                   /* Position of CMDLINE in CODE    */
   int maxsize_cmdline;           /* Size of CMDLINE-Buffer         */
   int actsize_cmdline;           /* actual size of CMDLINE         */
                                  /*--------------------------------*/
   int ip;                        /* Instruktion-Pointer            */
   int sp;                        /* Stack-Pointer                  */
   int hp;                        /* Heap-Pointer                   */
   int display [256];             /* Display fuer 256 Levels        */
   int level;                     /* Display fuer 256 Levels        */
   int pcups;                     /* Pointer auf CUP-Savearea       */
   int stepanz;                   /* Stepanzahl Debugger            */
                                  /*--------------------------------*/
                                  /* nur tempor. waehrend assembly: */
   int pcode_ent_akt;             /* pcode-Nummer aktueller Entry   */
   int local_error;               /* lokaler Fehler bei assembler   */
}                                 /**********************************/
global_store;


typedef struct
{
   char sourcename [16];
   int loc;
}
source_key;


typedef struct
{
   char zeile [132];
}
source_data;


typedef struct
{
   char eyecatch [4];         // offset 0
   int  pstore;               // offset 4 - pointer to store
   char ddname [9];           // offset 8
   char filename [257];       // offset 17
   char textfile;             // offset 274
   char inout;                // offset 275
   char eof;                  // offset 276
   char eoln;                 // offset 277
   char status;               // offset 278
   char terminal;             // offset 279 - Y = terminal-I/O
   FILE *fhandle;             // offset 280 - outside of store
   char *fbuffer;             // offset 284 - outside of store
   int  fbuflen;              // offset 288
   int  freclen;              // offset 292
   int  pfilvar;              // offset 296 - pointer to store
   char readbuf_sched;        // offset 300
   char unused2;              // offset 301 - not used yet
   char unused3;              // offset 302 - not used yet
   char unused4;              // offset 303 - not used yet
   char *fbufptr;             // offset 304 - outside of store
}
filecb;

static const filecb nullfcb_text =
{ "PFCB", 0, "", "", 1, 'U', 0, 0, '0', ' ', NULL };

static const filecb nullfcb_bin =
{ "PFCB", 0, "", "", 0, 'U', 0, 0, '0', ' ', NULL };





/**********************************************************/
/*                                                        */
/*   OP-Table / Stand 12.2017                             */
/*                                                        */
/**********************************************************/

#define XXX_ABI     0
#define XXX_ABR     1
#define XXX_ADA     2
#define XXX_ADI     3
#define XXX_ADR     4
#define XXX_AND     5
#define XXX_ASE     6
#define XXX_BGN     7
#define XXX_CHK     8
#define XXX_CHR     9
#define XXX_CRD    10
#define XXX_CSP    11
#define XXX_CST    12
#define XXX_CTI    13
#define XXX_CTS    14
#define XXX_CUP    15
#define XXX_DBG    16
#define XXX_DEC    17
#define XXX_DEF    18
#define XXX_DFC    19
#define XXX_DIF    20
#define XXX_DVI    21
#define XXX_DVR    22
#define XXX_END    23
#define XXX_ENT    24
#define XXX_EQU    25
#define XXX_FJP    26
#define XXX_FLO    27
#define XXX_FLR    28
#define XXX_FLT    29
#define XXX_GEQ    30
#define XXX_GRT    31
#define XXX_INC    32
#define XXX_IND    33
#define XXX_INN    34
#define XXX_INT    35
#define XXX_IOR    36
#define XXX_IXA    37
#define XXX_LAB    38
#define XXX_LCA    39
#define XXX_LDA    40
#define XXX_LDC    41
#define XXX_LEQ    42
#define XXX_LES    43
#define XXX_LOC    44
#define XXX_LOD    45
#define XXX_MCP    46
#define XXX_MFI    47
#define XXX_MOD    48
#define XXX_MOV    49
#define XXX_MPI    50
#define XXX_MPR    51
#define XXX_MSE    52
#define XXX_MST    53
#define XXX_NEQ    54
#define XXX_NEW    55
#define XXX_NGI    56
#define XXX_NGR    57
#define XXX_NOT    58
#define XXX_ODD    59
#define XXX_ORD    60
#define XXX_PAK    61
#define XXX_POP    62
#define XXX_RET    63
#define XXX_RND    64
#define XXX_RST    65
#define XXX_SAV    66
#define XXX_SBA    67
#define XXX_SBI    68
#define XXX_SBR    69
#define XXX_SCL    70
#define XXX_SLD    71
#define XXX_SMV    72
#define XXX_SQI    73
#define XXX_SQR    74
#define XXX_STO    75
#define XXX_STP    76
#define XXX_STR    77
#define XXX_TRC    78
#define XXX_UJP    79
#define XXX_UNI    80
#define XXX_UXJ    81
#define XXX_XJP    82
#define XXX_XLB    83
#define XXX_XOR    84
#define XXX_XPO    85




/**********************************************************/
/*   Verzeichnis der OpTypen                              */
/**********************************************************/
/*   A = nur numerische Adresse (z.B. LOC, IXA)           */
/*   B = Level und Adresse, z.B. LDA                      */
/*   C = Konstante, wie bei LDC                           */
/*   D = Typ, Adresse (wie bei DEC und INC z.B.)          */
/*   E = fuer LCA (Adressen von Strings usw.)             */
/*   J = Sprungziel sichern (Operand bei FJP und UJP)     */
/*   K = fuer DEF (Typ und Konstante)                     */
/*   L = Label (Offset uebernehmen)                       */
/*   R = RET                                              */
/*   S = Typ, Level, Adresse (wie bei STR z.B.)           */
/*   T = nur Typbuchstabe                                 */
/*   U = CUP (call user procedure)                        */
/*   V = Vergleich, also Typ und bei M noch Anzahl        */
/*   X = Sprungziel sichern bei XJP (Case)                */
/*   Y = Call Standard Function                           */
/*   0 = hat keine Operanden                              */
/*   1 = CST (sozusagen statische CSECT)                  */
/*   2 = DFC (Definition in statischer CSECT)             */
/*   3 = BGN (Programmheader und Startposition)           */
/*   4 = ENT (Entry Point)                                */
/**********************************************************/


static opctab ot [] =

{
   { "ABI", XXX_ABI, 0, '0' },
   { "ABR", XXX_ABR, 0, '0' },
   { "ADA", XXX_ADA, 0, '0' },    /* neu Opp 2016 */
   { "ADI", XXX_ADI, 0, '0' },
   { "ADR", XXX_ADR, 0, '0' },
   { "AND", XXX_AND, 0, 'W' },    /* Typ Kennz neu / Opp 2016 */
   { "ASE", XXX_ASE, 0, 'A' },    /* neu McGill: Add to Set */
   { "BGN", XXX_BGN, 0, '3' },
   { "CHK", XXX_CHK, 0, 'S' },
   { "CHR", XXX_CHR, 0, '0' },
   { "CRD", XXX_CRD, 0, '0' },    /* nicht in Stanford-Papier */
   { "CSP", XXX_CSP, 0, 'Y' },
   { "CST", XXX_CST, 0, '1' },    /* neu McGill: STATIC CSECT */
   { "CTI", XXX_CTI, 0, '0' },    /* nicht in Stanford-Papier */
   { "CTS", XXX_CTS, 0, '0' },
   { "CUP", XXX_CUP, 0, 'U' },
   { "DBG", XXX_DBG, 0, 'A' },    /* neu 2017: Debug Instrukt. */
   { "DEC", XXX_DEC, 0, 'D' },
   { "DEF", XXX_DEF, 0, 'K' },
   { "DFC", XXX_DFC, 0, '2' },    /* neu McGill: Def Constant */
   { "DIF", XXX_DIF, 0, '0' },
   { "DVI", XXX_DVI, 0, '0' },
   { "DVR", XXX_DVR, 0, '0' },
   { "END", XXX_END, 0, '0' },    /* nicht in Stanford-Papier */
   { "ENT", XXX_ENT, 0, '4' },
   { "EQU", XXX_EQU, 0, 'V' },
   { "FJP", XXX_FJP, 0, 'J' },
   { "FLO", XXX_FLO, 0, '0' },
   { "FLR", XXX_FLR, 0, '0' },
   { "FLT", XXX_FLT, 0, '0' },
   { "GEQ", XXX_GEQ, 0, 'V' },
   { "GRT", XXX_GRT, 0, 'V' },
   { "INC", XXX_INC, 0, 'D' },
   { "IND", XXX_IND, 0, 'D' },
   { "INN", XXX_INN, 0, '0' },
   { "INT", XXX_INT, 0, '0' },
   { "IOR", XXX_IOR, 0, 'W' },    /* Typ Kennz neu / Opp 2016 */
   { "IXA", XXX_IXA, 0, 'A' },
   { "LAB", XXX_LAB, 0, 'L' },
   { "LCA", XXX_LCA, 0, 'E' },
   { "LDA", XXX_LDA, 0, 'B' },
   { "LDC", XXX_LDC, 0, 'C' },
   { "LEQ", XXX_LEQ, 0, 'V' },
   { "LES", XXX_LES, 0, 'V' },
   { "LOC", XXX_LOC, 0, 'M' },
   { "LOD", XXX_LOD, 0, 'S' },
   { "MCP", XXX_MCP, 0, '0' },    /* neu 2017: Memcpy Instrukt.*/
   { "MFI", XXX_MFI, 0, 'A' },    /* neu 2017: Mem Fill fest.L.*/
   { "MOD", XXX_MOD, 0, '0' },
   { "MOV", XXX_MOV, 0, 'A' },
   { "MPI", XXX_MPI, 0, '0' },
   { "MPR", XXX_MPR, 0, '0' },
   { "MSE", XXX_MSE, 0, '0' },    /* neu 2017: Memset Instrukt.*/
   { "MST", XXX_MST, 0, 'B' },
   { "NEQ", XXX_NEQ, 0, 'V' },
   { "NEW", XXX_NEW, 0, 'B' },
   { "NGI", XXX_NGI, 0, '0' },
   { "NGR", XXX_NGR, 0, '0' },
   { "NOT", XXX_NOT, 0, 'W' },    /* Typ Kennz neu / Opp 2016 */
   { "ODD", XXX_ODD, 0, '0' },
   { "ORD", XXX_ORD, 0, '0' },
   { "PAK", XXX_PAK, 0, '0' },    /* nicht in Stanford-Papier */
   { "POP", XXX_POP, 0, '0' },    /* nicht in Stanford-Papier */
   { "RET", XXX_RET, 0, 'R' },
   { "RND", XXX_RND, 0, '0' },    /* gibt's nicht mehr, ist CSP */
   { "RST", XXX_RST, 0, '0' },
   { "SAV", XXX_SAV, 0, '0' },
   { "SBA", XXX_SBA, 0, '0' },    /* neu Opp 2016 */
   { "SBI", XXX_SBI, 0, '0' },
   { "SBR", XXX_SBR, 0, '0' },
   { "SCL", XXX_SCL, 0, 'B' },    /* neu McGill: Set Clear */
   { "SLD", XXX_SLD, 0, 'B' },    /* neu McGill: Set Load */
   { "SMV", XXX_SMV, 0, 'B' },    /* neu McGill: Set Move */
   { "SQI", XXX_SQI, 0, '0' },
   { "SQR", XXX_SQR, 0, '0' },
   { "STO", XXX_STO, 0, 'T' },
   { "STP", XXX_STP, 0, '0' },
   { "STR", XXX_STR, 0, 'S' },
   { "TRC", XXX_TRC, 0, '0' },    /* gibt's nicht mehr, ist CSP */
   { "UJP", XXX_UJP, 0, 'J' },
   { "UNI", XXX_UNI, 0, '0' },
   { "UXJ", XXX_UXJ, 0, 'J' },    /* neu McGill: Long Jump */
   { "XJP", XXX_XJP, 0, 'X' },
   { "XLB", XXX_XLB, 0, 'L' },    /* neu McGill: Long Jump Target */
   { "XOR", XXX_XOR, 0, 'W' },    /* neu Opp 2017 */
   { "XPO", XXX_XPO, 0, '0' },    /* nicht in Stanford-Papier */
   { NULL,  -1, 0, ' ' }
};






/**********************************************************/
/*                                                        */
/*   Prototypen PCINTCMP                                  */
/*                                                        */
/**********************************************************/

void translate (global_store *gs, FILE *f, char *fname);

void translate2 (global_store *gs);

void dump_stack (FILE *outfile,
                 char *header,
                 char *origin,
                 int start,
                 int len,
                 int align);

void dump_store (FILE *outfile,
                 char *header,
                 char *origin,
                 int start,
                 int len,
                 int align);

void read_pascal (global_store *gs, char *pasfilename);

void listing (global_store *gs);



