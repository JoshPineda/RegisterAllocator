%{
#include <stdio.h>
#include "attr.h"
#include "instrutil.h"
#include <string.h>
int yylex();
void yyerror(char * s);
#include "symtab.h"

FILE *outfile;
char *CommentBuffer;
 
%}

%union {tokentype token;
        regInfo targetReg;
       }

%token PROG PERIOD VAR 
%token INT BOOL PRINT THEN IF DO  
%token ARRAY OF 
%token BEG END ASG  
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token ELSE
%token WHILE 
%token <token> ID ICONST 

%type <targetReg> exp 
%type <targetReg> lhs 
%type <targetReg> idlist
%type <targetReg> stype
%type <targetReg> type
%type <targetReg> condexp
%type <targetReg> stmt wstmt ifstmt astmt writestmt cmpdstmt ifhead



%start program

%nonassoc EQ NEQ LT LEQ GT GEQ 
%left '+' '-' AND
%left '*' OR

%nonassoc THEN
%nonassoc ELSE

%{
    int idCtr = 0;
    int lastArr = 0;
%}

%%
program : {emitComment("Assign STATIC_AREA_ADDRESS to register \"r0\"");
           emit(NOLABEL, LOADI, STATIC_AREA_ADDRESS, 0, EMPTY);} 
           PROG ID ';' block PERIOD { }
	;

block	: variables cmpdstmt { }
	;
    
variables: /* empty */
	| VAR vardcls { }
	;
    


vardcls	: vardcls vardcl ';' { }
	| vardcl ';' { }
	| error ';' { yyerror("***Error: illegal variable declaration\n");} 
	;


vardcl	: idlist ':' type   {  

                /*The type of the idlist is type*/
                $1.type = $3.type;                

                /*Length of the array */
                int length = (int)((sizeof($1.valLst) / sizeof($1.valLst[0])));


                /*Make type of each entry in the table
                  the type that was derived*/
                int i;


                for (i = idCtr; i < length; i++)
                {
                    if ($1.valLst[i] == NULL)
                    {
                        idCtr = i;
                        break;
                    }
                    SymTabEntry* ste = lookup($1.valLst[i]);

                    /*If there is no entry anymore, then we can  quit the loop*/
                    if (ste == NULL)
                    {
                        /*Make sure we know where
                          we left off so that
                          we can still have more var 
                          declarations */

                        /*We know to stop at i
                          because if it is null
                          then this is the first
                          empty space in the 
                          valLst */


                        idCtr = i;
                        break;
                    }

                    else
                    {
                        /*Otherwise, this is a non-empty
                          space in the array and we can
                          change the type to the relevant type */
                        ste -> type = $3.type;
                        
                        /*If we a 1-dim array*/
                        if ($3.arraysize != 0)
                        {
                            
                            
                            ste -> sora = 1;
                            ste -> arraysize = $3.arraysize;
                            
                            ste->array = (int*)malloc(sizeof(int) * $3.arraysize);
  
                            
                            /* Look at current offset */
                            int curroffset = ste->offset;
                            
                            
                            /*Calculate what offset should be if 
                              the last thing that was in here was an array*/
                            if (lastArr != -1)
                            {
                                if (lastArr == 1)
                                {
                                    ste -> offset = curroffset + 4;
                                }
                                
                                
                                ste -> offset = curroffset + ( (lastArr - 1         ) * 4 );

                            }
                            
                            /*Set the lastArr to current arraysize*/
                            lastArr = $3.arraysize;
                            

                        }
                        /*If we are a scalar*/
                        else
                        {
                            ste -> sora = 0;
                        
                            /* Look at current offset */
                            int curroffset = ste->offset;    
                            
                            /*Calculate what offset should be if 
                              the last thing that was in here was an array*/
                            if (lastArr != -1)
                            {
                                if (lastArr == 1)
                                {
                                    ste -> offset = curroffset + 4;
                                }
                                if (lastArr == 0)
                                {
                                    /* do nothing */
                                }
                                else
                                {
                                    ste -> offset = curroffset + ( (lastArr - 1         ) * 4 );
                                }
     

                            }
                            
                            /*Set the lastArr to nothing*/
                            lastArr = -1;
                            
                            
                        }
                    
                    }

                }
                






            }
	;


idlist	: idlist ',' ID { 
                        
                            /*Compute offset and the array index from
                              that offset */
                            int offset = NextOffset(1);
                            int arrIndex;
                            if (offset  == 0)
                            {
                                arrIndex = 0;
                            }
                            else
                            {
                                arrIndex = offset / 4;

                            }

                            $$.valLst[arrIndex] = strdup($3.str);


                            /*Insert the string and offset with a 
                              bogus typing (error typing) */
                            insert($3.str, $1.type,offset);
                        

                        }
        | ID		{  
        
                        /*Same procedure as above but with less things in it*/

                        /*Might want to make a method for this later*/
                        int offset = NextOffset(1);
                        int arrIndex;
                        if (offset  == 0)
                        {
                            arrIndex = 0;
                        }
                        else
                        {
                            arrIndex = offset / 4;

                        }

                        $$.valLst[arrIndex] = strdup($1.str);
                        insert($1.str, $$.type,offset);
                    
        
                    } 
	   ;

/*Returning type properly*/
type	: ARRAY '[' ICONST ']' OF stype {$$.type = $6.type;  
                                         $$.arraysize = $3.num;}

        | stype {$$.type = $1.type;  $$.arraysize = 0;}
	;

stype	: INT { $$.type = TYPE_INT;  }
        | BOOL { $$.type = TYPE_BOOL;  }
	;

stmtlist : stmtlist ';' stmt { }
	| stmt { }
        | error { yyerror("***Error: ';' expected or illegal statement \n");}
	;

stmt    : ifstmt { }
	| wstmt { }
	| astmt { }
	| writestmt { }
	| cmpdstmt { }
	;

cmpdstmt: BEG stmtlist END { }
	;

ifstmt :  ifhead {}
          THEN {
            /*put the true comment here*/
          sprintf(CommentBuffer,"This is the \"TRUE\" branch");
          emitComment(CommentBuffer);
          
          }
          
          stmt {
                sprintf(CommentBuffer, "Branch to the statement following the \"ELSE\" statement list");
                emitComment(CommentBuffer);
                /*Here we branch to the thing beyond the else stmt*/
                emit(NOLABEL,
                BR,
                $1.afterLab + 1,
                EMPTY,
                EMPTY); 
          
          
          }
  	  ELSE {

            /*Otherwise we enter the false branch label*/
            emit ($1.afterLab,
            NOP,
            EMPTY,
            EMPTY,
            EMPTY);
            
              /*Put false comment here*/
            sprintf(CommentBuffer, "This is the \"FALSE\" branch");
            emitComment(CommentBuffer);
      }
          stmt {
          /*Label to continue process after the ifelse is over*/
          emit($1.afterLab + 1,
          NOP,
          EMPTY,
          EMPTY,
          EMPTY);
          
          }
	;

ifhead : IF condexp {  

            /*Conditional root stays the same*/
            $$.condRoot = $2.condRoot;
            
            /*We do not do a +1 here because we going to the 
            part AFTER the else stmt*/
            $$.afterLab = $2.afterLab;
                    }
        ;

writestmt: PRINT '(' exp ')' { 
                            int printOffset = -4;
                            
                            /* default location for printing */
  	                         sprintf(CommentBuffer, "Code for \"PRINT\" from offset %d", printOffset);
	                         emitComment(CommentBuffer);
                             
                                 emit(NOLABEL, STOREAI, $3.targetRegister, 0, printOffset);
                                 emit(NOLABEL, 
                                      OUTPUTAI, 
                                      0,
                                      printOffset, 
                                      EMPTY);
                               }
	;

wstmt: WHILE  {
                    
                    /*Making a new label section for control part of whiledo*/
                    int newLabel = NextLabel();
                    
                    sprintf(CommentBuffer, "Control part for \"WHILE DO\" ");
                    emitComment(CommentBuffer);
                    
                    /*The label for the control part Ex. L0*/
                    emit(newLabel,
                    NOP,
                    EMPTY,
                    EMPTY,
                    EMPTY);
                    
                    
                    
                    
                    
                    } 
                    
                    
          condexp { 
                    sprintf(CommentBuffer, "Body of the \"WHILE\" construct starts here");
                    emitComment(CommentBuffer);
                    } 
                    
                    
          DO stmt  {
                    
                    /*At the end of the DO portion: branch back to the root*/
                    emit(NOLABEL,
                    BR,
                    $3.condRoot,
                    EMPTY,
                    EMPTY);
                    
                    emit($3.afterLab,
                    NOP,
                    EMPTY,
                    EMPTY,
                    EMPTY);
            
                        
                    } 
	;

astmt : lhs ASG exp             { 
 				  if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) || 
				         (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
                                         printf("*** ERROR ***: Assignment types do not match.\n");
                                         
                                         printf("**%d, %d**",$1.type,$3.type);
                                        }
                                      
                                emit(NOLABEL,
                                       STORE, 
                                       $3.targetRegister,
                                       $1.targetRegister,
                                       EMPTY);
                                }
	;

lhs	: ID			{ 

                        /*General Gist : we get next offset
                          and then we add that to r0 to get
                          the address of the current variable.
                          The new address is in a new address*/
                          
                                  int newReg1 = NextRegister();
                                  int newReg2 = NextRegister();
                                  
                                  int offset;
                                  
                                  SymTabEntry* ste = lookup($1.str);
                                  
                                  if (ste == NULL)
                                  {
                                        printf("\n*** ERROR ***: Variable %s not declared.\n",$1.str);
                                  }
                                  
                                  
                                  offset = ste -> offset;
				                  
                                  $$.targetRegister = newReg2;
                                  $$.type = ste -> type;
                                  
                                  
                                  

				  
				   
                  sprintf(CommentBuffer,"Computing address of variable \"%s\" from offset %d in register %d", $1.str,offset,$$.targetRegister);
                  emitComment(CommentBuffer);
                  
                      emit(NOLABEL, LOADI, offset, newReg1, EMPTY);
                      emit(NOLABEL, ADD, 0, newReg1, newReg2);
				  
                         	  }
|  ID '[' exp ']' {  
                
                /*Finding entry in symbol table*/
                SymTabEntry* ste = lookup($1.str); 
                if (ste == NULL)
                {
                    printf("\n*** ERROR ***: Variable %s not declared.\n", $1.str);
                }
                
                int sora = ste->sora;
                
                /*If  the entry is even an array type*/
                if (sora == 1)
                {
                
                    /*The exp must be an int*/
                    if($3.type == TYPE_INT)
                    {
     
                          /*This is for offset calculation*/
                          int newreg1 = NextRegister();
                          int newreg2 = NextRegister();
                          int newreg3 = NextRegister();
                          int newreg4 = NextRegister();
                          int newreg5 = NextRegister();

                          int offset = ste -> offset;
                          
                          sprintf(CommentBuffer, "Computing address of variable \"%s\" from offset %d in register %d",$1.str,ste->offset,newreg1);
                          emitComment(CommentBuffer);
                          
                          /*Load 4 into a new register
                            Then multiply that by the base address
                            to get the array index in terms of offset*/
                            
                          emit(NOLABEL, LOADI, 4, newreg2,EMPTY);
                          emit(NOLABEL, MULT, $3.targetRegister, newreg2,newreg3);
                            
                          
                          /*Load the base address*/
                          emit(NOLABEL, LOADI, offset,newreg4,EMPTY);
                          
                          /*Get the augmented offset + base addr*/
                          emit(NOLABEL, ADD, newreg4, newreg3,newreg5);
                          
                          /*We are adding the augmented offset to the r0 register and returning that to */
                          emit(NOLABEL, ADD, 0,newreg5,newreg1);
                          
                          $$.targetRegister = newreg1;
                          $$.type = ste -> type;
                          
                          

                    }

                    /*ERROR - Exp must be an int*/
                    else
                    {
                       printf("\n*** ERROR ***: Array index must be integer.\n");
                    }
                }
                /*ERROR, ID is not array type*/
                else
                {
                    printf("\n*** ERROR ***: Variable %s is not an array variable.\n", $1.str);
                }
                
                                
                                
            }
            ;


exp	: exp '+' exp		{ int newReg = NextRegister();

        if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
    				    printf("*** ERROR ***: Operator types must be integer.\n");
                                                                }
                                  $$.type = $1.type;

                                  $$.targetRegister = newReg;
                                  emit(NOLABEL, 
                                       ADD, 
                                       $1.targetRegister, 
                                       $3.targetRegister, 
                                       newReg);
                                       
                                  $$.value = $1.value + $3.value;
                                }/*End Addition  */

        | exp '-' exp		{ int newReg = NextRegister();
        
         if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
    				    printf("*** ERROR ***: Operator types must be integer.\n");
                                                                 }
                                                                 
                                 $$.type = $1.type;                       $$.targetRegister = newReg;     
                                  emit(NOLABEL, 
                                       SUB, 
                                       $1.targetRegister, 
                                       $3.targetRegister, 
                                       newReg);  
                                 $$.value = $1.value - $3.value;
        
                            }/*End Subtraction */

        | exp '*' exp		{  int newReg = NextRegister();
        
         if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
    				    printf("*** ERROR ***: Operator types must be integer.\n");
                                                                 }
                                                                 
                                 $$.type = $1.type;                       $$.targetRegister = newReg;     
                                  emit(NOLABEL, 
                                       MULT, 
                                       $1.targetRegister, 
                                       $3.targetRegister, 
                                       newReg);  
                                 $$.value = $1.value * $3.value;
                                       
                             }/*End Multiplication*/

        | exp AND exp		{
        
         if (! (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL))) {
                printf("*** ERROR ***: Operator types must be boolean.\n");
                                                                    }
                
                int newreg1 = NextRegister();
                  
                
                $$.type = $1.type;
                
                $$.targetRegister = newreg1;
                
                emit(NOLABEL,
                     AND_INSTR,
                     $1.targetRegister,
                     $3.targetRegister,
                     newreg1);
                     
            
                
                if ($1.value && $3.value)
                {
                    $$.value = 1;
                }
                
                else
                {
                    $$.value = 0;
                }
        
                            } 


        | exp OR exp       	{  
        
         if (! (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL))) {
                printf("*** ERROR ***: Operator types must be boolean.\n");  
                                                                    }      
                int newreg1 = NextRegister();
                    
                
                $$.type = $1.type;
                
                $$.targetRegister = newreg1;
                
                emit(NOLABEL,
                     OR_INSTR,
                     $1.targetRegister,
                     $3.targetRegister,
                     newreg1);
                
                if ($1.value || $3.value)
                {
                    $$.value = 1;
                }
                
                else
                {
                    $$.value = 0;
                }
        
        
                            }


        | ID			{ 
	                        int newReg = NextRegister();
                            SymTabEntry* ste = lookup($1.str);
                            if (ste == NULL)
                            {
                                printf("\n*** ERROR ***: Variable %s not declared.\n", $1.str);
                            }
                            int sora = ste->sora;
                            if (sora == 1)
                            {
                                printf("\n*** ERROR ***: Variable %s is not a scalar variable.\n", $1.str);

                            }
                           	int offset = ste -> offset;
				
                            sprintf(CommentBuffer,"Load RHS value of variable \"%s\" at offset %d",$1.str,offset);
                            emitComment(CommentBuffer);
					
                            $$.targetRegister = newReg;
			                $$.type = ste -> type;
                            emit(NOLABEL, LOADAI, 0, offset, newReg);
                                  
                        }

        | ID '[' exp ']'	{   
        
                /*Finding entry in symbol table*/
                SymTabEntry* ste = lookup($1.str); 
                
                if (ste == NULL)
                {
                    printf("\n*** ERROR ***: Variable %s not declared.\n", $1.str);
                }
                
                int sora = ste->sora;
                
                /*If  the entry is even an array type*/
                if (sora == 1)
                {
                
                    /*The exp must be an int*/
                    if($3.type == TYPE_INT)
                    {
                    
                        
                    
                        int offset = ste -> offset;
                        
                        /*This is for offset calculation*/
                          int newreg1 = NextRegister();
                          int newreg2 = NextRegister();
                          int newreg3 = NextRegister();
                          int newreg4 = NextRegister();
                          int newreg5 = NextRegister();
                    
                        sprintf(CommentBuffer, "Load RHS value of variable \"%s\" with based address %d",$1.str,offset);
                        emitComment(CommentBuffer);
                        
                        /*Put 4 into a register*/
                        emit(NOLABEL, LOADI,4,newreg2,EMPTY);
                        
                        /*Multiply exp value with 4 to get the offset*/
                        emit(NOLABEL, MULT,$3.targetRegister,newreg2,newreg3);
                        
                        /*Load the base addr of the array into a new register*/
                        emit(NOLABEL, LOADI,offset,newreg4,EMPTY);
                        
                        /*Add the base address of the array and the calc offset*/
                        emit(NOLABEL, ADD,newreg4,newreg3,newreg5);
                        
                        /*Now we add that address calculated above to r0*/
                        emit(NOLABEL, LOADAO,0,newreg5,newreg1);
                        
                        
                        $$.targetRegister = newreg1;
                        $$.type = ste->type;
                    }
                    else
                    {
                     printf("\n*** ERROR ***: Array index must be integer.\n");
                    }
                    
                }
                else
                {
                 printf("\n*** ERROR ***: Variable %s is not an array variable.\n", $1.str);
                }
        
        
        
        
                            }
 


	   | ICONST                 { int newReg = NextRegister();
	                           $$.targetRegister = newReg;
				   $$.type = TYPE_INT;
				   emit(NOLABEL, LOADI, $1.num, newReg, EMPTY); 
                   
                   $$.value = $1.num;
                   
                   }

        | TRUE                   { int newReg = NextRegister(); /* TRUE is encoded as value '1' */
	                           $$.targetRegister = newReg;
				   $$.type = TYPE_BOOL;
				   emit(NOLABEL, LOADI, 1, newReg, EMPTY); 
                   
                   $$.value = 1;
                   
                   }

        | FALSE                   { int newReg = NextRegister(); /* TRUE is encoded as value '0' */
	                           $$.targetRegister = newReg;
				   $$.type = TYPE_BOOL;
				   emit(NOLABEL, LOADI, 0, newReg, EMPTY);
                   
                   $$.value = 0;
                   }

	| error { yyerror("***Error: illegal expression\n");}  
	;

/* Condexp has its own section/label where the condition is evaluated. */
condexp	: exp NEQ exp		{
                 if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) || 
				         (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
                    printf("\n*** ERROR ***: != operator with different types.\n");
                    
                     }        
                     
                                                      
                     /*Get a new  label and register for the comparison*/
                     int newreg = NextRegister();
                     int newlab = NextLabel();
                     int newlab2 = NextLabel();
                     
                     
                     /*Calculate root*/
                     int root = newlab2 - 2;
                     $$.condRoot = root;
                     
                     /*Be sure to store where to go after*/
                     $$.afterLab = newlab2;
                     
                     /*Compare the exp */
                     emit(NOLABEL,
                     CMPNE,
                     $1.targetRegister,
                     $3.targetRegister,
                     newreg);
                     
                     /*Branch*/
                     emit(NOLABEL,
                     CBR,
                     newreg,
                     newlab,
                     newlab2);
                     
                     /*start the new section for the next label, the other
                       side of the conditional (else or then or outside a while*/
                     emit(newlab,
                     NOP,
                     EMPTY,
                     EMPTY,
                     EMPTY);

                            } 

        | exp EQ exp		{
        
                 if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) || 
				         (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
                    printf("\n*** ERROR ***: == operator with different types.\n");
                                                                 }
                 /*Get a new  label and register for the comparison*/
                     int newreg = NextRegister();
                     int newlab = NextLabel();
                     int newlab2 = NextLabel();
                     
                     
                     /*Calculate root*/
                     int root = newlab2 - 2;
                     $$.condRoot = root;
                     
                     /*Be sure to store where to go after*/
                     $$.afterLab = newlab2;
                     
                     /*Compare the exp */
                     emit(NOLABEL,
                     CMPEQ,
                     $1.targetRegister,
                     $3.targetRegister,
                     newreg);
                     
                     /*Branch*/
                     emit(NOLABEL,
                     CBR,
                     newreg,
                     newlab,
                     newlab2);
                     
                     /*start the new section for the next label, the other
                       side of the conditional (else or then or outside a while*/
                     emit(newlab,
                     NOP,
                     EMPTY,
                     EMPTY,
                     EMPTY);

        
        
        
                            } 

        | exp LT exp		{

                if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
                    printf("\n*** ERROR ***: Relational operator with illegal type.\n");
                                                                        }
                     /*Get a new  label and register for the comparison*/
                     int newreg = NextRegister();
                     int newlab = NextLabel();
                     int newlab2 = NextLabel();
                     
                     
                     /*Calculate root*/
                     int root = newlab2 - 2;
                     $$.condRoot = root;
                     
                     /*Be sure to store where to go after*/
                     $$.afterLab = newlab2;
                     
                     /*Compare the exp */
                     emit(NOLABEL,
                     CMPLT,
                     $1.targetRegister,
                     $3.targetRegister,
                     newreg);
                     
                     /*Branch*/
                     emit(NOLABEL,
                     CBR,
                     newreg,
                     newlab,
                     newlab2);
                     
                     /*start the new section for the next label, the other
                       side of the conditional (else or then or outside a while*/
                     emit(newlab,
                     NOP,
                     EMPTY,
                     EMPTY,
                     EMPTY);

        
        
        }

        | exp LEQ exp		{
        
        
                 if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
    				    printf("\n*** ERROR ***: Relational operator with illegal type.\n");
                                                                        }
                    /*Get a new  label and register for the comparison*/
                     int newreg = NextRegister();
                     int newlab = NextLabel();
                     int newlab2 = NextLabel();
                     
                     
                     /*Calculate root*/
                     int root = newlab2 - 2;
                     $$.condRoot = root;
                     
                     /*Be sure to store where to go after*/
                     $$.afterLab = newlab2;
                     
                     /*Compare the exp */
                     emit(NOLABEL,
                     CMPLE,
                     $1.targetRegister,
                     $3.targetRegister,
                     newreg);
                     
                     /*Branch*/
                     emit(NOLABEL,
                     CBR,
                     newreg,
                     newlab,
                     newlab2);
                     
                     /*start the new section for the next label, the other
                       side of the conditional (else or then or outside a while*/
                     emit(newlab,
                     NOP,
                     EMPTY,
                     EMPTY,
                     EMPTY);

        
        
                            }

	| exp GT exp		{
    
             if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
    				    printf("\n*** ERROR ***: Relational operator with illegal type.\n");
                                                                    }
             /*Get a new  label and register for the comparison*/
                     int newreg = NextRegister();
                     int newlab = NextLabel();
                     int newlab2 = NextLabel();
                     
                     
                     /*Calculate root*/
                     int root = newlab2 - 2;
                     $$.condRoot = root;
                     
                     /*Be sure to store where to go after*/
                     $$.afterLab = newlab2;
                     
                     /*Compare the exp */
                     emit(NOLABEL,
                     CMPGT,
                     $1.targetRegister,
                     $3.targetRegister,
                     newreg);
                     
                     /*Branch*/
                     emit(NOLABEL,
                     CBR,
                     newreg,
                     newlab,
                     newlab2);
                     
                     /*start the new section for the next label, the other
                       side of the conditional (else or then or outside a while*/
                     emit(newlab,
                     NOP,
                     EMPTY,
                     EMPTY,
                     EMPTY);

    
    
    
    
                        }

	| exp GEQ exp		{
    
             if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
    				    printf("\n*** ERROR ***: Relational operator with illegal type.\n");
                                                                    }
               /*Get a new  label and register for the comparison*/
                     int newreg = NextRegister();
                     int newlab = NextLabel();
                     int newlab2 = NextLabel();
                     
                     
                     /*Calculate root*/
                     int root = newlab2 - 2;
                     $$.condRoot = root;
                     
                     /*Be sure to store where to go after*/
                     $$.afterLab = newlab2;
                     
                     /*Compare the exp */
                     emit(NOLABEL,
                     CMPGE,
                     $1.targetRegister,
                     $3.targetRegister,
                     newreg);
                     
                     /*Branch*/
                     emit(NOLABEL,
                     CBR,
                     newreg,
                     newlab,
                     newlab2);
                     
                     /*start the new section for the next label, the other
                       side of the conditional (else or then or outside a while*/
                     emit(newlab,
                     NOP,
                     EMPTY,
                     EMPTY,
                     EMPTY);

    
    
    
    
    
                        }

	| error { yyerror("***Error: illegal conditional expression\n");}  
        ;

%%

void yyerror(char* s) {
        fprintf(stderr,"%s\n",s);
        }


int
main(int argc, char* argv[]) {

  printf("\n     CS415 Spring 2018 Compiler\n\n");

  outfile = fopen("iloc.out", "w");
  if (outfile == NULL) { 
    printf("ERROR: cannot open output file \"iloc.out\".\n");
    return -1;
  }

  CommentBuffer = (char *) malloc(650);  
  InitSymbolTable();

  printf("1\t");
  yyparse();
  printf("\n");

  PrintSymbolTable();
  
  fclose(outfile);
  
  return 1;
}




