/**********************************************
        CS415  Project 2
        Spring  2015
        Student Version
**********************************************/

#ifndef ATTR_H
#define ATTR_H
#include <stddef.h>
#include <stdlib.h>



typedef union {int num; char *str;} tokentype;

typedef enum type_expression {TYPE_INT=0, TYPE_BOOL, TYPE_ERROR} Type_Expression;



typedef struct {
        Type_Expression type;
        int targetRegister;
        /*Need this to handle inherited attributes*/
        char* valLst[20];  
        /*The root of the Conditionals */
        int condRoot;
        /*The after of the conditional*/
        int afterLab;
        /*Helps to create arrays*/
        int arraysize;
        /*Whether it is SCALAR or 1-DIM ARRAY*/
        int sora;
        /*VALUE for the array indices*/
        int value;
    
        } regInfo;


#endif




  
