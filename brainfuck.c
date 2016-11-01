#include <stdio.h>

int d(char c){
    return '\\' - c; // ASCII hack
}
void interpret(char *program){
    char *progPtr = program;
    char mem[10000];
    char *ptr = mem;
    while(*progPtr != '\0'){
        switch (*progPtr){
            case '>': ++ptr; break;
            case '<': --ptr; break;
            case '+': ++*ptr; break;
            case '-': --*ptr; break;
            case '.': putchar(*ptr); break;
            case ',': *ptr = getchar(); break;
            case '[':
            case ']':
                if (*progPtr == '[' && *ptr == 0 || *progPtr == ']' && *ptr != 0){
                    int nest = 1;
                    int instr = d(*progPtr);
                    while (progPtr && nest != 0){
                        progPtr += instr;
                        if (*progPtr == '[' || *progPtr == ']'){
                            nest += d(*progPtr) * instr;
                        }
                    }
                }
                break;
            default:
                break;
        }
        progPtr++;
    }
}

int main(int argc, char** argv){
    if ( argc != 2 ){
        printf( "usage: %s filename", argv[0] );
        return 1;
    }
    FILE *fp = fopen(argv[1], "rb");
    fseek(fp, 0, SEEK_END);
    long siz = ftell(fp);
    rewind(fp);
    char buffer[siz + 1];
    fread(buffer, 1, siz, fp);
    buffer[siz] = '\0';
    interpret(buffer);
    return 0;
}

