/* ppc/lib.c */

#include "obx.h"
#include <stdio.h>

/* Primitives that can be called from picoPascal programs */

unsigned prim_check = 0;	/* Fake checksum for set of primitives */

#define args (bp + HEAD + 1)

static void _new(value *sp) {
     value *bp = sp;
     (*(args[0].p)).x = malloc(args[1].i);
}

static void _argc(value *sp) {
     ob_res.i = saved_argc;
}

static void _argv(value *sp) {
     value *bp = sp;
     /* Buffer overflow waiting to happen */
     strcpy((char *) args[1].x, saved_argv[args[0].i]);
}

static void _print_num(value *sp) {
     value *bp = sp;
     printf("%d", args[0].i);
}

static void _print_string(value *sp) {
     value *bp = sp;
     printf("%s", args[0].x);
}

static void _print_char(value *sp) {
     value *bp = sp;
     printf("%c", args[0].i);
}

static void _newline(value *sp) {
     printf("\n");
}

static FILE *infile = NULL;

static void _open_in(value *sp) {
     value *bp = sp;
     FILE *f = fopen((char *) args[0].x, "r");
     if (f == NULL) {
	  ob_res.i = 0; return;
     }
     if (infile != NULL) fclose(infile);
     infile = f;
     ob_res.i = 1;
}

static void _close_in(value *sp) {
     if (infile == NULL) return;
     fclose(infile);
     infile = NULL;
}

static void _read_char(value *sp) {
     value *bp = sp;
     FILE *f = (infile == NULL ? stdin : infile);
     int ch = fgetc(f);
     *(args[0].x) = (ch == EOF ? 127 : ch);
}

static void _pexit(value *sp) {
     value *bp = sp;
     exit(args[0].i);
}

void dltrap(value *sp) {
     fprintf(stderr, "Oops: dltrap called!\n");
     exit(2);
}

primitive *primtab[] = {
     interp, dltrap, _new, _open_in, _close_in, _read_char,
     _print_num, _print_string, _print_char, _newline,
     _argc, _argv, _pexit, NULL
};

char *primname[] = {
     "INTERP", "DLTRAP", "_new", "_open_in", "_close_in",
     "_read_char", "_print_num", "_print_string", 
     "_print_char", "_newline", "_argc", "_argv", "_pexit"
};

